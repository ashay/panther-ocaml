(* Check if the file exists and whether it is readable and writable.  There is
 * a potential TOCTOU problem here, but we punt on it since we do not
 * anticipate this binary to have setuid or setgid permissions. *)
let check_if_file_exists (filename : string) : bool =
  try
    Unix.access filename [ R_OK; W_OK; F_OK ];
    true
  with Unix.Unix_error _ -> false

(* Check if directory with given path exists.  Potential TOCTOU problem
 * similar to `check_if_file_exists`. *)
let is_directory (path : string) : bool =
  try
    let stats = Unix.lstat path in
    stats.st_kind = Unix.S_DIR
  with Unix.Unix_error _ -> false

(* Read non-subdirectory entries of a directory.  Fails if input argument is
 * not a directory. *)
let read_dir (dirpath : string) : string list =
  let handle = Unix.opendir dirpath in

  let rec descend () : string list =
    try
      let entry = Unix.readdir handle in

      (* XXX: If making changes, remember to skip the '.' and '..' files. *)
      match is_directory entry with
      | true -> descend ()
      | false -> List.cons entry (descend ())
    with End_of_file -> []
  in

  let entries = descend () in
  Unix.closedir handle;

  entries

(* Create a blank file. *)
let create_empty_file (filename : string) : (unit, string) result =
  try
    let fd = Unix.openfile filename [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in
    Ok (Unix.close fd)
  with Unix.Unix_error (err, fn, arg) ->
    let msg = Unix.error_message err in
    Error (Printf.sprintf "%s: %s [%s]" fn msg arg)

(* Read a file in its entirety, and return contents (or error). *)
let read_file (filename : string) : (string, string) result =
  if is_directory filename then
    let message = Printf.sprintf "invalid source file '%s'" filename in
    Error message
  else
    try
      let fd = Unix.openfile filename [ O_RDONLY ] 0 in

      (* Find file size and revert file descriptor back to start. *)
      let file_size = Unix.lseek fd 0 Unix.SEEK_END
      and _ = Unix.lseek fd 0 Unix.SEEK_SET in

      (* We're about to read in the entire file.  Create a large buffer. *)
      let buffer = Bytes.create file_size in

      (* Check whether we were able to read the entire file. *)
      let ret_val =
        (* If it's an empty file, return an empty buffer. *)
        match file_size with
        | 0 -> Ok ""
        | _ -> (
            match Unix.read fd buffer 0 file_size with
            | 0 -> Error (Printf.sprintf "failed to read %d byte(s)" file_size)
            | _ -> Ok (Bytes.to_string buffer) )
      in

      Unix.close fd;
      ret_val
    with Unix.Unix_error (err, fn, arg) ->
      let msg = Unix.error_message err in
      Error (Printf.sprintf "%s: %s [%s]" fn msg arg)

(* Write file contents.  We want to be able to write both raw string and
 * hex-encoded strings. *)
let write_file (filename : string) (contents : string) : (unit, string) result =
  try
    let fd = Unix.openfile filename [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in

    (* Turn the string buffer into a byte buffer. *)
    let buffer = Bytes.of_string contents in
    let buffer_size = Bytes.length buffer in

    (* And write the file. *)
    let ret_val =
      match Unix.write fd buffer 0 buffer_size with
      | 0 -> Error (Printf.sprintf "failed to write %d bytes(s)" buffer_size)
      | _ -> Ok ()
    in

    Unix.close fd;
    ret_val
  with Unix.Unix_error (err, fn, arg) ->
    let msg = Unix.error_message err in
    Error (Printf.sprintf "%s: %s [%s]" fn msg arg)

(* Get canonical path of the file, if it exists.  Masks exceptions. *)
let canonical_path (filepath : string) : string =
  try Core.Filename.realpath filepath
  with
  (* If we couldn't find the real path, return the input path. *)
  | Unix.Unix_error _ ->
    filepath

(* Read lines from standard input. *)
let rec collect_stdin acc =
  let maybe_read_line () = try Some (read_line ()) with End_of_file -> None in

  match maybe_read_line () with
  | Some line -> collect_stdin (List.append acc [ line ])
  (* Add an implicit newline at the end of the input. *)
  | None -> String.concat "\n" acc

(* Read from file or stdin. *)
let read_source (path : string) : (string, string) result =
  match path with "-" -> Ok (collect_stdin []) | _ -> read_file path

(* Read a file, encrypt it, and save it into a specific destination. *)
let encrypt_file_and_save (key : Types.raw_string) (src : string) (dst : string)
    : (unit, string) result =
  (* Make sure the `src` and `dst` paths don't refer to the same file. *)
  if canonical_path src = canonical_path dst && src <> "-" then
    Error "both source and destination refer to the same file"
  else
    let open Base.Result.Let_syntax in
    let%bind contents = read_source src in
    let%bind ciphertext = Crypto.encrypt_string key contents in

    match dst with
    | "-" ->
        print_string ciphertext;
        Ok ()
    | _ -> write_file dst ciphertext

(* Read a file, decrypt it, and save result into destination. *)
let decrypt_file_and_save (key : Types.raw_string) (src : string) (dst : string)
    : (unit, string) result =
  (* Make sure the `src` and `dst` paths don't refer to the same file. *)
  if canonical_path src = canonical_path dst && src <> "-" then
    Error "both source and destination refer to the same file"
  else
    let open Base.Result.Let_syntax in
    let%bind contents = read_source src in

    (* Try to extract the initialization vector from encrypted file. *)
    let%bind hex_cipher, hex_iv = Util.parse_contents contents in
    let%bind plaintext = Crypto.decrypt_string key hex_cipher hex_iv in

    match dst with
    | "-" ->
        print_string plaintext;
        Ok ()
    | _ -> (
        (* Since `write_file` does not like to write files with zero bytes
         * (exception in the Unix module), we test for empty plaintext here,
         * and if so, create an empty "decrypted" file. *)
        match String.length plaintext with
        | 0 -> create_empty_file dst
        | _ -> write_file dst plaintext )

(* Driver routine to react to notifications about changes to a specific file.
 * We continue to listen for updates until the process identified by `pid`
 * terminates. *)
let rec listen_for_fs_updates (pid : int) (key : Types.raw_string)
    (sync_filepath : string) (tmp_filepath : string)
    (msgBox : Fswatch.Event.t array Lwt_mvar.t) : unit Lwt.t =
  (* Number of seconds to sleep in between reads of message queue. *)
  let sleep_count = 1.0 in

  (* First check if the message box is empty. *)
  let open Lwt_ppx_let.Let_syntax in
  let%bind _ =
    match Lwt_mvar.is_empty msgBox with
    | true -> (
        try
          (* If the message box is empty, then check if the process has exited. *)

          (* Waiting for a non-existed process causes an exception. *)
          match Unix.waitpid [ Unix.WNOHANG ] pid with
          | exit_pid, Unix.WEXITED _ -> (
              match exit_pid = pid with
              (* Process died, go back to the caller. *)
              | true -> Lwt.return ()
              (* Some other process died, not the one we cared about. *)
              | false ->
                  let%bind _ = Lwt_unix.sleep sleep_count in
                  listen_for_fs_updates pid key sync_filepath tmp_filepath
                    msgBox )
          (* The process lives on, go back to polling the message box. *)
          | _ ->
              let%bind _ = Lwt_unix.sleep sleep_count in
              listen_for_fs_updates pid key sync_filepath tmp_filepath msgBox
          (* Exception likely because the process is already dead. *)
        with Unix.Unix_error _ -> Lwt.return () )
    | false ->
        (* There are some messages in the message box, go fetch them. *)
        let%bind events = Lwt_mvar.take msgBox in

        (* Function to check whether the array of flags contains `Updated`. *)
        let has_update_flag (flags : Fswatch.Event.flag array) : bool =
          Array.exists (fun flag -> flag = Fswatch.Event.Updated) flags
        in

        (* Function to check whether the array of events has one with the
         * `Updated` flag set. *)
        let check_update (event : Fswatch.Event.t) =
          has_update_flag event.flags
        in

        let%bind _ =
          match Array.exists check_update events with
          (* This is where we encrypt contents and save them into sync_filepath. *)
          | true -> (
              match encrypt_file_and_save key tmp_filepath sync_filepath with
              | Ok _ -> Lwt.return ()
              | Error message -> Lwt_io.eprintf "err: %s\n" message )
          (* No changes to the decrypted file, no action necessary. *)
          | false -> Lwt.return ()
        in

        (* Continue polling as usual. *)
        listen_for_fs_updates pid key sync_filepath tmp_filepath msgBox
  in

  Lwt.return ()

let __monitor_editor (key : Types.raw_string) (sync_filepath : string)
    (editor : string) (tmp_filepath : string) : unit Lwt.t =
  (* Start the binary with the provided arguments. *)
  match Util.run_binary_without_waiting editor [| editor; tmp_filepath |] with
  | Error message ->
      (* If we failed to run the program, dump the message and return. *)
      Lwt_io.eprintf "%s\n" message
  | Ok pid -> (
      (* Otherwise, start the file system monitoring code. *)
      match Fswatch.init_library () with
      | Fswatch.Status.FSW_OK ->
          (* Start a new monitoring session. *)
          let monitor = Fswatch.Monitor.System_default in
          let handle, msgBox = Fswatch_lwt.init_session monitor in

          (* Tell fswatch to monitor the specific file. *)
          Fswatch.add_path handle tmp_filepath;
          Lwt.async (Fswatch_lwt.start_monitor handle);

          (* TODO: Use channels so that we can unlink the temp file here. *)

          (* Start an async thread to receive notifications from the monitor. *)
          let open Lwt_ppx_let.Let_syntax in
          let%bind _ =
            listen_for_fs_updates pid key sync_filepath tmp_filepath msgBox
          in

          (* We've existed the polling code, so stop the monitor. *)
          Fswatch.stop_monitor handle;
          Lwt.return ()
      (* We ran into an error while initializing the fswatch library. *)
      | err -> Lwt_io.eprintf "%s\n" (Fswatch.Status.t_to_string err) )

(* Top-level routine in non-Lwt domain for starting an editor and monitoring
 * changes to the file being edited. *)
let monitor_editor (key : Types.raw_string) (sync_filepath : string)
    (editor : string) (tmp_filepath : string) : unit =
  Lwt_main.run @@ __monitor_editor key sync_filepath editor tmp_filepath

(* Edit the file using a combination of the above two routines. *)
let edit_file (key : Types.raw_string) (filepath : string) (tmp_path : string) :
    (unit, string) result =
  let open Base.Result.Let_syntax in
  (* Get the editor environment variable. *)
  let%bind editor = Util.get_env_var "EDITOR" in

  let%bind _ =
    match check_if_file_exists filepath with
    (* If it exists, decrypt and save the contents to the temporary file. *)
    | true -> decrypt_file_and_save key filepath tmp_path
    (* Otherwise, create an empty "decrypted" file. *)
    | false -> create_empty_file tmp_path
  in

  let _ = monitor_editor key filepath editor tmp_path in

  Ok ()

(* Validate the key by making sure that we can decrypt a known ciphertext. *)
let validate_key (key : Types.raw_string) (dirpath : string) : bool =
  let src_file = Printf.sprintf "%s/.panther" dirpath in

  match check_if_file_exists src_file with
  | true -> (
      match decrypt_file_and_save key src_file Filename.null with
      | Ok _ -> true
      | Error _ -> false )
  (* If the file doesn't exist, the validation is implicitly true. *)
  | false -> true

(* Decrypt then re-crypt using the two keys.  Overwrites source file. *)
let rotate_key_file (old_key : Types.raw_string) (new_key : Types.raw_string)
    (filepath : string) : (unit, string) result =
  let open Base.Result.Let_syntax in
  let%bind contents = read_source filepath in

  (* Parse source string into cipher and IV. *)
  let%bind hex_cipher, hex_iv = Util.parse_contents contents in

  (* Translate cipher and IV into plaintext. *)
  let%bind plaintext = Crypto.decrypt_string old_key hex_cipher hex_iv in

  (* Encrypt plaintext into cipher and IV. *)
  let%bind cipher_and_iv = Crypto.encrypt_string new_key plaintext in

  (* Write new ciphertext to the destination. *)
  match filepath with
  | "-" ->
      print_string cipher_and_iv;
      Ok ()
  | _ -> write_file filepath cipher_and_iv

(* Create a directory, including its parents, if necessary. *)
let rec mk_path (dir_path : string) : unit =
  let parent_dir = Filename.dirname dir_path in

  let _ =
    match is_directory parent_dir with
    | true -> ()
    | false -> mk_path parent_dir
  in

  Unix.mkdir dir_path 0o700

(* Copy file from `src_path` to `dst_path`. *)
let cp_file (src_path : string) (dst_path : string) : (unit, string) result =
  let open Base.Result.Let_syntax in
  let%bind contents = read_file src_path in
  write_file dst_path contents

(* Copy named files into ~/.config/panther/backup.  Create dir if needed. *)
let backup_files (files : string list) : (unit, string) result =
  let home_dir = Unix.getenv "HOME" in
  let backup_dir = Printf.sprintf "%s/.config/panther/backup" home_dir in

  (* If backup directory does not exist, then create it. *)
  let _ =
    match is_directory backup_dir with
    | true -> ()
    | false -> mk_path backup_dir
  in

  (* Copy each file one by one, and accumulate the result. *)
  let fold_function (acc_result : (unit, string) result) (filepath : string) :
      (unit, string) result =
    match acc_result with
    | Ok _ ->
        let dst_path = backup_dir ^ "/" ^ Filename.basename filepath in
        cp_file filepath dst_path
    | Error _ -> acc_result
  in

  List.fold_left fold_function (Ok ()) files

(* Remove specific files from the backup directory. *)
let remove_backed_files (files : string list) : (unit, string) result =
  let home_dir = Unix.getenv "HOME" in
  let backup_dir = Printf.sprintf "%s/.config/panther/backup" home_dir in

  let open Base.Result.Let_syntax in
  (* If backup directory does not exist, then we have a problem. *)
  let%bind _ =
    match is_directory backup_dir with
    | true -> Ok ()
    | false -> Error "Warning: failed to locate backup directory."
  in

  (* Remove each file one by one, and accumulate the result. *)
  let fold_function (acc_result : (unit, string) result) (filepath : string) :
      (unit, string) result =
    match acc_result with
    | Ok _ ->
        let backup_path = backup_dir ^ "/" ^ Filename.basename filepath in
        Ok (Unix.unlink backup_path)
    | Error _ -> acc_result
  in

  List.fold_left fold_function (Ok ()) files

(* Decrypt file(s) using old key, and re-encrypt them using new key. *)
let rotate_key_in_dir (dir : string) (old_key : Types.raw_string)
    (new_key : Types.raw_string) : (unit, string) result =
  match is_directory dir with
  | false ->
      let err_msg = Printf.sprintf "Failed to read '%s' directory." dir in
      Error err_msg
  | true -> (
      (* Get all top-level files in this directory. *)
      let filepaths = read_dir dir in

      (* Backup the top-level files in this directory. *)
      let backup_dir = "$HOME/.config/panther/backup" in
      let backup_msg =
        Printf.sprintf "Backing up file(s) in %s ..." backup_dir
      in

      let tty_chan = open_out "/dev/tty" in
      Console.update_message tty_chan backup_msg;

      let open Base.Result.Let_syntax in
      let%bind _ = backup_files filepaths in

      (* Backup the checksum file, if it exists. *)
      let cfg_file = Printf.sprintf "%s/.panther" dir in

      let home_dir = Unix.getenv "HOME" in
      let checksum_path = ".config/panther/backup/space checksum" in
      let cfg_backup = home_dir ^ "/" ^ checksum_path in

      let%bind _ =
        match check_if_file_exists cfg_file with
        | true -> cp_file cfg_file cfg_backup
        | false -> Ok ()
      in

      let acc_init = Ok () in

      let fold_fn (acc_result : (unit, string) result) (filepath : string) :
          (unit, string) result =
        match acc_result with
        | Ok _ -> rotate_key_file old_key new_key filepath
        | Error _ -> acc_result
      in

      (* Loop over all files and re-key them individually. *)
      let rotate_msg =
        "Encrypting a large number of files can deplete the kernel's entropy.  \n"
        ^ "If this operation appears to take a long time or if appears stuck,  \n"
        ^ "you can add to the kernel's entropy pool by moving the mouse pointer\n"
        ^ " or by typing random characters in any window."
      in
      Console.terminal_message tty_chan rotate_msg;

      let%bind _ = List.fold_left fold_fn acc_init filepaths in

      (* Delete the files that were backed up. *)
      let unlink_msg =
        "Removing files from backup after successful re-key ..."
      in
      Console.update_message tty_chan unlink_msg;

      (* Close the teletype channel. *)
      close_out tty_chan;

      let%bind _ = remove_backed_files filepaths in

      (* Remove the backed config checksum file and create a new one. *)
      match check_if_file_exists cfg_file with
      | true ->
          Unix.unlink cfg_backup;

          (* Encrypt plaintext into cipher and IV. *)
          let%bind cipher_and_iv = Crypto.encrypt_string new_key "panther" in

          (* Write new ciphertext to the destination. *)
          write_file cfg_file cipher_and_iv
      | false -> Ok () )
