(* Read a file in its entirety, and return contents (or error). *)
let read_file (filename : string) : (string, string) result =
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

(* Read a file, encrypt it, and save it into a specific destination. *)
let encrypt_file_and_save (key : Types.raw_string) (src : string) (dst : string)
    : (Types.raw_string, string) result =
  (* Make sure the `src` and `dst` paths don't refer to the same file. *)
  if canonical_path src = canonical_path dst then
    Error "both source and destination refer to the same file"
  else
    let open Base.Result.Let_syntax in
    let%bind contents = read_file src in

    (* Generate a random 16-byte initialization vector. *)
    let iv = Crypto.random_string 16 in

    (* Encrypt the file. *)
    let%bind cipher = Crypto.encrypt key iv (Types.RawString contents) in

    let hex_cipher = Types.hex_base (Crypto.hex_encode cipher)
    and hex_iv = Types.hex_base (Crypto.hex_encode iv) in

    (* Turn ciphertext and IV into hex for serialization. *)
    match write_file dst (hex_cipher ^ hex_iv ^ "\n") with
    | Ok () -> Ok key
    | Error message -> Error message

(* Read a file, decrypt it, and save result into destination. *)
let decrypt_file_and_save (key : Types.raw_string) (src : string) (dst : string)
    : (Types.raw_string, string) result =
  (* Make sure the `src` and `dst` paths don't refer to the same file. *)
  if canonical_path src = canonical_path dst then
    Error "both source and destination refer to the same file"
  else
    let open Base.Result.Let_syntax in
    let%bind contents = read_file src in

    (* Try to extract the initialization vector from encrypted file. *)
    let%bind hex_cipher, hex_iv = Util.parse_contents contents in

    let cipher = Crypto.hex_decode hex_cipher
    and iv = Crypto.hex_decode hex_iv in

    let%bind plaintext = Crypto.decrypt key iv cipher in

    match write_file dst (Types.raw_base plaintext) with
    | Ok () -> Ok key
    | Error message -> Error message

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

  (* Decrypt and save the contents to the temporary file. *)
  let%bind _ = decrypt_file_and_save key filepath tmp_path in

  let _ = monitor_editor key filepath editor tmp_path in

  Ok ()
