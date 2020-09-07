let test_code () : (unit, string) result =
  let root = "../../../test/files" in
  let src_file = root ^ "/plaintext-file" in

  (* Generate a temporary file to store the ciphertext. *)
  let src_cipher, _ = Core.Filename.open_temp_file "panther" "" in

  (* Generate a temporary file to backup the config checksum file. *)
  let cfg_backup, _ = Core.Filename.open_temp_file "panther" "" in

  let old_key = Lib.Crypto.random_string 16 in
  let new_key = Lib.Crypto.random_string 16 in

  let open Base.Result.Let_syntax in
  (* Read the plaintext file contents. *)
  let%bind old_text = Lib.Files.read_file src_file in

  (* Generate the ciphertext file using the old key. *)
  let%bind _ = Lib.Files.encrypt_file_and_save old_key src_file src_cipher in

  (* Read ciphertext so that we can compare it against the re-key output. *)
  let%bind old_contents = Lib.Files.read_file src_cipher in

  (* Backup the checksum file, if it exists. *)
  let cfg_file = Filename.dirname src_file ^ "/.panther" in

  let%bind _ =
    match Lib.Files.check_if_file_exists cfg_file with
    | true -> Lib.Files.cp_file cfg_file cfg_backup
    | false -> Ok ()
  in

  (* Save the status.  We come back to this after restoring * from backups. *)
  let status = Lib.Files.rotate_key_file old_key new_key src_cipher in

  (* Read the re-keyed file contents before we restore the file. *)
  let%bind new_contents = Lib.Files.read_file src_cipher in

  (* Restore from our backup.  XXX: If we encounter an error at this point in
     * time, then we're hosed. *)
  let%bind _ =
    match Lib.Files.check_if_file_exists cfg_file with
    | true -> Lib.Files.cp_file cfg_backup cfg_file
    | false -> Ok ()
  in

  (* Remove all generated files. *)
  Unix.unlink src_cipher;
  Unix.unlink cfg_backup;

  match status with
  | Ok _ -> (
      (* Check if the source file was re-keyed correctly. *)
      let%bind new_cipher, new_iv = Lib.Util.parse_contents new_contents in
      let%bind new_text = Lib.Crypto.decrypt_string new_key new_cipher new_iv in

      match old_contents <> new_contents with
      | true -> (
          match new_text = old_text with
          | true -> Ok ()
          | false -> Error "mismatch" )
      | false -> Error "re-key had no effect on ciphertext" )
  | Error _ -> status

let () = match test_code () with Ok _ -> exit 0 | Error _ -> exit 1
