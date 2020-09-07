let test_code () : (unit, string) result =
  let root = "../../../test/files" in
  let src_file = root ^ "/plaintext-file" in

  (* Generate a temporary file to save the decrypted contents. *)
  let perm = 0o600 in
  let in_dir =
    match Lib.Files.check_if_file_exists "/dev/shm" with
    | true -> "/dev/shm"
    | false -> "/tmp"
  in

  let key = Lib.Crypto.random_string 16 in
  let dst_file, _ = Core.Filename.open_temp_file ~perm ~in_dir "panther" "" in
  let txt_file, _ = Core.Filename.open_temp_file ~perm ~in_dir "panther" "" in

  let open Base.Result.Let_syntax in
  (* Encrypt plaintext file and decrypt the encrypted file. *)
  let%bind _ = Lib.Files.encrypt_file_and_save key src_file dst_file in
  let%bind _ = Lib.Files.decrypt_file_and_save key dst_file txt_file in

  (* Read the plaintext file and the decrypted file. *)
  let%bind plain_contents = Lib.Files.read_file src_file in
  let%bind decrypted_contents = Lib.Files.read_file txt_file in

  (* Remove all generated file. *)
  Unix.unlink dst_file;
  Unix.unlink txt_file;

  (* Make sure that the pre-encryption and post-decryption contents match. *)
  if plain_contents = decrypted_contents then Ok () else Error "mismatch"

let () = match test_code () with Ok _ -> exit 0 | Error _ -> exit 1
