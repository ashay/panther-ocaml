let () =
  let root = "../../../test/files" in
  let src_filename = root ^ "/valid-file" in
  let dst_filename = root ^ "/../files/valid-file" in
  let key = Lib.Crypto.random_string 16 in

  match Lib.Files.decrypt_file_and_save key src_filename dst_filename with
  | Error _ -> exit 0
  | Ok _ -> exit 1
