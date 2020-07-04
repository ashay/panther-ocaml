let () =
  let root = "../../../test/files" in
  let filename = root ^ "/valid-file" in
  let key = Lib.Crypto.random_string 16 in

  match Lib.Files.encrypt_file_and_save key filename filename with
  | Error _ -> exit 0
  | Ok _ -> exit 1
