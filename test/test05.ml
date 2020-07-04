let () =
  let root = "../../../test/files" in
  let filename = root ^ "/valid-file" in

  match Lib.Files.encrypt_file_and_save filename filename with
  | Error _ -> exit 0
  (* Technically, we will not finish since we will block on user input. *)
  | Ok _ -> exit 1
