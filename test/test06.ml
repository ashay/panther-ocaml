let () =
  let root = "../../../test/files" in
  let src_filename = root ^ "/valid-file" in
  let dst_filename = root ^ "/../files/valid-file" in

  match Lib.Files.decrypt_and_save src_filename dst_filename with
  | Error _ -> exit 0

  (* Technically, we will not finish since we will block on user input. *)
  | Ok _ -> exit 1
