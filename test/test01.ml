let () =
  let root = "../../../test/files" in
  let filename = root ^ "/non-existent-file" in

  match Lib.Files.read_file filename with Error _ -> exit 0 | Ok _ -> exit 1
