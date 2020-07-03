let () =
  let root = "../../../test/files" in
  match Lib.Files.read_file (root ^ "/non-existent-file") with
  | Error _ -> exit 0
  | Ok _ -> exit 1
