let () =
  let root = "../../../test/files" in
  match Lib.Util.parse_contents (root ^ "/empty-file") with
  | Ok _ -> exit 1
  | Error _ -> exit 0
