let () =
  let root = "../../../test/files" in
  let filename = Lib.Types.RawString (root ^ "/empty-file") in

  match Lib.Files.read_file filename with
  | Ok contents -> (
      match Lib.Util.parse_contents (Lib.Types.HexString contents) with
      | Ok _ -> exit 1
      | Error _ -> exit 0 )
  | Error msg ->
      print_string msg;
      exit 2
