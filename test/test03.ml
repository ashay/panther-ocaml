let () =
  let root = "../../../test/files" in
  let filename = Lib.Types.RawString (root ^ "/valid-file") in

  match Lib.Files.read_file filename with
  | Ok contents -> (
      let hex_ref_iv = Lib.Types.HexString "def462085cb5cf4a09365949bf8be03a"
      and hex_ref_cipher =
        Lib.Types.HexString "dda4832c7e1bc8bdae2ffcb2bf464196"
      in

      (* Perform the actual operation under test. *)
      match Lib.Util.parse_contents (Lib.Types.HexString contents) with
      | Ok (hex_cipher, hex_iv) ->
          if hex_cipher = hex_ref_cipher && hex_iv = hex_ref_iv then exit 0
          else exit 1
      (* Parsing failed. *)
      | Error _ -> exit 2 )
  (* Failed to read file. *)
  | Error _ -> exit 1
