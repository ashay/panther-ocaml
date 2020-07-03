let () =
  let root = "../../../test/files" in
  match Lib.Files.read_file (root ^ "/valid-file") with
  | Ok contents -> (
      let ref_iv_hex = "def462085cb5cf4a09365949bf8be03a"
      and ref_cipher_hex = "dda4832c7e1bc8bdae2ffcb2bf464196" in

      (* Since `parse_contents` turns hex to bytes, we do the same here. *)
      match Lib.Crypto.hex_decode [ ref_iv_hex; ref_cipher_hex ] with
      | [ ref_iv; ref_cipher ] -> (
          (* Perform the actual operation under test. *)
          match Lib.Util.parse_contents contents with
          | Ok (cipher, iv) ->
              if cipher = ref_cipher && iv = ref_iv then exit 0
              else Printf.printf "%s %s\n" cipher iv;
              exit 1
          (* Parsing failed. *)
          | Error _ -> exit 2 )
      (* We didn't receive a valid list from calling `hex_decode`. *)
      | _ -> exit 3 )
  (* Failed to read file. *)
  | Error _ -> exit 1
