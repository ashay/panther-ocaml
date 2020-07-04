let test_code () : (unit, string) result =
  let root = "../../../test/files" in
  let filename = root ^ "/valid-file" in

  (* Reference IV and cipher. *)
  let ref_iv = Lib.Types.HexString "def462085cb5cf4a09365949bf8be03a"
  and ref_cipher = Lib.Types.HexString "dda4832c7e1bc8bdae2ffcb2bf464196" in

  (* Read the encrypted file. *)
  let open Base.Result.Let_syntax in
  let%bind contents = Lib.Files.read_file filename in

  (* Separate out the cipher and IV from the encrypted file. *)
  let%bind cipher, iv = Lib.Util.parse_contents contents in

  (* And finally, check against the reference values. *)
  if cipher = ref_cipher && iv = ref_iv then Ok () else Error "mismatch"

let () = match test_code () with Ok _ -> exit 0 | Error _ -> exit 1
