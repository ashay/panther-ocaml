let test_code () : (unit, string) result =
  let root = "../../../test/files" in
  let filename = root ^ "/empty-file" in

  let open Base.Result.Let_syntax in
  let%bind contents = Lib.Files.read_file filename in

  (* We expect the following operation to fail, hence we invert the return. *)
  match Lib.Util.parse_contents contents with
  | Ok _ -> Error "parsed empty file"
  | Error _ -> Ok ()

let () = match test_code () with Ok _ -> exit 0 | Error _ -> exit 1
