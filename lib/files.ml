(* Read a file in its entirety, and return contents (or error). *)
let read_file (filename : string) : (string, string) result =
  try
    let fd = Unix.openfile filename [ O_RDONLY ] 0 in

    (* Find file size and revert file descriptor back to start. *)
    let file_size = Unix.lseek fd 0 Unix.SEEK_END
    and _ = Unix.lseek fd 0 Unix.SEEK_SET in

    (* We're about to read in the entire file.  Create a large buffer. *)
    let buffer = Bytes.create file_size in

    (* Check whether we were able to read the entire file. *)
    let ret_val =
      (* If it's an empty file, return an empty buffer. *)
      match file_size with
      | 0 -> Ok ""
      | _ -> (
          match Unix.read fd buffer 0 file_size with
          | 0 -> Error (Printf.sprintf "failed to read %d byte(s)" file_size)
          | _ -> Ok (Bytes.to_string buffer) )
    in

    Unix.close fd;
    ret_val
  with Unix.Unix_error (err, fn, arg) ->
    let msg = Unix.error_message err in
    Error (Printf.sprintf "%s: %s [%s]" fn msg arg)

(* Write file contents.  We want to be able to write both raw string and
 * hex-encoded strings. *)
let write_file (filename : string) (contents : string) : (unit, string) result =
  try
    let fd = Unix.openfile filename [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in

    (* Turn the string buffer into a byte buffer. *)
    let buffer = Bytes.of_string contents in
    let buffer_size = Bytes.length buffer in

    (* And write the file. *)
    let ret_val =
      match Unix.write fd buffer 0 buffer_size with
      | 0 -> Error (Printf.sprintf "failed to write %d bytes(s)" buffer_size)
      | _ -> Ok ()
    in

    Unix.close fd;
    ret_val
  with Unix.Unix_error (err, fn, arg) ->
    let msg = Unix.error_message err in
    Error (Printf.sprintf "%s: %s [%s]" fn msg arg)

(* Get canonical path of the file, if it exists.  Masks exceptions. *)
let canonical_path (filepath : string) : string =
  try Core.Filename.realpath filepath
  with
  (* If we couldn't find the real path, return the input path. *)
  | Unix.Unix_error _ ->
    filepath

(* Read a file, encrypt it, and save it into a specific destination. *)
let encrypt_file_and_save (src : string) (dst : string) :
    (Types.raw_string, string) result =
  (* Make sure the `src` and `dst` paths don't refer to the same file. *)
  if canonical_path src = canonical_path dst then
    Error "both source and destination refer to the same file"
  else
    let open Base.Result.Let_syntax in
    let%bind contents = read_file src in

    (* Get the key and initialization vector from the console. *)
    let key = Util.gather_key () in
    let iv = Crypto.random_string 16 in

    (* Encrypt the file. *)
    let%bind cipher = Crypto.encrypt key iv (Types.RawString contents) in

    let hex_cipher = Types.hex_base (Crypto.hex_encode cipher)
    and hex_iv = Types.hex_base (Crypto.hex_encode iv) in

    (* Turn ciphertext and IV into hex for serialization. *)
    match write_file dst (hex_cipher ^ hex_iv ^ "\n") with
    | Ok () -> Ok key
    | Error message -> Error message

(* Read a file, decrypt it, and save result into destination. *)
let decrypt_file_and_save (src : string) (dst : string) :
    (Types.raw_string, string) result =
  (* Make sure the `src` and `dst` paths don't refer to the same file. *)
  if canonical_path src = canonical_path dst then
    Error "both source and destination refer to the same file"
  else
    let open Base.Result.Let_syntax in
    let%bind contents = read_file src in

    (* Try to extract the initialization vector from encrypted file. *)
    let%bind hex_cipher, hex_iv = Util.parse_contents contents in

    let cipher = Crypto.hex_decode hex_cipher
    and iv = Crypto.hex_decode hex_iv in

    (* Get the key from the console. *)
    let key = Util.gather_key () in

    let%bind plaintext = Crypto.decrypt key iv cipher in

    match write_file dst (Types.raw_base plaintext) with
    | Ok () -> Ok key
    | Error message -> Error message
