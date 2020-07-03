(* Read a file in its entirety, and return contents (or error). *)
let read_file (filename : Types.raw_string) : (string, string) result =
  try
    let raw_filename = Types.raw_base filename in
    let fd = Unix.openfile raw_filename [ O_RDONLY ] 0 in

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
let write_file (filename : Types.raw_string) (contents : string) :
    (unit, string) result =
  try
    let raw_filename = Types.raw_base filename in
    let fd = Unix.openfile raw_filename [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in

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

(* Read a file, encrypt it, and save it into a specific destination. *)
let encrypt_and_save (src : string) (dst : string) : (unit, string) result =
  match read_file (Types.RawString src) with
  | Error message -> Error message
  | Ok contents -> (
      (* Get the key and initialization vector from the console. *)
      let key = Util.gather_key () in
      let iv = Crypto.random_string 16 in

      (* Encrypt the file. *)
      match Crypto.encrypt key iv (Types.RawString contents) with
      | Error message -> Error message
      | Ok cipher ->
          let dst_filename = Types.RawString dst
          and hex_cipher = Types.hex_base (Crypto.hex_encode cipher)
          and hex_iv = Types.hex_base (Crypto.hex_encode iv) in

          (* Turn ciphertext and initialization vector into hex for serdes. *)
          write_file dst_filename (hex_cipher ^ hex_iv ^ "\n") )

(* Read a file, decrypt it, and save result into destination. *)
let decrypt_and_save (src : string) (dst : string) : (unit, string) result =
  match read_file (Types.RawString src) with
  | Error message -> Error message
  | Ok contents -> (
      (* Try to extract the initialization vector from the encrypted file. *)
      match Util.parse_contents (Types.HexString contents) with
      | Error message -> Error message
      | Ok (hex_cipher, hex_iv) -> (
          let cipher = Crypto.hex_decode hex_cipher
          and iv = Crypto.hex_decode hex_iv in

          (* Get the key from the console. *)
          let key = Util.gather_key () in

          match Crypto.decrypt key iv cipher with
          | Error message -> Error message
          | Ok plaintext ->
              write_file (Types.RawString dst) (Types.raw_base plaintext) ) )
