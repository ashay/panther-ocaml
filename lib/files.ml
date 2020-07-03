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
      match Unix.read fd buffer 0 file_size with
      | 0 -> Error (Printf.sprintf "failed to read %d byte(s)" file_size)
      | _ -> Ok (Bytes.to_string buffer)
    in

    Unix.close fd;
    ret_val
  with Unix.Unix_error (err, fn, arg) ->
    let msg = Unix.error_message err in
    Error (Printf.sprintf "%s: %s [%s]" fn msg arg)

(* Write file contents. *)
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

(* Read a file, encrypt it, and save it into a specific destination. *)
let encrypt_and_save (src : string) (dst : string) : (unit, string) result =
  match read_file src with
  | Error message -> Error message
  | Ok contents -> (
      (* Get the key and initialization vector from the console. *)
      let key = Util.gather_key () in
      let iv = Crypto.random_string 16 in

      (* Encrypt the file. *)
      match Crypto.encrypt key iv [ contents ] with
      | Error message -> Error message
      | Ok ciphers -> (
          (* Turn ciphertext and initialization vector into hex for serdes. *)
          match Crypto.hex_encode (List.append ciphers [ iv ]) with
          | [ hex_cipher; hex_iv ] ->
              write_file dst (Printf.sprintf "%s%s\n" hex_cipher hex_iv)
          | _ -> Error "hex_encode failed to return correct number of values" )
      )

(* Read a file, decrypt it, and save result into destination. *)
let decrypt_and_save (src : string) (dst : string) : (unit, string) result =
  match read_file src with
  | Error message -> Error message
  | Ok contents -> (
      (* Try to extract the initialization vector from the encrypted file. *)
      match Util.parse_contents contents with
      | Error message -> Error message
      | Ok (cipher, iv) -> (
          (* Get the key from the console. *)
          let key = Util.gather_key () in

          match Crypto.decrypt key iv [ cipher ] with
          | Error message -> Error message
          | Ok plaintexts -> (
              match plaintexts with
              | [ plain ] -> write_file dst plain
              | _ -> Error "hex_decode returned unexpected number of values!" )
          ) )
