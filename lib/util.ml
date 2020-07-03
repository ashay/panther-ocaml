(* Check if the string ends with a 16-byte initialization vector, which is
 * stored as a 32 hex values.  If the initialization vector exists, return the
 * ciphertext, which is the string before the initialization vector, and the
 * initialization vector. *)
let parse_contents (contents : string) : (string * string, string) result =
  let length = String.length contents in
  match length >= 32 with
  | false -> Error "file is too small to contain an initialization vector."
  | true -> (
      (* We count *33* characters back because the file includes a newline. *)
      let hex_cipher = String.sub contents 0 (length - 33) in
      let hex_iv = String.sub contents (length - 33) 32 in

      match Crypto.hex_decode [ hex_cipher; hex_iv ] with
      | [ cipher; iv ] -> Ok (cipher, iv)
      | _ -> Error "hex_decode returned unexpected number of values!" )

(* Read a password from the console and turn it into an AES key. *)
let gather_key () : string =
  (* Read password string from stdin without echoing characters. *)
  Console.update_message "password: ";
  let password = Console.read_without_echo () in

  (* Compute its hash and take just the first 16 bytes to use as the key. *)
  let pw_hashes = Crypto.sha256 [ password ] in
  let hash = List.hd pw_hashes in

  Console.clear_message ();
  String.sub hash 0 16
