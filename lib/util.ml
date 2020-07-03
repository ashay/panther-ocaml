(* Check if the string ends with a 16-byte initialization vector, which is
 * stored as a 32 hex values.  If the initialization vector exists, return the
 * ciphertext, which is the string before the initialization vector, and the
 * initialization vector. *)
let parse_contents (contents : Types.hex_string) :
    (Types.hex_string * Types.hex_string, string) result =
  let hex_contents = Types.hex_base contents in
  let length = String.length hex_contents in
  match length >= 32 with
  | false -> Error "file is too small to contain an initialization vector."
  | true ->
      (* We count *33* characters back because the file includes a newline. *)
      let cipher = String.sub hex_contents 0 (length - 33) in
      let iv = String.sub hex_contents (length - 33) 32 in

      Ok (Types.HexString cipher, Types.HexString iv)

(* Read a password from the console and turn it into an AES key. *)
let gather_key () : Types.raw_string =
  (* Read password string from stdin without echoing characters. *)
  Console.update_message "password: ";
  let password = Console.read_without_echo () in

  (* Compute its hash and take just the first 16 bytes to use as the key. *)
  let pw_hash = Crypto.sha256 password in

  Console.clear_message ();
  Types.RawString (String.sub pw_hash 0 16)
