(* Check if the string ends with a 16-byte initialization vector, which is
 * stored as a 32 hex values.  If the initialization vector exists, return the
 * ciphertext, which is the string before the initialization vector, and the
 * initialization vector. *)
let parse_contents (contents : string) :
    (Types.hex_string * Types.hex_string, string) result =
  let length = String.length contents in
  match length >= 32 with
  | false -> Error "file is too small to contain an initialization vector."
  | true ->
      (* We count *33* characters back because the file includes a newline. *)
      let cipher = String.sub contents 0 (length - 33) in
      let iv = String.sub contents (length - 33) 32 in

      Ok (Types.HexString cipher, Types.HexString iv)

(* Read a password from the console and turn it into an AES key. *)
let gather_key () : Types.raw_string =
  (* Read password string from stdin without echoing characters. *)
  Console.update_message "password: ";
  let password = Console.read_without_echo () in

  (* Compute its hash and take just the first 16 bytes to use as the key. *)
  let pw_hash = Crypto.sha256 password in

  (* Take the cursor back to the beginning of the line. *)
  Console.clear_message ();
  Console.reset_cursor ();

  (* Return the first 16 bytes of the hash as the key. *)
  Types.RawString (String.sub pw_hash 0 16)

(* Read the environment variable but if it doesn't exist then return error. *)
let get_env_var (name : string) : (string, string) result =
  try
    match Unix.getenv name with
    | "" -> Error (Printf.sprintf "environment variable `%s` is empty" name)
    | result -> Ok result
  with Not_found -> Error "environment variable not found"

(* Allowed binary names.  This doesn't prevent someone from symlinking or
 * changing the PATH to execute another program, but that attack vector is out
 * of scope for now. *)
let validate_binary (binary : string) : bool =
  match binary with
  | "vi" -> true
  | "vim" -> true
  | "emacs" -> true
  | "nvim" -> true
  | _ -> false

(* Run a binary with the provided arguments and wait until it finishes. *)
let run_binary (binary : string) (args : string array) : (unit, string) result =
  (* Make sure it's a sanitized name. This isn't fool-proof, though. *)
  match validate_binary binary with
  | false -> Error "requested application not white-listed; will not execute"
  | true ->
      (* Start the application. *)
      let pid =
        Unix.create_process binary args Unix.stdin Unix.stdout Unix.stderr
      in

      (* Wait for the application to terminate. *)
      let _ = Unix.waitpid [] pid in

      Ok ()
