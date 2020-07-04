let test_code () : (unit, string) result =
  (* Generate a random AES key. *)
  let password = Lib.Types.raw_base (Lib.Crypto.random_string 13) in
  let password_hash = Lib.Crypto.sha256 password in
  let key = Lib.Types.RawString (String.sub password_hash 0 16) in

  (* Generate a random initialization vector and message to encrypt. *)
  let iv = Lib.Crypto.random_string 16 in
  let message = Lib.Crypto.pseudo_random_string 4096 in

  let open Base.Result.Let_syntax in
  (* Check if the encryption was successful. *)
  let%bind cipher = Lib.Crypto.encrypt key iv message in

  (* Check if the decryption was successful. *)
  let%bind plaintext = Lib.Crypto.decrypt key iv cipher in

  if plaintext = message then Ok () else Error "mismatch"

let () = match test_code () with Ok _ -> exit 0 | Error _ -> exit 1
