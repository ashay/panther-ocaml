let () =
  (* Generate a random AES key. *)
  let password = Lib.Types.raw_base (Lib.Crypto.random_string 13) in
  let password_hash = Lib.Crypto.sha256 password in
  let key = Lib.Types.RawString (String.sub password_hash 0 16) in

  (* Generate a random initialization vector and message to encrypt. *)
  let iv = Lib.Crypto.random_string 16 in
  let message = Lib.Crypto.pseudo_random_string 4096 in

  (* Check if the encryption was successful. *)
  match Lib.Crypto.encrypt key iv message with
  | Error _ -> exit 1
  | Ok cipher ->

    (* Check if the decryption was successful. *)
      match Lib.Crypto.decrypt key iv cipher with
      | Error _ -> exit 2
      | Ok plaintext ->

          (* Check if the decrypted text matches the original message. *)
          if plaintext = message then
            exit 0
          else
            exit 3
