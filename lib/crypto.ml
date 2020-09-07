(* Wrapper to generate a random string of specific size. *)
let random_string (length : int) : Types.raw_string =
  Types.RawString (Cryptokit.Random.string Cryptokit.Random.secure_rng length)

(* Wrapper to generate a pseudo random string of specific size. *)
let pseudo_random_string (length : int) : Types.raw_string =
  let seed = Types.raw_base (random_string 16) in
  let rng = Cryptokit.Random.pseudo_rng seed in
  Types.RawString (Cryptokit.Random.string rng length)

(* Wrapper to compute hex encoding of an input string. *)
let hex_encode (input : Types.raw_string) : Types.hex_string =
  let transform = Cryptokit.Hexa.encode ()
  and raw_input = Types.raw_base input in
  Types.HexString (Cryptokit.transform_string transform raw_input)

(* Wrapper to compute hex decoding of an input string. *)
let hex_decode (input : Types.hex_string) : Types.raw_string =
  let transform = Cryptokit.Hexa.decode ()
  and hex_input = Types.hex_base input in
  Types.RawString (Cryptokit.transform_string transform hex_input)

(* Wrapper to compute SHA256 hash of a list of input strings. *)
let sha256 (inputs : string) : string =
  let hash = Cryptokit.Hash.sha256 () in
  Cryptokit.hash_string hash inputs

(* Generic encryption function that uses AES in CBC mode. *)
let encrypt (key : Types.raw_string) (iv : Types.raw_string)
    (message : Types.raw_string) : (Types.raw_string, string) result =
  let padding = Cryptokit.Padding.length
  and raw_key = Types.raw_base key
  and raw_iv = Types.raw_base iv
  and raw_message = Types.raw_base message in

  let aes =
    Cryptokit.Cipher.aes ~mode:CBC ~pad:padding ~iv:raw_iv raw_key Encrypt
  in

  try Ok (Types.RawString (Cryptokit.transform_string aes raw_message))
  with _ -> Error "failed to encrypt with given key and initialization vector"

(* Generic decryption function that uses AES in CBC mode. *)
let decrypt (key : Types.raw_string) (iv : Types.raw_string)
    (cipher : Types.raw_string) : (Types.raw_string, string) result =
  let padding = Cryptokit.Padding.length
  and raw_key = Types.raw_base key
  and raw_iv = Types.raw_base iv
  and raw_cipher = Types.raw_base cipher in

  let aes =
    Cryptokit.Cipher.aes ~mode:CBC ~pad:padding ~iv:raw_iv raw_key Decrypt
  in

  try Ok (Types.RawString (Cryptokit.transform_string aes raw_cipher))
  with _ -> Error "failed to decrypt with given key and initialization vector"

(* Encrypt input and return the ciphertext (or error). *)
let encrypt_string (key : Types.raw_string) (contents : string) :
    (string, string) result =
  let open Base.Result.Let_syntax in
  (* Generate a random 16-byte initialization vector. *)
  let iv = random_string 16 in

  (* Encrypt the string. *)
  let%bind cipher = encrypt key iv (Types.RawString contents) in

  let hex_cipher = Types.hex_base (hex_encode cipher)
  and hex_iv = Types.hex_base (hex_encode iv) in

  Ok (hex_cipher ^ hex_iv ^ "\n")

(* Decrypt input and return the plaintext (or error). *)
let decrypt_string (key : Types.raw_string) (hex_cipher : Types.hex_string)
    (hex_iv : Types.hex_string) : (string, string) result =
  let open Base.Result.Let_syntax in
  let cipher = hex_decode hex_cipher and iv = hex_decode hex_iv in

  let%bind plaintext = decrypt key iv cipher in
  Ok (Types.raw_base plaintext)
