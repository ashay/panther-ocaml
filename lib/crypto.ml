(* Generic encryption function that uses AES in CBC mode. *)
let encrypt (key : string) (iv : string) (messages : string list) :
    (string list, string) result =
  let padding = Cryptokit.Padding.length in
  let aes = Cryptokit.Cipher.aes ~mode:CBC ~pad:padding ~iv key Encrypt in

  try Ok (List.map (Cryptokit.transform_string aes) messages)
  with _ ->
    Error "failed to encrypt with given key and initialization vector!"

(* Generic decryption function that uses AES in CBC mode. *)
let decrypt (key : string) (iv : string) (ciphers : string list) :
    (string list, string) result =
  let padding = Cryptokit.Padding.length in
  let aes = Cryptokit.Cipher.aes ~mode:CBC ~pad:padding ~iv key Decrypt in

  try Ok (List.map (Cryptokit.transform_string aes) ciphers)
  with _ ->
    Error "failed to decrypt with given key and initialization vector!"

(* Wrapper to generate a random string of specific size. *)
let random_string (length : int) : string =
  Cryptokit.Random.string Cryptokit.Random.secure_rng length

(* Wrapper to compute base64 encoding of a list of input strings. *)
let b64_encode (inputs : string list) : string list =
  let transform = Cryptokit.Base64.encode_compact () in
  List.map (Cryptokit.transform_string transform) inputs

(* Wrapper to compute base64 decoding of a list of input strings. *)
let b64_decode (inputs : string list) : string list =
  let transform = Cryptokit.Base64.decode () in
  List.map (Cryptokit.transform_string transform) inputs

(* Wrapper to compute hex encoding of a list of input strings. *)
let hex_encode (inputs : string list) : string list =
  let transform = Cryptokit.Hexa.encode () in
  List.map (Cryptokit.transform_string transform) inputs

(* Wrapper to compute hex decoding of a list of input strings. *)
let hex_decode (inputs : string list) : string list =
  let transform = Cryptokit.Hexa.decode () in
  List.map (Cryptokit.transform_string transform) inputs

(* Wrapper to compute SHA256 hash of a list of input strings. *)
let sha256 (inputs : string list) : string list =
  let hash = Cryptokit.Hash.sha256 () in
  List.map (Cryptokit.hash_string hash) inputs
