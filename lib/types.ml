type raw_string = RawString of string

type hex_string = HexString of string

(* Accessors for getting the underlying string. *)
let raw_base (input : raw_string) : string =
  let (RawString __input) = input in
  __input

let hex_base (input : hex_string) : string =
  let (HexString __input) = input in
  __input
