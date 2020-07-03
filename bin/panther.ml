(* Show valid options to the program. *)
let printUsage () : unit =
  let name = "panther" in
  let description = "a handy utility program to encrypt and decrypt files" in
  Printf.printf "%s - %s.\n" name description;

  let commands = "encrypt|decrypt" in
  Printf.printf "USAGE: %s %s source-file destination-file\n" name commands

(* Driver for encrypting a file and saving ciphertext to a destination. *)
let encrypt_file (src_file : string) (dst_file : string) : unit =
  match Lib.Files.encrypt_and_save src_file dst_file with
  | Ok () -> Lib.Console.reset_cursor ()
  | Error message ->
      Lib.Console.terminal_message (Printf.sprintf "encrypt: %s" message)

(* Driver for decrypting a file and saving plaintext to a destination. *)
let decrypt_file (src_file : string) (dst_file : string) : unit =
  match Lib.Files.decrypt_and_save src_file dst_file with
  | Ok () -> Lib.Console.reset_cursor ()
  | Error message ->
      Lib.Console.terminal_message (Printf.sprintf "decrypt: %s" message)

(* Entry point; check arguments and direct control accordingly. *)
let () =
  let arg_count = Array.length Sys.argv in
  match Array.sub Sys.argv 1 (arg_count - 1) with
  | [| "encrypt"; src_file; dst_file |] -> encrypt_file src_file dst_file
  | [| "decrypt"; src_file; dst_file |] -> decrypt_file src_file dst_file
  | _ -> printUsage ()
