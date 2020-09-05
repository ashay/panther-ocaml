(* Show valid options to the program. *)
let printUsage () : unit =
  let name = "panther" in
  let description = "a handy utility program to encrypt and decrypt files" in
  Printf.printf "%s - %s.\n" name description;

  Printf.printf "USAGE: %s command args...\n\n" name;
  Printf.printf
    "  %s enc|encrypt src dst  # encrypt src file and save into dst file\n" name;
  Printf.printf
    "  %s dec|decrypt src dst  # decrypt src file and save into dst file\n" name;
  Printf.printf
    "  %s ed|edit src          # edit src file by copying decrypted text into \
     /tmp\n"
    name

(* Driver for encrypting a file and saving ciphertext to a destination. *)
let encrypt_file (src_file : string) (dst_file : string) : unit =
  (* Get the key from the console. *)
  let key = Lib.Util.gather_key () in

  match Lib.Files.encrypt_file_and_save key src_file dst_file with
  | Ok _ -> ()
  | Error message -> Lib.Console.terminal_message stderr ("encrypt: " ^ message)

(* Driver for decrypting a file and saving plaintext to a destination. *)
let decrypt_file (src_file : string) (dst_file : string) : unit =
  (* Get the key from the console. *)
  let key = Lib.Util.gather_key () in

  match Lib.Files.decrypt_file_and_save key src_file dst_file with
  | Ok _ -> ()
  | Error message -> Lib.Console.terminal_message stderr ("decrypt: " ^ message)

(* Try to decrypt the file and if successful, save it into /tmp before opening
 * editor.  Once editor closes, encrypt contents, rewrite original file, and
 * delete the file in /tmp. *)
let edit_file (filepath : string) : unit =
  (* Get the key from the console. *)
  let key = Lib.Util.gather_key () in

  (* Generate a temporary file to save the decrypted contents. *)
  let tmp_path, _ = Core.Filename.open_temp_file "panther" "" in

  (* Be sure to remove the temporary file, which may contain the decrypted
   * text, regardless of the success (or failure) of the editing step. *)
  match Lib.Files.edit_file key filepath tmp_path with
  | Ok _ -> Unix.unlink tmp_path
  | Error message ->
      Unix.unlink tmp_path;
      Lib.Console.terminal_message stderr ("edit: " ^ message)

(* Entry point; check arguments and direct control accordingly. *)
let () =
  let arg_count = Array.length Sys.argv in
  match Array.sub Sys.argv 1 (arg_count - 1) with
  (* Allow "enc" or "encrypt" to create ciphertext. *)
  | [| "enc"; src_file; dst_file |] -> encrypt_file src_file dst_file
  | [| "encrypt"; src_file; dst_file |] -> encrypt_file src_file dst_file
  (* Allow "dec" or "decrypt" to create plaintext. *)
  | [| "dec"; src_file; dst_file |] -> decrypt_file src_file dst_file
  | [| "decrypt"; src_file; dst_file |] -> decrypt_file src_file dst_file
  (* Allow "ed" or "edit" to update files using the editor. *)
  | [| "edit"; filepath |] -> edit_file filepath
  (* Allow "-h" or "--help" to print possible invocations. *)
  | [| "-h" |] -> printUsage ()
  | [| "--help" |] -> printUsage ()
  | _ -> printUsage ()
