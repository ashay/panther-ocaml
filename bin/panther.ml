(* Show valid options to the program. *)
let printUsage () : unit =
  let name = "panther" in
  let description = "a handy utility program to encrypt and decrypt files" in
  Printf.printf "%s - %s.\n" name description;

  let commands = "encrypt|decrypt" in
  Printf.printf "USAGE: %s %s source-file destination-file\n" name commands

(* Driver for encrypting a file and saving ciphertext to a destination. *)
let encrypt_file (src_file : string) (dst_file : string) : unit =
  match Lib.Files.encrypt_file_and_save src_file dst_file with
  | Ok _ -> ()
  | Error message -> Lib.Console.terminal_message ("encrypt: " ^ message)

(* Driver for decrypting a file and saving plaintext to a destination. *)
let decrypt_file (src_file : string) (dst_file : string) : unit =
  match Lib.Files.decrypt_file_and_save src_file dst_file with
  | Ok _ -> ()
  | Error message -> Lib.Console.terminal_message ("decrypt: " ^ message)

(* Try to decrypt the file and if successful, save it into /tmp before opening
 * editor.  Once editor closes, encrypt contents, rewrite original file, and
 * delete the file in /tmp. *)
let edit_file (filepath : string) : unit =
  let tmp_path, _ = Core.Filename.open_temp_file "panther" "ext" in

  match Lib.Files.decrypt_file_and_save filepath tmp_path with
  | Ok _ -> (
      let pid =
        Unix.create_process "/usr/bin/nvim"
          [| "/usr/bin/nvim"; tmp_path |]
          Unix.stdin Unix.stdout Unix.stderr
      in
      let _ = Unix.waitpid [] pid in

      match Lib.Files.encrypt_file_and_save tmp_path filepath with
      | Ok _ -> Unix.unlink tmp_path
      | Error message -> Lib.Console.terminal_message ("edit: " ^ message) )
  | Error message -> Lib.Console.terminal_message ("edit: " ^ message)

(* Entry point; check arguments and direct control accordingly. *)
let () =
  let arg_count = Array.length Sys.argv in
  match Array.sub Sys.argv 1 (arg_count - 1) with
  | [| "encrypt"; src_file; dst_file |] -> encrypt_file src_file dst_file
  | [| "decrypt"; src_file; dst_file |] -> decrypt_file src_file dst_file
  | [| "edit"; filepath |] -> edit_file filepath
  | _ -> printUsage ()
