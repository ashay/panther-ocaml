(* Take cursor back to begining of line. *)
let reset_cursor () : unit = Printf.printf "\r"

(* Clear existing message and show a new one. *)
let clear_message () : unit =
  Printf.printf "\r                                                          "

(* Clear existing message and show a new one. *)
let update_message (message : string) : unit =
  clear_message ();
  Printf.printf "\r%s" message

(* Message that ends in a line feed. *)
let terminal_message (message : string) : unit =
  clear_message ();
  update_message message;
  Printf.printf "\n"

(* Read from stdin without showing the entered characters. *)
let read_without_echo () : string =
  (* First disable echoing of characters. *)
  let tios = Unix.tcgetattr Unix.stdin in
  tios.c_echo <- false;
  Unix.tcsetattr Unix.stdin Unix.TCSANOW tios;

  (* Now read the console input. *)
  let console_input = read_line () in

  (* Finally re-enable echoing of characters. *)
  let tios = Unix.tcgetattr Unix.stdin in
  tios.c_echo <- true;
  Unix.tcsetattr Unix.stdin Unix.TCSANOW tios;

  console_input

(* Read from stdin without showing the entered characters. *)
let read_with_echo () : string =
  (* Forcefully enable echoing of characters. *)
  let tios = Unix.tcgetattr Unix.stdin in
  tios.c_echo <- true;
  Unix.tcsetattr Unix.stdin Unix.TCSANOW tios;

  (* Now read the console input. *)
  read_line ()
