(* Take cursor back to begining of line. *)
let reset_cursor (channel : out_channel) : unit =
  Printf.fprintf channel "\r";
  flush channel

(* Clear existing message and show a new one. *)
let clear_message (channel : out_channel) : unit =
  Printf.fprintf channel
    "\r                                                        ";
  flush channel

(* Clear existing message and show a new one. *)
let update_message (channel : out_channel) (message : string) : unit =
  clear_message channel;
  Printf.fprintf channel "\r%s" message;
  flush channel

(* Message that ends in a line feed. *)
let terminal_message (channel : out_channel) (message : string) : unit =
  clear_message channel;
  update_message channel (message ^ "\n");
  flush channel

(* Read from stdin without showing the entered characters. *)
let read_without_echo () : string =
  (* Read from tty and not from stdin to avoid reading passwd from stdout. *)
  let fd = Unix.openfile "/dev/tty" [ O_RDONLY ] 0 in

  (* First disable echoing of characters. *)
  let tios = Unix.tcgetattr fd in
  tios.c_echo <- false;
  Unix.tcsetattr fd Unix.TCSANOW tios;

  (* Now read the console input. *)
  let tty_chan = open_in "/dev/tty" in
  let console_input = input_line tty_chan in
  close_in tty_chan;

  (* Finally, re-eanble echoing of characters. *)
  let tios = Unix.tcgetattr fd in
  tios.c_echo <- true;
  Unix.tcsetattr fd Unix.TCSANOW tios;

  Unix.close fd;
  console_input
