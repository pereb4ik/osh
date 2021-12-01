let f (x,y) = 
  match Unix.fork () with
| 0 -> Unix.execvp x y
| _ -> Unix.wait();; ()

let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
    let _ = List.map f (Parser.main Lexer.token linebuf) in
    Printf.printf "%!"
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel
  
let () =
  if Array.length Sys.argv > 1 then
    repeat (Lexing.from_channel (open_in Sys.argv.(1)))
  else
    repeat (Lexing.from_channel stdin)