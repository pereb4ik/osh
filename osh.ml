let eval l =
  let open Unix in
  let rec makechain chain ic =
    let (fd_in, fd_out) = pipe () in
    if stdin <> ic then dup2 ic stdin;
    match chain with
    | [(cmd, args)] -> execvp cmd args
    | (cmd, args) :: t -> (match fork () with
                    | 0 -> close fd_out; makechain t fd_in
                    | _ -> close fd_in; dup2 fd_out stdout; execvp cmd args
                    )
    | [] -> ()
  in makechain l stdin

let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
    let _ = List.map eval (Parser.main Lexer.token linebuf) in ()
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