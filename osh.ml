let eval l = match l with
| [] -> ()
| _ ->
  let open Unix in
  let rec makechain chain ic =
    let open Unix in
    let (fd_in, fd_out) = pipe () in
    match fork () with
    | 0 ->  if stdin <> ic then
              dup2 ic stdin;
            close fd_in;
            (match chain with
            | [(cmd, args)] -> execvp cmd args
            | (cmd, args) :: _ -> dup2 fd_out stdout; execvp cmd args
            | [] -> ()) 
    | _ -> close fd_out;
            let ll = List.tl chain in 
            if ll <> [] then
              makechain ll fd_in;
            let _ = wait() in ()
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

let rec repeat channel (prompt : bool) =
  if prompt then begin
    print_string "osh% ";
    flush stdout
  end;
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel prompt
  
let () =
  if Array.length Sys.argv > 1 then
    repeat (Lexing.from_channel (open_in Sys.argv.(1))) false
  else
    repeat (Lexing.from_channel stdin) true