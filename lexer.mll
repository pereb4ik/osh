{
  open Parser

  exception Error of string

  (* Extra get *)
  let eget (a, _) = match a with
  | Some(s) -> s
  | None -> ""

}

let allowd_cmd = ['A'-'Z' 'a'-'z' '0'-'9' '_' '-' '\'' '\"' '=' '.' '/']

(* This rule looks for a single line, terminated with '\n' or eof.
   It returns a pair of an optional string (the line that was found)
   and a Boolean flag (false if eof was reached). *)
rule line = parse
    (* To remove comments *)
| (([^'\n''#']* as line) '#' [^'\n']* '\n')
    { Some (line ^ "\n"), true }
| ([^'\n']* as ln)"\\\n"
    { Some (ln ^ eget(line lexbuf)), true }
| ([^'\n']* '\n') as line
    (* Normal case: one line, no eof. *)
    { Some line, true }
| eof
    (* Normal case: no data, eof. *)
    { None, false }
| ([^'\n']+ as line) eof
    (* Special case: some data but missing '\n', then eof.
       Consider this as the last line, and add the missing '\n'. *)
    { Some (line ^ "\n"), false }

(* This rule analyzes a single line and turns it into a stream of
   tokens. *)

and token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOL }
| '\''([^'\n']+ as it)'\''
    { ITEM (it) }
| '\"'([^'\n']+ as it)'\"'
    { ITEM (it) }
| '$'(allowd_cmd+ as var)
    { ITEM (Unix.getenv var) }
| allowd_cmd+ as it
    { ITEM (it) }
| ';'
    { DELIM }
| '|'
    { PIPE }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }