%token EOL
%token <string> ITEM /* command or args */
%token DELIM /* ; */
%token PIPEIN
%token PIPEOUT

%start <(string * string array) list> main

%%

main:
| l = separated_list(DELIM, expr) EOL
    { l }

expr:
| c = ITEM l = list(ITEM)
    (* Append command name to head of arg list *)
    { (c, Array.of_list (c :: l)) }
