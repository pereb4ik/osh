%token EOL
%token <string> ITEM /* command or args */
%token DELIM /* ; */
%token PIPEIN
%token PIPEOUT
%token PIPE

%start <(string * string array)list list> main

%%

main:
| l = separated_list(DELIM, lopt(chain)) EOL
    { l }

/* Copy-paste of loption, but inline*/
%inline lopt(X):
| { [] }
| x = X
    { x }

chain:
| l = separated_nonempty_list(PIPE, expr)
    { l }

expr:
| c = ITEM l = list(ITEM)
    (* Append command name to head of arg list *)
    { (c, Array.of_list (c :: l)) }
