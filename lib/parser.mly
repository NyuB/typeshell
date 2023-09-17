%token <string> DOLLAR_ID
%token EQ
%token ECHO
%token <string> ID
%token SEPARATOR
%token <string> STRING_LITERAL
%token EOF

%start <Lang.program> prog

%%

prog:
    | commands = separated_list(SEPARATOR, command); EOF { commands }
    ;

command:
    | ECHO; id = ID { Echo id }
    | name = ID; EQ; value = STRING_LITERAL { Assign (name, Str value) }
    | name = ID; EQ; env_name = DOLLAR_ID { Assign (name, Env env_name) }
    ;