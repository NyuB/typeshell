%token <string> DOLLAR_ID
%token EQ
%token ECHO
%token <string> ID
%token SEPARATOR
%token <string> STRING_LITERAL
%token EOF
%token VAR

%start <Lang.program> prog

%%

prog:
    | commands = separated_list(SEPARATOR, command); EOF { commands }
    ;

command:
    | ECHO; id = ID { Echo id }
    | a = assignment { a }
    ;

assignment:
    | a = const_assignment { Assign a }
    | a = var_assignment { Assign a }
    ;

const_assignment:
    | name = ID; EQ; value = STRING_LITERAL { Lang.{ name; expression = Str value; const = true } }
    | name = ID; EQ; env_name = DOLLAR_ID { Lang.{ name; expression = Env env_name; const = true } }
    ;

var_assignment:
    | VAR; a = const_assignment { Lang.{ a with const = false } }
    ;