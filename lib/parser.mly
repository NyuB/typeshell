%token <string> DOLLAR_ID
%token EQ
%token ECHO
%token <string> ID
%token SEPARATOR
%token <string> STRING_LITERAL
%token EOF
%token VAL
%token VAR

%start <Lang.program> prog

%%

prog:
    | commands = separated_list(SEPARATOR, command); EOF { commands }
    ;

command:
    | ECHO; id = ID { Echo id }
    | a = assignment { Assign a }
    | d = declaration { Declare d }
    ;

declaration:
    | cd = const_declaration { cd }
    | vd = var_declaration { vd }
    ;

assignment:
    | variable_name = ID; EQ; s = STRING_LITERAL { Lang.{ variable_name; expression = Str s } }
    | variable_name = ID; EQ; env_name = DOLLAR_ID { Lang.{ variable_name; expression = Env env_name } }
    ;

const_declaration:
    | VAL; a = assignment { Lang.{ name = a.variable_name; expression = a.expression; const = true } }
    ;

var_declaration:
    | VAR; a = assignment { Lang.{ name = a.variable_name; expression = a.expression; const = false } }
    ;
