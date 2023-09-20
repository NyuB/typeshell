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
    | commands = list(command_trailed); EOF { commands }
    ;

command_trailed:
    | c = command; SEPARATOR+ { c }
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
    | name = ID; EQ; s = STRING_LITERAL { Lang.{ name; expression = Str s } }
    | name = ID; EQ; env_name = DOLLAR_ID { Lang.{ name; expression = Env env_name } }
    | name = ID; EQ; var_name = ID { Lang.{ name; expression = Var var_name } }
    ;

const_declaration:
    | VAL; a = assignment { Lang.{ name = a.name; expression = a.expression; const = true } }
    ;

var_declaration:
    | VAR; a = assignment { Lang.{ name = a.name; expression = a.expression; const = false } }
    ;
