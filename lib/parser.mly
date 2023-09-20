%token <string> DOLLAR_ID
%token EQ
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
    | fc = function_call { let f,a = fc in FCall (f,a) }
    | a = assignment { Assign a }
    | d = declaration { Declare d }
    ;

declaration:
    | cd = const_declaration { cd }
    | vd = var_declaration { vd }
    ;

assignment:
    | name = ID; EQ; e = expression { Lang.{ name; expression = e } }
    ;

const_declaration:
    | VAL; a = assignment { Lang.{ name = a.name; expression = a.expression; const = true } }
    ;

var_declaration:
    | VAR; a = assignment { Lang.{ name = a.name; expression = a.expression; const = false } }
    ;

expression:
    | s = STRING_LITERAL { Lang.(Str s) }
    | env_name = DOLLAR_ID { Lang.(Env env_name) }
    | var_name = ID { Lang.(Var var_name) }
    ;

function_call:
    | f = ID; el = expression * { (f, el) }
    ;
