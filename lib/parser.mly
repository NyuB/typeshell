%token COLON
%token <string> DOLLAR_ID
%token EOF
%token EQ
%token <string> ID
%token <string> OPTION
%token SEPARATOR
%token <string> STRING_LITERAL
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
    | fc = function_call { Lang.lex_located ($startpos(fc)) ($endpos(fc)) fc }
    | a = assignment { Lang.lex_located ($startpos(a)) ($endpos(a)) (Lang.Assign a) }
    | d = declaration { Lang.lex_located ($startpos(d)) ($endpos(d)) (Lang.Declare d) }
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

arg_pass:
    | e = expression { Lang.(Raw e) }
    | label = ID; COLON; e = expression { Lang.(Labeled (label, e)) }
    | opt = OPTION { Lang.(OptionFlag opt) }
    | opt = OPTION; EQ; e = expression { Lang.(OptionKeyValue (opt, e)) }

function_call:
    | f = ID; el = arg_pass * { Lang.FCall(f, el) }
    ;
