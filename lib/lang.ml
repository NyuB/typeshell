type expr =
  | Env of string
  | Str of string
  | Var of string

type declaration =
  { name : string
  ; expression : expr
  ; const : bool
  }

type assignment =
  { name : string
  ; expression : expr
  }

type call_argument =
  | Labeled of string * expr
  | OptionKeyValue of string * expr
  | OptionFlag of string
  | Raw of expr

type command =
  | Assign of assignment
  | Declare of declaration
  | FCall of string * call_argument list

type file_location = { line : int }

type location =
  { start_loc : file_location
  ; end_loc : file_location
  }

type 'a located =
  { loc : location
  ; item : 'a
  }

let located loc item = { loc; item }

let lex_located (start_loc : Lexing.position) (end_loc : Lexing.position) item =
  { loc =
      { start_loc = { line = start_loc.pos_lnum }; end_loc = { line = end_loc.pos_lnum } }
  ; item
  }
;;

type located_command = command located
type program = located_command list
type program_result = (program, exn located list) result
