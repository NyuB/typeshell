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

type program = command list
