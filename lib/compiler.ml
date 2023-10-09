open Lang
module SMap = Map.Make (String)

module type Compiler = sig
  type env
  type output

  val interpret_program : env -> program -> output
end

module type StandardLibrary = sig
  val stdlib : Functions_spec.library
end

module BashStdLib : StandardLibrary = struct
  let stdlib =
    Functions_spec.
      [ "echo", { arguments = []; options = []; accept_varargs = true }
      ; ( "grep"
        , { arguments = [ Positional; Positional ]
          ; options =
              [ Flag "-v"; WithValue "--after-context"; WithValue "--before-context" ]
          ; accept_varargs = true
          } )
      ; ( "cp"
        , { arguments = [ Labeled "from"; Labeled "to" ]
          ; options = []
          ; accept_varargs = false
          } )
      ]
    |> List.to_seq
    |> SMap.of_seq
  ;;
end

module Function_Calls = struct
  let interpret_program (stdlib : Functions_spec.library) (program : program)
    : program_result
    =
    let rec aux acc errors commands =
      match errors, commands with
      | [], [] -> Ok (List.rev acc)
      | errs, [] -> Error (List.rev errs)
      | errs, { loc; item = FCall (f, args) } :: t ->
        (match Functions_spec.library_allow_call f args stdlib with
         | Ok args ->
           let reordered_fcall = FCall (f, args) in
           aux ({ loc; item = reordered_fcall } :: acc) errs t
         | Error sl -> aux acc (List.rev_append (List.map (located loc) sl) errs) t)
      | errs, cmd :: t -> aux (cmd :: acc) errs t
    in
    aux [] [] program
  ;;
end

module Bash : Compiler with type env := unit and type output := string list = struct
  let shebang = "#!/bin/bash"
  let fail_fast_flags = "set -e"
  let blank_line = ""

  let expr_repr = function
    | Env env_name -> Printf.sprintf "\"${%s:?\"Null environment variable\"}\"" env_name
    | Str str -> Printf.sprintf "'%s'" str
    | Var var_name -> Printf.sprintf "\"${%s}\"" var_name
  ;;

  let arg_repr = function
    | Raw e | Labeled (_, e) -> expr_repr e
    | OptionKeyValue (k, v) -> Printf.sprintf "%s=%s" k (expr_repr v)
    | OptionFlag f -> f
  ;;

  let transpile_assign name expr = Printf.sprintf "%s=%s" name (expr_repr expr)

  let transpile_command cmd =
    match cmd.item with
    | Assign { name; expression } | Declare { name; expression; const = false } ->
      transpile_assign name expression
    | Declare { name; expression; const = true } ->
      Printf.sprintf "declare -r %s" (transpile_assign name expression)
    | FCall (f, args) ->
      Printf.sprintf "%s %s" f (String.concat " " (List.map arg_repr args))
  ;;

  let interpret_program _ program =
    shebang :: fail_fast_flags :: blank_line :: List.map transpile_command program
  ;;
end

module Interpreter :
  Compiler with type env := string -> string option and type output := unit = struct
  type context =
    { env : string -> string option
    ; variables : string SMap.t
    }

  exception NullEnvironmentVariable of string

  let env_unsafe ctxt env_name =
    match ctxt.env env_name with
    | None | Some "" -> raise (NullEnvironmentVariable env_name)
    | Some value -> value
  ;;

  let value_unsafe ctxt = function
    | Str s -> s
    | Var var_name -> SMap.find var_name ctxt.variables
    | Env env_name -> env_unsafe ctxt env_name
  ;;

  let argument_value_unsafe ctxt = function
    | OptionFlag f -> f
    | OptionKeyValue (k, v) -> Printf.sprintf "%s=%s" k (value_unsafe ctxt v)
    | Raw e | Labeled (_, e) -> value_unsafe ctxt e
  ;;

  let assign_unsafe name expr ctxt =
    match expr with
    | Str s ->
      let variables = SMap.add name s ctxt.variables in
      { ctxt with variables }
    | Env env_name ->
      let value = env_unsafe ctxt env_name in
      let variables = SMap.add name value ctxt.variables in
      { ctxt with variables }
    | Var var_name ->
      let variables = SMap.add name (SMap.find var_name ctxt.variables) ctxt.variables in
      { ctxt with variables }
  ;;

  let interpret_command ctxt cmd =
    match cmd.item with
    | Assign { name; expression } | Declare { name; expression; _ } ->
      assign_unsafe name expression ctxt
    | FCall ("echo", expr) ->
      let () =
        print_endline (String.concat " " (List.map (argument_value_unsafe ctxt) expr))
      in
      ctxt
    | FCall (f, args) ->
      let () =
        print_endline
        @@ Printf.sprintf
             "%s(%s)"
             f
             (String.concat ", " (List.map (argument_value_unsafe ctxt) args))
      in
      ctxt
  ;;

  let interpret_program env program =
    let rec aux ctxt = function
      | [] -> ()
      | cmd :: t ->
        let next_ctxt = interpret_command ctxt cmd in
        aux next_ctxt t
    in
    aux { env; variables = SMap.empty } program
  ;;
end
