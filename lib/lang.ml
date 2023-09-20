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

type command =
  | Assign of assignment
  | Declare of declaration
  | Echo of string
  | FCall of string * expr list

type program = command list

module SMap = Map.Make (String)

exception UndeclaredVariable of string

module type Compiler = sig
  type env
  type output

  val interpret_program : env -> program -> output
end

type phase_env = unit

let phase_env : phase_env = ()

module type Phase = Compiler with type output := program and type env := phase_env

module Assignments : Phase = struct
  type variable_constraints = { const : bool }
  type context = variable_constraints SMap.t

  exception AlreadyDeclaredVariable of string
  exception ReassignedConstant of string

  let check_declared ctxt name =
    match SMap.find_opt name ctxt with
    | Some _ -> Ok ctxt
    | None -> Error (UndeclaredVariable name)
  ;;

  let declare ctxt name const =
    match SMap.find_opt name ctxt with
    | Some _ -> Error (AlreadyDeclaredVariable name)
    | _ -> Ok (SMap.add name { const } ctxt)
  ;;

  let check_assignable ctxt name =
    match SMap.find_opt name ctxt with
    | None -> Error (UndeclaredVariable name)
    | Some { const = true } -> Error (ReassignedConstant name)
    | Some { const = false } -> Ok ctxt
  ;;

  let interpret_command (ctxt : context) (command : command) : (context, exn) result =
    match command with
    | Declare { name; const; expression = Var var_name } ->
      let declared = check_declared ctxt var_name in
      Result.bind declared (fun ctxt -> declare ctxt name const)
    | Declare { name; const; _ } -> declare ctxt name const
    | Assign { name; expression = Var var_name } ->
      let declared = check_declared ctxt var_name in
      Result.bind declared (fun ctxt -> check_assignable ctxt name)
    | Assign { name; _ } -> check_assignable ctxt name
    | Echo name ->
      (match SMap.find_opt name ctxt with
       | None -> Error (UndeclaredVariable name)
       | _ -> Ok ctxt)
    | FCall (_, expr) ->
      List.fold_left
        (fun acc e ->
          match e with
          | Var var_name -> Result.bind acc (fun _ -> check_declared ctxt var_name)
          | _ -> acc)
        (Ok ctxt)
        expr
  ;;

  let interpret_program (_ : phase_env) (program : program) : program =
    let rec aux ctxt = function
      | [] -> program
      | cmd :: t ->
        (match interpret_command ctxt cmd with
         | Error e -> raise e
         | Ok next_ctxt -> aux next_ctxt t)
    in
    aux SMap.empty program
  ;;
end

module Bash : Compiler with type env := phase_env and type output := string list = struct
  let expr_repr = function
    | Env env_name -> Printf.sprintf "\"${%s:?\"Null environment variable\"}\"" env_name
    | Str str -> Printf.sprintf "'%s'" str
    | Var var_name -> Printf.sprintf "\"${%s}\"" var_name
  ;;

  let transpile_assign name expr = Printf.sprintf "%s=%s" name (expr_repr expr)

  let transpile_command cmd =
    match cmd with
    | Assign { name; expression } | Declare { name; expression; const = false } ->
      transpile_assign name expression
    | Declare { name; expression; const = true } ->
      Printf.sprintf "declare -r %s" (transpile_assign name expression)
    | Echo name -> Printf.sprintf "echo \"${%s}\"" name
    | FCall (f, args) ->
      Printf.sprintf "%s %s" f (String.concat " " (List.map expr_repr args))
  ;;

  let interpret_program _ program =
    "#!/bin/bash" :: "" :: List.map transpile_command program
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
    match cmd with
    | Assign { name; expression } | Declare { name; expression; _ } ->
      assign_unsafe name expression ctxt
    | Echo id ->
      (match SMap.find_opt id ctxt.variables with
       | None -> raise (UndeclaredVariable id)
       | Some v ->
         let () = print_endline v in
         ctxt)
    | FCall (f, args) ->
      let () =
        print_endline
        @@ Printf.sprintf
             "%s(%s)"
             f
             (String.concat ", " (List.map (value_unsafe ctxt) args))
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
