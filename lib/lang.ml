type expr =
  | Env of string
  | Str of string

type declaration =
  { name : string
  ; expression : expr
  ; const : bool
  }

type assignment =
  { variable_name : string
  ; expression : expr
  }

type command =
  | Assign of assignment
  | Declare of declaration
  | Echo of string

type program = command list

module SMap = Map.Make (String)

exception KeyError of string
exception UndeclaredVariable of string
exception AlreadyDeclaredVariable of string
exception ReassignedConstant of string

module type Compiler = sig
  type env
  type output

  val interpret_program : env -> program -> output
end

type phase_env = unit

let phase_env : phase_env = ()

module type Phase = Compiler with type output := program and type env := phase_env

module Verifier : Phase = struct
  type variable_constraints = { const : bool }
  type context = variable_constraints SMap.t

  let interpret_command (ctxt : context) (command : command) : (context, exn) result =
    match command with
    | Declare { name; const; _ } ->
      (match SMap.find_opt name ctxt with
       | Some _ -> Error (AlreadyDeclaredVariable name)
       | _ -> Ok (SMap.add name { const } ctxt))
    | Assign { variable_name; _ } ->
      (match SMap.find_opt variable_name ctxt with
       | None -> Error (UndeclaredVariable variable_name)
       | Some { const = true } -> Error (ReassignedConstant variable_name)
       | Some { const = false } -> Ok ctxt)
    | Echo name ->
      (match SMap.find_opt name ctxt with
       | None -> Error (UndeclaredVariable name)
       | _ -> Ok ctxt)
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

module Interpreter :
  Compiler with type env := string -> string option and type output := unit = struct
  type context =
    { env : string -> string option
    ; variables : string SMap.t
    }

  let assign_unsafe name expr ctxt =
    match expr with
    | Str s ->
      let variables = SMap.add name s ctxt.variables in
      { ctxt with variables }
    | Env env_name ->
      (match ctxt.env env_name with
       | None -> raise (KeyError env_name)
       | Some value ->
         let variables = SMap.add name value ctxt.variables in
         { ctxt with variables })
  ;;

  let interpret_command ctxt cmd =
    match cmd with
    | Assign { variable_name = name; expression } | Declare { name; expression; _ } ->
      assign_unsafe name expression ctxt
    | Echo id ->
      (match SMap.find_opt id ctxt.variables with
       | None -> raise (KeyError id)
       | Some v ->
         let () = print_endline v in
         ctxt)
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
