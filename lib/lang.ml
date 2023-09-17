type expr =
  | Env of string
  | Str of string

type command =
  | Assign of (string * expr)
  | Echo of string

type program = command list

module SMap = Map.Make (String)

exception KeyError of string
exception UndeclaredVariable of string
exception ReassignedConstant of string

module type Compiler = sig
  type env
  type output

  val interpret_program : env -> program -> output
end

type verifier_env = unit

let verifier_env : verifier_env = ()

module Verifier : Compiler with type env := unit and type output := program = struct
  type variable_constraints = { const : bool }
  type context = variable_constraints SMap.t

  let interpret_command (ctxt : context) (command : command) : (context, exn) result =
    match command with
    | Assign (name, _) ->
      (match SMap.find_opt name ctxt with
       | Some { const = true } -> Error (ReassignedConstant name)
       | _ -> Ok (SMap.add name { const = true } ctxt))
    | Echo name ->
      (match SMap.find_opt name ctxt with
       | None -> Error (UndeclaredVariable name)
       | _ -> Ok ctxt)
  ;;

  let interpret_program (_ : verifier_env) (program : program) : program =
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

  let interpret_command ctxt cmd =
    match cmd with
    | Assign (name, Str value) ->
      let variables = SMap.add name value ctxt.variables in
      { ctxt with variables }
    | Assign (name, Env env_name) ->
      (match ctxt.env env_name with
       | None -> raise (KeyError env_name)
       | Some value ->
         let variables = SMap.add name value ctxt.variables in
         { ctxt with variables })
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
