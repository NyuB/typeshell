module SMap = Map.Make (String)
open Lang

type variable_constraints = { const : bool }
type context = variable_constraints SMap.t

exception AlreadyDeclaredVariable of string
exception ReassignedConstant of string
exception UndeclaredVariable of string

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
  | FCall (_, expr) ->
    List.fold_left
      (fun acc e ->
        match e with
        | Raw (Var var_name) | Labeled (_, Var var_name) | OptionKeyValue (_, Var var_name)
          -> Result.bind acc (fun _ -> check_declared ctxt var_name)
        | _ -> acc)
      (Ok ctxt)
      expr
;;

let interpret_program () (program : program) : program_result =
  let rec aux errors ctxt cmds =
    match errors, cmds with
    | [], [] -> Ok program
    | errs, [] -> Error (List.rev errs)
    | errs, cmd :: t ->
      (match interpret_command ctxt cmd with
       | Error e -> aux (e :: errs) ctxt t
       | Ok next_ctxt -> aux errs next_ctxt t)
  in
  aux [] SMap.empty program
;;

let exn_printer = function
  | UndeclaredVariable v -> Some (Printf.sprintf "Undeclared variable '%s'" v)
  | AlreadyDeclaredVariable v -> Some (Printf.sprintf "Already declared variable '%s'" v)
  | ReassignedConstant c -> Some (Printf.sprintf "Reassigned constant '%s'" c)
  | _ -> None
;;
