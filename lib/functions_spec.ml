open Lang
module SMap = Map.Make (String)

type option_specification =
  | Flag of string
  | WithValue of string

type arg_specification =
  | Labeled of string
  | Option of option_specification
  | Positional

type specification =
  { arguments : arg_specification list
  ; accept_varargs : bool
  }

let non_option_arg_count args =
  List.length
    (List.filter
       (function
        | OptionFlag _ | OptionKeyValue _ -> false
        | _ -> true)
       args)
;;

let positional_count specification =
  List.length
    (List.filter
       (function
        | Option _ -> false
        | _ -> true)
       specification.arguments)
;;

type arg_mismatch =
  { expected : int
  ; actual : int
  }

type spec_mismatch =
  | MissingArgument of arg_mismatch
  | TooManyArguments of arg_mismatch
  | InvalidLabel of string
  | InvalidOption of string

type spec_match =
  | Mismatch of spec_mismatch list
  | Match_Ok of call_argument list

exception UndeclaredFunction of string
exception SpecMismatch of spec_mismatch

type library = specification SMap.t

let ok = Ok ()

module Arg_Reordering : sig
  (** [ reordered_arg_list args spec ] returns an array containing the same elements as [args] with the labeled arguments reordered to match [spec.arguments].
        
    Unlabeled and positional arguments are inserted from the beginning of the resulting array on the positions left free after labeled arguments positioning.
        
    All labeled and named options in [args] are assumed to be present in [spec.arguments] *)
  val reordered_arg_list : call_argument list -> specification -> call_argument list
end = struct
  let find_index item arr =
    let res = ref None in
    Array.iteri (fun i e -> if e = item then res := Some i) arr;
    !res
  ;;

  let find_index_unsafe item arr = Option.get (find_index item arr)

  let insert_at_first_empty_slot item arr =
    let inserted = ref false
    and index = ref 0 in
    while not !inserted do
      match arr.(!index) with
      | None ->
        arr.(!index) <- Some item;
        inserted := true
      | Some _ -> index := !index + 1
    done
  ;;

  let insert_labels args spec arr =
    let spec_arr = Array.of_list spec.arguments in
    List.iter
      (fun (a : call_argument) ->
        match a with
        | Labeled (label, _) -> arr.(find_index_unsafe (Labeled label) spec_arr) <- Some a
        | _ -> ())
      args
  ;;

  let insert_options args spec arr =
    let spec_arr = Array.of_list spec.arguments in
    List.iter
      (fun (a : call_argument) ->
        match a with
        | OptionFlag f -> arr.(find_index_unsafe (Option (Flag f)) spec_arr) <- Some a
        | OptionKeyValue (k, _) ->
          arr.(find_index_unsafe (Option (WithValue k)) spec_arr) <- Some a
        | _ -> ())
      args
  ;;

  let insert_unlabeled args arr =
    List.iter
      (fun (a : call_argument) ->
        match a with
        | Labeled _ | OptionFlag _ | OptionKeyValue _ -> ()
        | _ -> insert_at_first_empty_slot a arr)
      args
  ;;

  let reordered_arg_list (args : call_argument list) (spec : specification)
    : call_argument list
    =
    let result_arr = Array.make (List.length args) None in
    insert_labels args spec result_arr;
    insert_options args spec result_arr;
    insert_unlabeled args result_arr;
    result_arr |> Array.map Option.get |> Array.to_list
  ;;
end

let invalid_labels labels = List.map (fun label -> InvalidLabel label) labels

let check_labels args spec =
  match
    List.filter_map
      (fun (a : call_argument) ->
        match a with
        | Labeled (l, _) ->
          (match List.find_opt (( = ) (Labeled l)) spec with
           | None -> Some l
           | Some _ -> None)
        | _ -> None)
      args
  with
  | [] -> ok
  | labels -> Error (invalid_labels labels)
;;

let invalid_options opts = List.map (fun opt -> InvalidOption opt) opts

let check_options args spec =
  match
    List.filter_map
      (function
       | OptionFlag f ->
         (match List.find_opt (( = ) (Option (Flag f))) spec with
          | None -> Some f
          | Some _ -> None)
       | OptionKeyValue (k, _) ->
         (match List.find_opt (( = ) (Option (WithValue k))) spec with
          | None -> Some k
          | Some _ -> None)
       | _ -> None)
      args
  with
  | [] -> ok
  | opts -> Error (invalid_options opts)
;;

let allow_arg_count args spec =
  let arglen = non_option_arg_count args in
  let pos_spec = positional_count spec in
  if arglen < pos_spec
  then Mismatch [ MissingArgument { expected = pos_spec; actual = arglen } ]
  else (
    let no_varargs = arglen - pos_spec = 0 in
    if no_varargs || spec.accept_varargs
    then Match_Ok args
    else Mismatch [ TooManyArguments { expected = pos_spec; actual = arglen } ])
;;

let spec_allow_call args spec =
  match check_labels args spec.arguments, check_options args spec.arguments with
  | Ok (), Ok () ->
    let reordered = Arg_Reordering.reordered_arg_list args spec in
    allow_arg_count reordered spec
  | Error err_labels, Ok () -> Mismatch err_labels
  | Ok (), Error err_options -> Mismatch err_options
  | Error err_labels, Error err_options -> Mismatch (err_labels @ err_options)
;;

let library_allow_call f args lib =
  match SMap.find_opt f lib with
  | None -> Error [ UndeclaredFunction f ]
  | Some spec ->
    (match spec_allow_call args spec with
     | Match_Ok args -> Ok args
     | Mismatch mismatches ->
       Error (List.map (fun mismatch -> SpecMismatch mismatch) mismatches))
;;

let exn_printer e =
  match e with
  | SpecMismatch (InvalidLabel l) -> Some (Printf.sprintf "Invalid label '%s'" l)
  | SpecMismatch (InvalidOption o) -> Some (Printf.sprintf "Invalid option '%s'" o)
  | SpecMismatch (MissingArgument { expected; actual }) ->
    Some
      (Printf.sprintf
         "Missing argument, expected %d arguments but got %d"
         expected
         actual)
  | SpecMismatch (TooManyArguments { expected; actual }) ->
    Some
      (Printf.sprintf
         "Too many arguments, expected %d arguments but got %d"
         expected
         actual)
  | UndeclaredFunction f -> Some (Printf.sprintf "Undeclared function '%s'" f)
  | _ -> None
;;
