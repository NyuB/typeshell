open Typeshell.Lang

let spec_mismatch_format ppf (s : Functions.spec_mismatch) =
  match s with
  | TooManyArguments { expected; actual } ->
    Format.pp_print_string
      ppf
      (Printf.sprintf "TooManyArguments { expected = %d actual = %d }" expected actual)
  | MissingArgument { expected; actual } ->
    Format.pp_print_string
      ppf
      (Printf.sprintf "MissingArgument { expected = %d actual = %d }" expected actual)
;;

let spec_arg_format ppf (a : call_argument) =
  match a with
  | Labeled (l, _) -> Format.pp_print_string ppf (Printf.sprintf "%s: _" l)
  | Raw _ -> Format.pp_print_string ppf "_"
  | OptionFlag f -> Format.pp_print_string ppf (Printf.sprintf "-%s" f)
  | OptionKeyValue (k, _) -> Format.pp_print_string ppf (Printf.sprintf "-%s=_" k)
;;

let spec_match_format ppf (s : Functions.spec_match) =
  match s with
  | Match_Ok args ->
    Format.pp_print_string ppf "Match_Ok [ ";
    Format.pp_print_list spec_arg_format ppf args;
    Format.pp_print_string ppf " ]"
  | Mismatch m -> spec_mismatch_format ppf m
;;

let spec_match_testable = Alcotest.testable spec_match_format ( = )

let example_specification_too_many_args () =
  let spec : Functions.specification = { arguments = []; accept_varargs = false } in
  let args = [ Raw (Str "a") ] in
  Alcotest.(check spec_match_testable)
    "Expected too many arguments error"
    (Functions.Mismatch (Functions.TooManyArguments { expected = 0; actual = 1 }))
    (Functions.spec_allow_call args spec)
;;

let example_specification_not_enough_args () =
  let spec : Functions.specification =
    { arguments = [ Positional; Positional ]; accept_varargs = false }
  in
  let args = [ Raw (Str "a") ] in
  Alcotest.(check spec_match_testable)
    "Expected missing argument error"
    (Functions.Mismatch (Functions.MissingArgument { expected = 2; actual = 1 }))
    (Functions.spec_allow_call args spec)
;;

let example_specification_no_arg_ok () =
  let spec : Functions.specification = { arguments = []; accept_varargs = false } in
  let args = [] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions.Match_Ok [])
    (Functions.spec_allow_call args spec)
;;

let example_specification_varargs_ok () =
  let spec : Functions.specification =
    { arguments = [ Positional ]; accept_varargs = true }
  in
  let args = [ Raw (Str "a"); Raw (Str "b") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions.Match_Ok [ Raw (Str "a"); Raw (Str "b") ])
    (Functions.spec_allow_call args spec)
;;

let example_specification_no_varargs_ok () =
  let spec : Functions.specification =
    { arguments = [ Positional ]; accept_varargs = true }
  in
  let args = [ Raw (Str "a") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions.Match_Ok [ Raw (Str "a") ])
    (Functions.spec_allow_call args spec)
;;

let example_specification_varargs_missing_required () =
  let spec : Functions.specification =
    { arguments = [ Positional ]; accept_varargs = true }
  in
  let args = [] in
  Alcotest.(check spec_match_testable)
    "Expected missing argument error"
    (Functions.Mismatch (Functions.MissingArgument { expected = 1; actual = 0 }))
    (Functions.spec_allow_call args spec)
;;

let () =
  Alcotest.run
    "Typeshell language"
    [ ( "Function specification"
      , [ "0 allowed, 1 passed", `Quick, example_specification_too_many_args
        ; "2 required, 1 passed", `Quick, example_specification_not_enough_args
        ; "0 allowed, 0 passed", `Quick, example_specification_no_arg_ok
        ; "1 allowed with vararg, 2 passed", `Quick, example_specification_varargs_ok
        ; "1 allowed with vararg, 1 passed", `Quick, example_specification_no_varargs_ok
        ; ( "1 allowed with vararg, 0 passed"
          , `Quick
          , example_specification_varargs_missing_required )
        ] )
    ]
;;
