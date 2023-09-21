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

let spec_match_format ppf (s : Functions.spec_match) =
  match s with
  | Match_Ok -> Format.pp_print_string ppf "Match_Ok"
  | Mismatch m -> spec_mismatch_format ppf m
;;

let spec_match_testable = Alcotest.testable spec_match_format ( = )

let example_specification_too_many_args () =
  let spec : Functions.specification = { positional_count = 0; accept_varargs = false } in
  let args = [ Str "a" ] in
  Alcotest.(check spec_match_testable)
    "Expected too many arguments error"
    (Functions.Mismatch (Functions.TooManyArguments { expected = 0; actual = 1 }))
    (Functions.spec_allow_call args spec)
;;

let example_specification_not_enough_args () =
  let spec : Functions.specification = { positional_count = 2; accept_varargs = false } in
  let args = [ Str "a" ] in
  Alcotest.(check spec_match_testable)
    "Expected too many arguments error"
    (Functions.Mismatch (Functions.MissingArgument { expected = 2; actual = 1 }))
    (Functions.spec_allow_call args spec)
;;

let () =
  Alcotest.run
    "Typeshell language"
    [ ( "Function specification"
      , [ "0 allowed, 1 passed", `Quick, example_specification_too_many_args
        ; "2 required, 1 passed", `Quick, example_specification_not_enough_args
        ] )
    ]
;;
