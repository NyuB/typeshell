open Typeshell.Lang
open Typeshell

let spec_mismatch_format ppf (s : Functions_spec.spec_mismatch) =
  match s with
  | TooManyArguments { expected; actual } ->
    Format.pp_print_string
      ppf
      (Printf.sprintf "TooManyArguments { expected = %d; actual = %d }" expected actual)
  | MissingArgument { expected; actual } ->
    Format.pp_print_string
      ppf
      (Printf.sprintf "MissingArgument { expected = %d; actual = %d }" expected actual)
  | InvalidLabel l ->
    Format.pp_print_string ppf "InvalidLabel [ ";
    Format.pp_print_list
      ~pp_sep:(fun p () -> Format.pp_print_string p ";")
      (fun p i -> Format.pp_print_string p i)
      ppf
      l;
    Format.pp_print_string ppf " ]"
  | InvalidOption o ->
    Format.pp_print_string ppf "InvalidOption [ ";
    Format.pp_print_list
      ~pp_sep:(fun p () -> Format.pp_print_string p ";")
      (fun p i -> Format.pp_print_string p i)
      ppf
      o;
    Format.pp_print_string ppf " ]"
;;

let spec_arg_format ppf (a : call_argument) =
  match a with
  | Labeled (l, _) -> Format.pp_print_string ppf (Printf.sprintf "%s:_" l)
  | Raw _ -> Format.pp_print_string ppf "_"
  | OptionFlag f -> Format.pp_print_string ppf (Printf.sprintf "-%s" f)
  | OptionKeyValue (k, _) -> Format.pp_print_string ppf (Printf.sprintf "-%s=_" k)
;;

let spec_match_format ppf (s : Functions_spec.spec_match) =
  match s with
  | Match_Ok args ->
    Format.pp_print_string ppf "Match_Ok [ ";
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
      spec_arg_format
      ppf
      args;
    Format.pp_print_string ppf " ]"
  | Mismatch m -> spec_mismatch_format ppf m
;;

let spec_match_testable = Alcotest.testable spec_match_format ( = )

let example_specification_too_many_args () =
  let spec : Functions_spec.specification = { arguments = []; accept_varargs = false } in
  let args = [ Raw (Str "a") ] in
  Alcotest.(check spec_match_testable)
    "Expected too many arguments error"
    (Functions_spec.Mismatch
       (Functions_spec.TooManyArguments { expected = 0; actual = 1 }))
    (Functions_spec.spec_allow_call args spec)
;;

let example_specification_not_enough_args () =
  let spec : Functions_spec.specification =
    { arguments = [ Positional; Positional ]; accept_varargs = false }
  in
  let args = [ Raw (Str "a") ] in
  Alcotest.(check spec_match_testable)
    "Expected missing argument error"
    (Functions_spec.Mismatch (Functions_spec.MissingArgument { expected = 2; actual = 1 }))
    (Functions_spec.spec_allow_call args spec)
;;

let example_specification_no_arg_ok () =
  let spec : Functions_spec.specification = { arguments = []; accept_varargs = false } in
  let args = [] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions_spec.Match_Ok [])
    (Functions_spec.spec_allow_call args spec)
;;

let example_specification_varargs_ok () =
  let spec : Functions_spec.specification =
    { arguments = [ Positional ]; accept_varargs = true }
  in
  let args = [ Raw (Str "a"); Raw (Str "b") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions_spec.Match_Ok [ Raw (Str "a"); Raw (Str "b") ])
    (Functions_spec.spec_allow_call args spec)
;;

let example_specification_no_varargs_ok () =
  let spec : Functions_spec.specification =
    { arguments = [ Positional ]; accept_varargs = true }
  in
  let args = [ Raw (Str "a") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions_spec.Match_Ok [ Raw (Str "a") ])
    (Functions_spec.spec_allow_call args spec)
;;

let example_specification_varargs_missing_required () =
  let spec : Functions_spec.specification =
    { arguments = [ Positional ]; accept_varargs = true }
  in
  let args = [] in
  Alcotest.(check spec_match_testable)
    "Expected missing argument error"
    (Functions_spec.Mismatch (Functions_spec.MissingArgument { expected = 1; actual = 0 }))
    (Functions_spec.spec_allow_call args spec)
;;

let example_specification_label_accept_no_label () =
  let spec : Functions_spec.specification =
    { arguments = [ Labeled "label" ]; accept_varargs = true }
  in
  let args = [ Raw (Str "a") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions_spec.Match_Ok [ Raw (Str "a") ])
    (Functions_spec.spec_allow_call args spec)
;;

let example_specification_wrong_label () =
  let spec : Functions_spec.specification =
    { arguments = [ Labeled "label"; Positional ]; accept_varargs = true }
  in
  let args = [ Labeled ("wrong", Str "labeled"); Raw (Str "a") ] in
  Alcotest.(check spec_match_testable)
    "Expected invalid label error"
    (Functions_spec.Mismatch (InvalidLabel [ "wrong" ]))
    (Functions_spec.spec_allow_call args spec)
;;

let example_specification_reorder_label () =
  let spec : Functions_spec.specification =
    { arguments = [ Labeled "label"; Positional ]; accept_varargs = false }
  in
  let args = [ Raw (Str "a"); Labeled ("label", Str "labeled") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions_spec.Match_Ok [ Labeled ("label", Str "labeled"); Raw (Str "a") ])
    (Functions_spec.spec_allow_call args spec)
;;

let example_vararg_after_reorder () =
  let spec : Functions_spec.specification =
    { arguments = [ Labeled "label" ]; accept_varargs = true }
  in
  let args = [ Raw (Str "a"); Labeled ("label", Str "labeled") ] in
  Alcotest.(check spec_match_testable)
    "Expected reordered ok"
    (Functions_spec.Match_Ok [ Labeled ("label", Str "labeled"); Raw (Str "a") ])
    (Functions_spec.spec_allow_call args spec)
;;

let example_invalid_vararg_after_reorder () =
  let spec : Functions_spec.specification =
    { arguments = [ Labeled "label" ]; accept_varargs = false }
  in
  let args = [ Raw (Str "a"); Labeled ("label", Str "labeled") ] in
  Alcotest.(check spec_match_testable)
    "Expected mismatch"
    (Functions_spec.Mismatch (TooManyArguments { expected = 1; actual = 2 }))
    (Functions_spec.spec_allow_call args spec)
;;

let example_two_labels_in_reverse_order () =
  let spec : Functions_spec.specification =
    { arguments = [ Labeled "from"; Labeled "to" ]; accept_varargs = false }
  in
  let args = [ Labeled ("to", Str "target"); Labeled ("from", Str "source") ] in
  Alcotest.(check spec_match_testable)
    "Expected reordered ok"
    (Functions_spec.Match_Ok
       [ Labeled ("from", Str "source"); Labeled ("to", Str "target") ])
    (Functions_spec.spec_allow_call args spec)
;;

let example_one_flag_option_no_arg () =
  let spec : Functions_spec.specification =
    { arguments = [ Option (Flag "verbose") ]; accept_varargs = false }
  in
  let args = [] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions_spec.Match_Ok [])
    (Functions_spec.spec_allow_call args spec)
;;

let example_one_flag_option_option_passed () =
  let spec : Functions_spec.specification =
    { arguments = [ Option (Flag "verbose") ]; accept_varargs = false }
  in
  let args = [ OptionFlag "verbose" ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions_spec.Match_Ok [ OptionFlag "verbose" ])
    (Functions_spec.spec_allow_call args spec)
;;

let example_one_flag_option_invalid_option_passed () =
  let spec : Functions_spec.specification =
    { arguments = [ Option (Flag "verbose") ]; accept_varargs = false }
  in
  let args = [ OptionFlag "nounbose" ] in
  Alcotest.(check spec_match_testable)
    "Expected invalid option"
    (Functions_spec.Mismatch (InvalidOption [ "nounbose" ]))
    (Functions_spec.spec_allow_call args spec)
;;

let example_one_flag_option_invalid_vararg_passed () =
  let spec : Functions_spec.specification =
    { arguments = [ Option (Flag "verbose") ]; accept_varargs = false }
  in
  let args = [ Raw (Str "var1") ] in
  Alcotest.(check spec_match_testable)
    "Expected invalid option"
    (Functions_spec.Mismatch (TooManyArguments { expected = 0; actual = 1 }))
    (Functions_spec.spec_allow_call args spec)
;;

let example_one_flag_option_only_varargs_passed () =
  let spec : Functions_spec.specification =
    { arguments = [ Option (Flag "verbose") ]; accept_varargs = true }
  in
  let args = [ Raw (Str "var1"); Raw (Str "var2") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions_spec.Match_Ok [ Raw (Str "var1"); Raw (Str "var2") ])
    (Functions_spec.spec_allow_call args spec)
;;

let example_invalid_value_for_flag () =
  let spec : Functions_spec.specification =
    { arguments = [ Option (Flag "verbose") ]; accept_varargs = false }
  in
  let args = [ OptionKeyValue ("verbose", Str "unexpected_value") ] in
  Alcotest.(check spec_match_testable)
    "Expected mismatch"
    (Functions_spec.Mismatch (InvalidOption [ "verbose" ]))
    (Functions_spec.spec_allow_call args spec)
;;

let example_mixed_reversed () =
  let spec : Functions_spec.specification =
    { arguments = [ Option (WithValue "verbose"); Labeled "label"; Positional ]
    ; accept_varargs = false
    }
  in
  let args =
    [ Raw (Str "var1")
    ; Labeled ("label", Str "labeled")
    ; OptionKeyValue ("verbose", Str "1")
    ]
  in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions_spec.Match_Ok
       [ OptionKeyValue ("verbose", Str "1")
       ; Labeled ("label", Str "labeled")
       ; Raw (Str "var1")
       ])
    (Functions_spec.spec_allow_call args spec)
;;

let () =
  Alcotest.run
    "Function specification"
    [ ( "Positionals"
      , [ "0 allowed, 1 passed", `Quick, example_specification_too_many_args
        ; "2 required, 1 passed", `Quick, example_specification_not_enough_args
        ; "0 allowed, 0 passed", `Quick, example_specification_no_arg_ok
        ; "1 allowed with vararg, 2 passed", `Quick, example_specification_varargs_ok
        ; "1 allowed with vararg, 1 passed", `Quick, example_specification_no_varargs_ok
        ; "2 labels in reverse order", `Quick, example_two_labels_in_reverse_order
        ; ( "1 allowed with vararg, 0 passed"
          , `Quick
          , example_specification_varargs_missing_required )
        ; ( "1 allowed with vararg, 0 passed"
          , `Quick
          , example_specification_varargs_missing_required )
        ] )
    ; ( "Labeled"
      , [ "1 labeled, 1 raw passed", `Quick, example_specification_label_accept_no_label
        ; "1 invalid label", `Quick, example_specification_wrong_label
        ; "reordered label then vararg", `Quick, example_vararg_after_reorder
        ; ( "reordered label then invalid vararg"
          , `Quick
          , example_invalid_vararg_after_reorder )
        ; ( "reordered 2nd labeled arg to its 1st spec position"
          , `Quick
          , example_specification_reorder_label )
        ] )
    ; ( "Options"
      , [ "one option, 0 arg passed", `Quick, example_one_flag_option_no_arg
        ; ( "one option, only varargs passed"
          , `Quick
          , example_one_flag_option_only_varargs_passed )
        ; ( "one option, invalid vararg passed"
          , `Quick
          , example_one_flag_option_invalid_vararg_passed )
        ; ( "one option, invalid option passed"
          , `Quick
          , example_one_flag_option_invalid_option_passed )
        ; ( "one option, call with this option"
          , `Quick
          , example_one_flag_option_option_passed )
        ; ( "one flag option, call with invalid value"
          , `Quick
          , example_invalid_value_for_flag )
        ] )
    ; "Mixed examples", [ "Reverse passing order", `Quick, example_mixed_reversed ]
    ]
;;
