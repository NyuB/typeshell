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
  | InvalidLabel l -> Format.pp_print_string ppf (Printf.sprintf "InvalidLabel %s" l)
;;

let spec_arg_format ppf (a : call_argument) =
  match a with
  | Labeled (l, _) -> Format.pp_print_string ppf (Printf.sprintf "%s:_" l)
  | Raw _ -> Format.pp_print_string ppf "_"
  | OptionFlag f -> Format.pp_print_string ppf (Printf.sprintf "-%s" f)
  | OptionKeyValue (k, _) -> Format.pp_print_string ppf (Printf.sprintf "-%s=_" k)
;;

let spec_match_format ppf (s : Functions.spec_match) =
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

let example_specification_label_accept_no_label () =
  let spec : Functions.specification =
    { arguments = [ Labeled "label" ]; accept_varargs = true }
  in
  let args = [ Raw (Str "a") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions.Match_Ok [ Raw (Str "a") ])
    (Functions.spec_allow_call args spec)
;;

let example_specification_wrong_label () =
  let spec : Functions.specification =
    { arguments = [ Labeled "label"; Positional ]; accept_varargs = true }
  in
  let args = [ Labeled ("wrong", Str "labeled"); Raw (Str "a") ] in
  Alcotest.(check spec_match_testable)
    "Expected invalid label error"
    (Functions.Mismatch (InvalidLabel "wrong"))
    (Functions.spec_allow_call args spec)
;;

let example_specification_reorder_label () =
  let spec : Functions.specification =
    { arguments = [ Labeled "label"; Positional ]; accept_varargs = false }
  in
  let args = [ Raw (Str "a"); Labeled ("label", Str "labeled") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions.Match_Ok [ Labeled ("label", Str "labeled"); Raw (Str "a") ])
    (Functions.spec_allow_call args spec)
;;

let example_vararg_after_reorder () =
  let spec : Functions.specification =
    { arguments = [ Labeled "label" ]; accept_varargs = true }
  in
  let args = [ Raw (Str "a"); Labeled ("label", Str "labeled") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions.Match_Ok [ Labeled ("label", Str "labeled"); Raw (Str "a") ])
    (Functions.spec_allow_call args spec)
;;

let example_invalid_vararg_after_reorder () =
  let spec : Functions.specification =
    { arguments = [ Labeled "label" ]; accept_varargs = false }
  in
  let args = [ Raw (Str "a"); Labeled ("label", Str "labeled") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions.Mismatch (TooManyArguments { expected = 1; actual = 2 }))
    (Functions.spec_allow_call args spec)
;;

let example_two_labels_in_reverse_order () =
  let spec : Functions.specification =
    { arguments = [ Labeled "from"; Labeled "to" ]; accept_varargs = false }
  in
  let args = [ Labeled ("to", Str "target"); Labeled ("from", Str "source") ] in
  Alcotest.(check spec_match_testable)
    "Expected ok"
    (Functions.Match_Ok [ Labeled ("from", Str "source"); Labeled ("to", Str "target") ])
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
        ; "2 labels in reverse order", `Quick, example_two_labels_in_reverse_order
        ; ( "1 allowed with vararg, 0 passed"
          , `Quick
          , example_specification_varargs_missing_required )
        ; ( "1 allowed with vararg, 0 passed"
          , `Quick
          , example_specification_varargs_missing_required )
        ; "1 labeled, 1 raw passed", `Quick, example_specification_label_accept_no_label
        ; "1 invalid label", `Quick, example_specification_wrong_label
        ; "reordered label then vararg", `Quick, example_vararg_after_reorder
        ; ( "reordered label then invalid vararg"
          , `Quick
          , example_invalid_vararg_after_reorder )
        ; ( "reordered 2nd labeled arg to its 1st spec position"
          , `Quick
          , example_specification_reorder_label )
        ] )
    ]
;;
