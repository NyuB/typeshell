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
  | InvalidLabel l -> Format.pp_print_string ppf (Printf.sprintf "InvalidLabel \"%s\"" l)
  | InvalidOption o ->
    Format.pp_print_string ppf (Printf.sprintf "InvalidOption \"%s\"" o)
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
  | Mismatch m ->
    Format.pp_print_list
      ~pp_sep:(fun p -> Format.pp_print_newline p)
      spec_mismatch_format
      ppf
      m
;;

let spec_match_testable = Alcotest.testable spec_match_format ( = )

let example_specification_too_many_args =
  ( "0 allowed, 1 passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = []; accept_varargs = false }
      in
      let args = [ Raw (Str "a") ] in
      Alcotest.(check spec_match_testable)
        "Expected too many arguments error"
        (Functions_spec.Mismatch
           [ Functions_spec.TooManyArguments { expected = 0; actual = 1 } ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_specification_not_enough_args =
  ( "2 required, 1 passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Positional; Positional ]; accept_varargs = false }
      in
      let args = [ Raw (Str "a") ] in
      Alcotest.(check spec_match_testable)
        "Expected missing argument error"
        (Functions_spec.Mismatch
           [ Functions_spec.MissingArgument { expected = 2; actual = 1 } ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_specification_no_arg_ok =
  ( "0 allowed, 0 passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = []; accept_varargs = false }
      in
      let args = [] in
      Alcotest.(check spec_match_testable)
        "Expected ok"
        (Functions_spec.Match_Ok [])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_specification_varargs_ok =
  ( "1 allowed with vararg, 2 passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Positional ]; accept_varargs = true }
      in
      let args = [ Raw (Str "a"); Raw (Str "b") ] in
      Alcotest.(check spec_match_testable)
        "Expected ok"
        (Functions_spec.Match_Ok [ Raw (Str "a"); Raw (Str "b") ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_specification_no_varargs_ok =
  ( "1 allowed with vararg, 1 passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Positional ]; accept_varargs = true }
      in
      let args = [ Raw (Str "a") ] in
      Alcotest.(check spec_match_testable)
        "Expected ok"
        (Functions_spec.Match_Ok [ Raw (Str "a") ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_specification_varargs_missing_required =
  ( "1 allowed with vararg, 0 passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Positional ]; accept_varargs = true }
      in
      let args = [] in
      Alcotest.(check spec_match_testable)
        "Expected missing argument error"
        (Functions_spec.Mismatch
           [ Functions_spec.MissingArgument { expected = 1; actual = 0 } ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_specification_label_accept_no_label =
  ( "1 labeled, 1 raw passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Labeled "label" ]; accept_varargs = true }
      in
      let args = [ Raw (Str "a") ] in
      Alcotest.(check spec_match_testable)
        "Expected ok"
        (Functions_spec.Match_Ok [ Raw (Str "a") ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_specification_wrong_label =
  ( "1 invalid label"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Labeled "label"; Positional ]; accept_varargs = true }
      in
      let args = [ Labeled ("wrong", Str "labeled"); Raw (Str "a") ] in
      Alcotest.(check spec_match_testable)
        "Expected invalid label error"
        (Functions_spec.Mismatch [ InvalidLabel "wrong" ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_specification_reorder_label =
  ( "reordered 2nd labeled arg to its 1st spec position"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Labeled "label"; Positional ]; accept_varargs = false }
      in
      let args = [ Raw (Str "a"); Labeled ("label", Str "labeled") ] in
      Alcotest.(check spec_match_testable)
        "Expected ok"
        (Functions_spec.Match_Ok [ Labeled ("label", Str "labeled"); Raw (Str "a") ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_vararg_after_reorder =
  ( "reordered label then vararg"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Labeled "label" ]; accept_varargs = true }
      in
      let args = [ Raw (Str "a"); Labeled ("label", Str "labeled") ] in
      Alcotest.(check spec_match_testable)
        "Expected reordered ok"
        (Functions_spec.Match_Ok [ Labeled ("label", Str "labeled"); Raw (Str "a") ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_invalid_vararg_after_reorder =
  ( "reordered label then invalid vararg"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Labeled "label" ]; accept_varargs = false }
      in
      let args = [ Raw (Str "a"); Labeled ("label", Str "labeled") ] in
      Alcotest.(check spec_match_testable)
        "Expected mismatch"
        (Functions_spec.Mismatch [ TooManyArguments { expected = 1; actual = 2 } ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_two_labels_in_reverse_order =
  ( "2 labels in reverse order"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Labeled "from"; Labeled "to" ]; accept_varargs = false }
      in
      let args = [ Labeled ("to", Str "target"); Labeled ("from", Str "source") ] in
      Alcotest.(check spec_match_testable)
        "Expected reordered ok"
        (Functions_spec.Match_Ok
           [ Labeled ("from", Str "source"); Labeled ("to", Str "target") ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_one_flag_option_no_arg =
  ( "one option, 0 arg passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Option (Flag "verbose") ]; accept_varargs = false }
      in
      let args = [] in
      Alcotest.(check spec_match_testable)
        "Expected ok"
        (Functions_spec.Match_Ok [])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_one_flag_option_option_passed =
  ( "one option, call with this option"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Option (Flag "verbose") ]; accept_varargs = false }
      in
      let args = [ OptionFlag "verbose" ] in
      Alcotest.(check spec_match_testable)
        "Expected ok"
        (Functions_spec.Match_Ok [ OptionFlag "verbose" ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_one_flag_option_invalid_option_passed =
  ( "one option, invalid option passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Option (Flag "verbose") ]; accept_varargs = false }
      in
      let args = [ OptionFlag "nounbose" ] in
      Alcotest.(check spec_match_testable)
        "Expected invalid option"
        (Functions_spec.Mismatch [ InvalidOption "nounbose" ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_one_flag_option_invalid_vararg_passed =
  ( "one option, invalid vararg passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Option (Flag "verbose") ]; accept_varargs = false }
      in
      let args = [ Raw (Str "var1") ] in
      Alcotest.(check spec_match_testable)
        "Expected invalid option"
        (Functions_spec.Mismatch [ TooManyArguments { expected = 0; actual = 1 } ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_one_flag_option_only_varargs_passed =
  ( "one option, only varargs passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Option (Flag "verbose") ]; accept_varargs = true }
      in
      let args = [ Raw (Str "var1"); Raw (Str "var2") ] in
      Alcotest.(check spec_match_testable)
        "Expected ok"
        (Functions_spec.Match_Ok [ Raw (Str "var1"); Raw (Str "var2") ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_two_options_only_one_passed =
  ( "Two options, only the last one passed"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Option (Flag "verbose"); Positional; Option (Flag "force") ]
        ; accept_varargs = false
        }
      in
      let args = [ Raw (Str "var1"); OptionFlag "force" ] in
      Alcotest.(check spec_match_testable)
        "Expected ok"
        (Functions_spec.Match_Ok [ OptionFlag "force"; Raw (Str "var1") ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_invalid_option_values =
  ( "one flag and one keyval spec, invalid call for each"
  , fun () ->
      let spec : Functions_spec.specification =
        { arguments = [ Option (Flag "verbose"); Option (WithValue "output") ]
        ; accept_varargs = false
        }
      in
      let args =
        [ OptionKeyValue ("verbose", Str "unexpected_value"); OptionFlag "output" ]
      in
      Alcotest.(check spec_match_testable)
        "Expected mismatch"
        (Functions_spec.Mismatch [ InvalidOption "verbose"; InvalidOption "output" ])
        (Functions_spec.spec_allow_call args spec) )
;;

let example_mixed_reversed =
  ( "Reverse passing order"
  , fun () ->
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
        (Functions_spec.spec_allow_call args spec) )
;;

let quick_test (name, test) = name, `Quick, test
let quick_tests tests = List.map quick_test tests

let () =
  Alcotest.run
    "Function specification"
    [ ( "Positionals"
      , quick_tests
          [ example_specification_too_many_args
          ; example_specification_not_enough_args
          ; example_specification_no_arg_ok
          ; example_specification_varargs_ok
          ; example_specification_no_varargs_ok
          ; example_two_labels_in_reverse_order
          ; example_specification_varargs_missing_required
          ] )
    ; ( "Labeled"
      , quick_tests
          [ example_specification_label_accept_no_label
          ; example_specification_wrong_label
          ; example_vararg_after_reorder
          ; example_invalid_vararg_after_reorder
          ; example_specification_reorder_label
          ] )
    ; ( "Options"
      , quick_tests
          [ example_one_flag_option_no_arg
          ; example_one_flag_option_only_varargs_passed
          ; example_one_flag_option_invalid_vararg_passed
          ; example_one_flag_option_invalid_option_passed
          ; example_one_flag_option_option_passed
          ; example_two_options_only_one_passed
          ; example_invalid_option_values
          ] )
    ; "Mixed examples", quick_tests [ example_mixed_reversed ]
    ]
;;
