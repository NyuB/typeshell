let exn_printers = [ Functions_spec.exn_printer; Assignments.exn_printer ]

let exn_print Lang.{ loc = { start_loc = sl; end_loc = _ }; item = e } =
  List.fold_left
    (fun acc printer ->
      match acc with
      | None ->
        Option.map
          (fun s -> Printf.sprintf "Fatal error (line %d): %s" sl.line s)
          (printer e)
      | Some s -> Some s)
    None
    exn_printers
  |> Option.value
       ~default:
         (Printf.sprintf "Fatal error: exception %s" (Printexc.to_string_default e))
;;

let default_print exn =
  Printf.sprintf "Fatal error: exception %s" (Printexc.to_string exn)
;;
