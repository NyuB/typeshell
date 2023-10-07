open Tea

module IDE : App = struct
  type model =
    { dimensions : View.dimensions
    ; char_size : View.dimensions
    ; text : Text.t
    ; transpiled : Text.t
    ; errors : Text.t
    }

  let width model = model.dimensions.width
  let height model = model.dimensions.height
  let text_y_base model = height model * 14 / 15
  let errors_y_base model = height model * 3 / 15
  let transpiled_x model = (width model / 2) + 1

  let display ?(start_x : int = 0) ~(start_y : int) ~(char_height : int) text =
    Text.map_lines
      (fun line s ->
        View.(Text (s, { position = pos start_x (start_y - (line * char_height)) })))
      text
  ;;

  let cursor_position model =
    let cursor = Text.cursor model.text in
    View.
      { position =
          pos
            (cursor.col * model.char_size.width)
            (text_y_base model - (cursor.line * model.char_size.height))
      }
  ;;

  let cursor = "_"

  type cursorMoveDirection =
    | Left
    | Right

  type message =
    | UpdateDimensions of View.dimensions
    | CharTyped of char
    | CharResized of View.dimensions
    | CursorMove of cursorMoveDirection

  let subscribes = function
    | Subscription.Resize d -> Some (UpdateDimensions d)
    | Subscription.KeyPressed k -> Some (CharTyped k)
    | Subscription.CharResize d -> Some (CharResized d)
    | Subscription.Arrow Left -> Some (CursorMove Left)
    | Subscription.Arrow Right -> Some (CursorMove Right)
    | _ -> None
  ;;

  let init =
    ( { dimensions = View.dim_zero
      ; char_size = View.dim_zero
      ; text = Text.empty
      ; transpiled = Text.empty
      ; errors = Text.empty
      }
    , subscribes )
  ;;

  let result_bind f r = Result.bind r f

  let exn_printers =
    [ Typeshell.Functions_spec.exn_printer; Typeshell.Assignments.exn_printer ]
  ;;

  let exn_print e =
    List.fold_left
      (fun acc printer ->
        match acc with
        | None -> Option.map (fun s -> Printf.sprintf "Fatal error: %s" s) (printer e)
        | Some s -> Some s)
      None
      exn_printers
    |> Option.value
         ~default:
           (Printf.sprintf "Fatal error: exception %s" (Printexc.to_string_default e))
  ;;

  let with_text text model =
    let lexbuf = Lexing.from_string (Text.to_string text) in
    try
      match
        Typeshell.Parser.prog Typeshell.Lexer.read lexbuf
        |> Typeshell.Assignments.interpret_program ()
        |> result_bind
             (Typeshell.Compiler.Function_Calls.interpret_program
                Typeshell.Compiler.BashStdLib.stdlib)
        |> Result.map (Typeshell.Compiler.Bash.interpret_program ())
        |> Result.map Text.of_list
      with
      | Ok transpiled -> { model with text; transpiled; errors = Text.empty }
      | Error exns -> { model with text; errors = Text.of_list (List.map exn_print exns) }
    with
    | exn -> { model with text; errors = Text.of_list [ exn_print exn ] }
  ;;

  let update model = function
    | UpdateDimensions dimensions -> { model with dimensions }, subscribes
    | CharTyped '\n' | CharTyped '\r' ->
      with_text (Text.newline model.text) model, subscribes
    | CharTyped '\008' -> with_text (Text.del model.text) model, subscribes
    | CharTyped c -> with_text (Text.add c model.text) model, subscribes
    | CharResized char_size -> { model with char_size }, subscribes
    | CursorMove Left -> with_text (Text.cursor_left model.text) model, subscribes
    | CursorMove Right -> with_text (Text.cursor_right model.text) model, subscribes
  ;;

  let view model =
    let middle_x = width model / 2
    and error_line_y = errors_y_base model + model.char_size.height in
    View.
      [ Line ({ x = middle_x; y = error_line_y }, { x = middle_x; y = height model })
      ; Line ({ x = 0; y = error_line_y }, { x = width model; y = error_line_y })
      ]
    @ display ~start_y:(text_y_base model) ~char_height:model.char_size.height model.text
    @ display
        ~start_y:(text_y_base model)
        ~start_x:(transpiled_x model)
        ~char_height:model.char_size.height
        model.transpiled
    @ View.
        [ Colored
            ( Red
            , display
                ~start_y:(errors_y_base model)
                ~char_height:model.char_size.height
                model.errors )
        ]
    @ View.[ Colored (Red, [ Text (cursor, cursor_position model) ]) ]
  ;;
end
