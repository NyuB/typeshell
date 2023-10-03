open Tea

module IDE : App = struct
  module Text : sig
    type t

    type cursor =
      { line : int
      ; col : int
      }

    val of_list : string list -> t
    val to_string : t -> string
    val del : t -> t
    val add : char -> t -> t
    val newline : t -> t
    val map_lines : (int -> string -> 'a) -> t -> 'a list
    val cursor_left : t -> t
    val cursor_right : t -> t
    val cursor : t -> cursor
    val empty : t
  end = struct
    type cursor =
      { line : int
      ; col : int
      }

    type t =
      { lines : string list
      ; cursor : cursor
      }

    let cursor t = t.cursor
    let make_cursor line col = { line; col }
    let empty = { lines = []; cursor = make_cursor 0 0 }
    let of_list l = { lines = List.rev l; cursor = make_cursor 0 0 }
    let to_string t = String.concat "\n" (List.rev t.lines)

    let last_line_index = function
      | { lines = []; _ } -> 0
      | { lines = l; _ } -> List.length l - 1
    ;;

    let map_lines f t =
      let rec aux acc line = function
        | [] -> List.rev acc
        | s :: rest -> aux (f line s :: acc) (line - 1) rest
      in
      aux [] (last_line_index t) t.lines
    ;;

    let del = function
      | { lines = []; cursor = _ } as t -> t
      | { lines = "" :: []; cursor = _ } -> empty
      | { lines = "" :: l :: t; cursor = _ } ->
        { lines = l :: t; cursor = { col = String.length l; line = List.length t } }
      | { lines = s :: t; cursor } ->
        { lines = String.sub s 0 (String.length s - 1) :: t
        ; cursor = { cursor with col = cursor.col - 1 }
        }
    ;;

    let add c = function
      | { lines = []; cursor = _ } ->
        { lines = [ String.make 1 c ]; cursor = make_cursor 0 1 }
      | { lines = s :: t; cursor } ->
        { lines = Printf.sprintf "%s%c" s c :: t
        ; cursor = { line = cursor.line; col = cursor.col + 1 }
        }
    ;;

    let newline t =
      { lines = "" :: t.lines; cursor = { line = t.cursor.line + 1; col = 0 } }
    ;;

    let line_at l t = List.nth t.lines l

    let cursor_left t =
      match t.lines with
      | [] -> t
      | _ ->
        let next_col = t.cursor.col - 1 in
        if next_col = -1
        then
          if t.cursor.line = 0
          then t
          else (
            let prev_line = line_at (t.cursor.line - 1) t in
            { t with cursor = make_cursor (t.cursor.line - 1) (String.length prev_line) })
        else t
    ;;

    let cursor_right t =
      match t.lines with
      | [] | [ "" ] -> t
      | _ ->
        let line_length = String.length (line_at t.cursor.line t)
        and next_col = t.cursor.col + 1 in
        if line_length = next_col
        then (
          let next_line = t.cursor.line + 1 in
          if next_line = List.length t.lines
          then t
          else { t with cursor = make_cursor next_line 0 })
        else { t with cursor = { t.cursor with col = next_col } }
    ;;
  end

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
