open Tea
module G = Graphics

module GraphicsRenderer () : Renderer = struct
  let () =
    G.set_window_title "TypeShell IDE";
    G.open_graph ":0 500x300-0+0";
    G.set_color 0x000000
  ;;

  type state =
    { mutable previous_dimensions : View.dimensions
    ; mutable previous_char_size : View.dimensions
    }

  let state = { previous_dimensions = View.dim_zero; previous_char_size = View.dim_zero }

  let batch_draw f =
    Fun.protect
      ~finally:(fun () ->
        G.synchronize ();
        G.auto_synchronize true)
      (fun () ->
        G.auto_synchronize false;
        f ())
  ;;

  let render_text text View.{ position = { x; y } } =
    G.moveto x y;
    G.draw_string text
  ;;

  let rec render_rec = function
    | [] -> ()
    | View.Text (t, pos) :: rest ->
      render_text t pos;
      render_rec rest
    | Line ({ x; y }, { x = tx; y = ty }) :: rest ->
      G.draw_poly_line [| x, y; tx, ty |];
      render_rec rest
    | Colored (c, l) :: rest ->
      G.set_color (View.color_code c);
      render_rec l;
      G.set_color G.foreground;
      render_rec rest
  ;;

  let render view =
    batch_draw (fun () ->
      G.clear_graph ();
      render_rec view)
  ;;

  let current_dimensions () =
    let width, height = G.size_x (), G.size_y () in
    View.{ width; height }
  ;;

  let read_key_opt () = if G.key_pressed () then Some (G.read_key ()) else None

  let read_available_keys () =
    let rec aux acc =
      match read_key_opt () with
      | None -> List.rev acc
      | Some k -> aux (k :: acc)
    in
    aux []
  ;;

  let poll_keys_event () =
    List.map (fun k -> Subscription.KeyPressed k) (read_available_keys ())
  ;;

  let poll_size_event () =
    match state.previous_dimensions, current_dimensions () with
    | previous, current when previous <> current ->
      state.previous_dimensions <- current;
      [ Subscription.Resize current ]
    | _ -> []
  ;;

  let poll_char_size_event () =
    let width, height = G.text_size "a" in
    if View.{ width; height } <> state.previous_char_size
    then (
      let () = state.previous_char_size <- { width; height } in
      [ Subscription.CharResize { width; height } ])
    else []
  ;;

  let poll_events () =
    let keys = poll_keys_event ()
    and char = poll_char_size_event ()
    and resize = poll_size_event () in
    resize @ keys @ char
  ;;
end

module IDE : App = struct
  type model =
    { dimensions : View.dimensions
    ; char_size : View.dimensions
    ; text : string list
    }

  let last_line_length model =
    match model.text with
    | [] -> 0
    | s :: _ -> String.length s
  ;;

  let last_line model =
    match model.text with
    | [] -> 0
    | l -> List.length l - 1
  ;;

  let width model = model.dimensions.width
  let height model = model.dimensions.height
  let cursor_x model = last_line_length model * model.char_size.width
  let text_y_base model = height model * 5 / 8
  let text_line_y line model = text_y_base model - (line * model.char_size.height)
  let text_y_end model = text_line_y (last_line model) model
  let cursor_position model = View.{ position = pos (cursor_x model) (text_y_end model) }
  let cursor = "_"

  type message =
    | UpdateDimensions of View.dimensions
    | CharTyped of char
    | CharResized of View.dimensions

  let del = '\008'

  let subscribes = function
    | Subscription.Resize d -> Some (UpdateDimensions d)
    | Subscription.KeyPressed k -> Some (CharTyped k)
    | Subscription.CharResize d -> Some (CharResized d)
    | _ -> None
  ;;

  let init =
    { dimensions = View.dim_zero; char_size = View.dim_zero; text = [] }, subscribes
  ;;

  let del_char model =
    match model.text with
    | [] -> []
    | "" :: t -> t
    | s :: t -> String.sub s 0 (String.length s - 1) :: t
  ;;

  let add_char c model =
    match c, model.text with
    | '\n', t | '\r', t -> "" :: t
    | c, [] -> [ String.make 1 c ]
    | c, s :: t -> Printf.sprintf "%s%c" s c :: t
  ;;

  let update model = function
    | UpdateDimensions dimensions -> { model with dimensions }, subscribes
    | CharTyped c ->
      let text = if c <> del then add_char c model else del_char model in
      { model with text }, subscribes
    | CharResized char_size -> { model with char_size }, subscribes
  ;;

  let text_view model =
    let rec aux acc line = function
      | [] -> List.rev acc
      | s :: t ->
        aux
          View.(Text (s, { position = pos 0 (text_line_y line model) }) :: acc)
          (line - 1)
          t
    in
    aux [] (List.length model.text - 1) model.text
  ;;

  let view model =
    let middle_x = width model / 2
    and quarter_y = height model / 4 in
    View.
      [ Line ({ x = middle_x; y = 0 }, { x = middle_x; y = height model })
      ; Line ({ x = 0; y = quarter_y }, { x = width model; y = quarter_y })
      ; Text
          ( Printf.sprintf "Current dimensions : %d x %d" (width model) (height model)
          , { position = pos 0 (height model * 7 / 8) } )
      ]
    @ text_view model
    @ View.[ Colored (Red, [ Text (cursor, cursor_position model) ]) ]
  ;;
end

let () =
  let fps = 60.0 in
  let module R = GraphicsRenderer () in
  loop_app (module IDE) (module R) fps
;;
