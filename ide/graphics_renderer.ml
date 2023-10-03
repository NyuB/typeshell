open Ide_lib.Tea
module G = Graphics

let init () =
  G.set_window_title "TypeShell IDE";
  G.open_graph ":0 1500x900-0+0";
  G.set_color 0x000000
;;

type state =
  { mutable previous_dimensions : View.dimensions
  ; mutable previous_char_size : View.dimensions
  ; color : View.color
  }

let state =
  { previous_dimensions = View.dim_zero
  ; previous_char_size = View.dim_zero
  ; color = Raw 0x000000
  }
;;

let batch_draw f =
  Fun.protect
    ~finally:(fun () ->
      G.synchronize ();
      G.auto_synchronize true)
    (fun () ->
      G.auto_synchronize false;
      f ())
;;

let with_color c f =
  Fun.protect
    ~finally:(fun () -> G.set_color (View.color_code state.color))
    (fun () ->
      G.set_color (View.color_code c);
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
    with_color c (fun () -> render_rec l);
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
