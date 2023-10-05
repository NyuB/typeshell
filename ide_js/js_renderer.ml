open Ide_lib.Tea
open Js_of_ocaml

let jstr = Js.string
let ( ~~ ) = Js.Unsafe.js_expr
let ( ~! ) = Js.Unsafe.inject
let iof f = if Js.isNaN f then -42 else Js.float_of_number f |> int_of_float
let noi i = i |> float_of_int |> Js.number_of_float

let provided_call f_name =
  Js.Unsafe.fun_call (Js.Unsafe.eval_string @@ "globalThis.jsoo_runtime." ^ f_name)
;;

let getEffectiveDimensions el =
  let d = provided_call "effectiveDimensions" [| ~!el |] in
  let width = iof d##.width
  and height = iof (Js.Unsafe.get d "height") in
  View.{ width; height }
;;

module type Injection = sig
  val canvas : Dom_html.element Js.t
  val canvas_parent : Dom_html.element Js.t
end

class context_2d canvas canvas_parent =
  object (self)
    val ctx =
      let ctx = Js.Unsafe.meth_call canvas "getContext" [| ~~"\"2d\"" |] in
      ctx##.lineWidth := 1;
      ctx

    method clear : unit =
      let View.{ width; height } = self#dimensions in
      ctx##clearRect 0 0 width height

    method drawText (t : string) (pos : View.position) : unit =
      ctx##fillText t pos.x pos.y

    method drawLine (origin : View.position) (target : View.position) : unit =
      ctx##beginPath;
      ctx##moveTo origin.x origin.y;
      ctx##lineTo target.x target.y;
      ctx##stroke

    method setColor (c : View.color) : unit = ctx##.strokeStyle := View.color_code c
    method dimensions : View.dimensions = getEffectiveDimensions ~!canvas_parent
  end

module MakeRenderer (I : Injection) : Renderer = struct
  let ctx = new context_2d I.canvas I.canvas_parent
  let state = ref View.{ width = 0; height = 0 }
  let color = ref (View.Raw 0x000000)
  let event_queue = Queue.create ()

  let handle_keyup_event evt =
    let open Subscription in
    let s = Js.to_string evt##.key in
    match s with
    | "Backspace" | "Delete" -> Queue.add (KeyPressed '\008') event_queue
    | "Tab" -> Queue.add (KeyPressed '\t') event_queue
    | "Enter" -> Queue.add (KeyPressed '\n') event_queue
    | s when String.length s = 1 -> Queue.add (KeyPressed (String.get s 0)) event_queue
    | _ -> ()
  ;;

  let set_dimensions () =
    let View.{ width; height } = ctx#dimensions in
    Js.Unsafe.set I.canvas "width" (noi width);
    Js.Unsafe.set I.canvas "height" (noi height)
  ;;

  let clear () : unit = ctx#clear
  let flip_position View.{ width = _; height } View.{ x; y } = View.{ x; y = height - y }

  let init () =
    clear ();
    set_dimensions ();
    let callback = Js.wrap_callback handle_keyup_event in
    Js.Unsafe.meth_call
      Dom_html.document
      "addEventListener"
      [| ~!(Js.string "keyup"); ~!callback |]
  ;;

  let sleepf seconds f =
    let delay = Js.number_of_float (1000.0 *. seconds) in
    let callback = Js.wrap_callback f in
    Js.Unsafe.fun_call ~~"setTimeout" [| ~!callback; Js.Unsafe.coerce delay |]
  ;;

  let with_color c f =
    let save = !color in
    color := c;
    ctx#setColor c;
    f ();
    color := save;
    ctx#setColor save
  ;;

  let rec render_item dimensions =
    let flip = flip_position dimensions in
    function
    | View.Line (origin, target) -> ctx#drawLine (flip origin) (flip target)
    | View.Text (t, { position }) -> ctx#drawText t (flip position)
    | View.Colored (c, items) ->
      with_color c (fun () -> List.iter (render_item dimensions) items)
  ;;

  let render view =
    clear ();
    set_dimensions ();
    List.iter (render_item ctx#dimensions) view
  ;;

  let rec poll_all_events acc =
    match Queue.take_opt event_queue with
    | None -> List.rev acc
    | Some evt -> poll_all_events (evt :: acc)
  ;;

  let poll_events () =
    let d = ctx#dimensions
    and current_state = !state in
    let events = poll_all_events [] in
    if d <> current_state then Subscription.Resize d :: events else events
  ;;
end
