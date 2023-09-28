module View = struct
  type position =
    { x : int
    ; y : int
    }

  let pos x y = { x; y }

  type dimensions =
    { width : int
    ; height : int
    }

  let dim_zero = { width = 0; height = 0 }

  type text_dimensions = { position : position }

  type color =
    | Red
    | Green
    | Blue
    | Raw of int

  let color_code = function
    | Red -> 0xFF0000
    | Green -> 0x00FF00
    | Blue -> 0x0000FF
    | Raw i -> i
  ;;

  type view_item =
    | Text of string * text_dimensions
    | Line of (position * position)
    | Colored of color * view_item list

  type t = view_item list
end

module Subscription = struct
  type display_event = ..
  type display_event += Resize of View.dimensions
  type display_event += CharResize of View.dimensions
  type display_event += Click of View.position
  type display_event += KeyPressed of char
  type 'm t = display_event -> 'm option
end

module type App = sig
  type model
  type message

  val init : model * message Subscription.t
  val update : model -> message -> model * message Subscription.t
  val view : model -> View.t
end

module type Renderer = sig
  val render : View.t -> unit
  val poll_events : unit -> Subscription.display_event list
end

let loop_app (module A : App) (module R : Renderer) (fps : float) : unit =
  let initial_model, initial_subscriptions = A.init in
  let model = ref initial_model
  and subscriptions = ref initial_subscriptions in
  while true do
    let events = R.poll_events () |> List.filter_map !subscriptions in
    let updated_model, updated_subscriptions =
      List.fold_left
        (fun (m, _) e ->
          let m, s = A.update m e in
          m, s)
        (!model, !subscriptions)
        events
    in
    model := updated_model;
    subscriptions := updated_subscriptions;
    R.render (A.view !model);
    Unix.sleepf (1.0 /. fps)
  done
;;
