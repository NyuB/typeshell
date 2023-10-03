open Ide_lib
open Tea

let string_of_pos p = View.(Printf.sprintf "(%d, %d)" p.x p.y)
let string_of_dim d = string_of_pos View.(d.position)

let string_of_color : View.color -> string = function
  | Red -> "(R)"
  | Green -> "(G)"
  | Blue -> "(B)"
  | Raw i -> Printf.sprintf "(#%s)" (string_of_int i)
;;

type 'a predicate = 'a -> bool

type ('a, 'b) assertion =
  { message : 'b -> string
  ; predicate : 'a predicate
  }

type 'a assertions =
  { all : ('a, 'a) assertion list
  ; none : ('a, 'a) assertion list
  ; ones : ('a, unit) assertion list
  }

let assert_all all i =
  List.iter
    (fun a ->
      if a.predicate i
      then ()
      else
        Alcotest.fail
          (Printf.sprintf "One item does not match the assertion: '%s'" (a.message i)))
    all
;;

let assert_none none i =
  List.iter
    (fun a ->
      if a.predicate i
      then
        Alcotest.fail
          (Printf.sprintf "One item match a forbidden assertion: '%s'" (a.message i))
      else ())
    none
;;

let assert_no_one_left ones =
  List.iter
    (fun a ->
      Alcotest.fail (Printf.sprintf "One assertion was not matched: '%s'" (a.message ())))
    ones
;;

let assert_list { all; ones; none } l =
  let rec aux o = function
    | [] -> assert_no_one_left o
    | i :: t ->
      assert_all all i;
      assert_none none i;
      let next_ones = List.filter (fun p -> not (p.predicate i)) o in
      aux next_ones t
  in
  aux ones l
;;

let always_true _ = true
let ( |? ) opt default = Option.value opt ~default

let contains_text_matching_at ?(pos : View.position option) ~message pt =
  let pos_pred = Option.map ( = ) pos |? always_true in
  let predicate_item = function
    | View.Text (s, p) -> pt s && pos_pred p.position
    | _ -> false
  in
  let predicate = function
    | View.Text _ as t -> predicate_item t
    | Colored (_, items) -> List.exists predicate_item items
    | _ -> false
  in
  { predicate; message }
;;

let contains_text t =
  contains_text_matching_at
    ?pos:None
    ~message:(fun _ -> Printf.sprintf "contains Text %s" t)
    (String.equal t)
;;

let matches re s =
  try
    let _ = Re.Str.search_forward re s 0 in
    true
  with
  | Not_found -> false
;;

let contains_text_containing t =
  let re = Re.Str.regexp_string t in
  contains_text_matching_at
    ?pos:None
    ~message:(fun _ -> Printf.sprintf "contains Text %s" t)
    (matches re)
;;

let rec view_item_formatter ppf item =
  match item with
  | View.Line (origin, target) ->
    Format.pp_print_string
      ppf
      (Printf.sprintf
         "<Line>%s -> %s</Line>"
         (string_of_pos origin)
         (string_of_pos target))
  | View.Text (s, d) ->
    Format.pp_print_string ppf (Printf.sprintf "<Text>|%s| %s</Text>" s (string_of_dim d))
  | View.Colored (c, items) ->
    Format.pp_print_string ppf (Printf.sprintf "<Color %s>" (string_of_color c));
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
      view_item_formatter
      ppf
      items;
    Format.pp_print_string ppf "</Color>"
;;

let view_item_testable = Alcotest.testable view_item_formatter ( = )

let check_view expected actual =
  Alcotest.(check (list view_item_testable)) "View mismatch" expected actual
;;

let play_events events init =
  Seq.fold_left
    (fun (model, sub) evt ->
      match sub evt with
      | None -> model, sub
      | Some msg -> App.IDE.update model msg)
    init
    events
;;

let key_events_of_string s =
  s |> String.to_seq |> Seq.map (fun c -> Subscription.KeyPressed c)
;;

let resize_event width height = Subscription.Resize { width; height }
let char_size_one_event = Subscription.CharResize { width = 1; height = 1 }
let ( @:: ) sa sb = Seq.cons sa sb

let example_contains_text_after_typing =
  ( "Text and no error after typing a valid program"
  , fun () ->
      let init = App.IDE.init
      and line = "val hello = \"Hello world !\";" in
      let key_events = key_events_of_string line in
      let model, _ = play_events key_events init in
      let view = App.IDE.view model in
      assert_list
        { all = []
        ; none = [ contains_text_containing "error"; contains_text_containing "Error" ]
        ; ones = [ contains_text line ]
        }
        view )
;;

let example_snapshot_hello_world =
  ( "Hello world"
  , fun () ->
      let init = App.IDE.init
      and line = "val hello = \"Hello world !;\"" in
      let key_events = key_events_of_string line in
      let width, height = 1000, 500 in
      let events = char_size_one_event @:: resize_event width height @:: key_events in
      let model, _ = play_events events init in
      let view = App.IDE.view model in
      check_view
        [ Line ({ x = width / 2; y = 101 }, { x = width / 2; y = 500 })
        ; Line ({ x = 0; y = 101 }, { x = width; y = 101 })
        ; Text (line, { position = { x = 0; y = 466 } })
        ; Colored
            ( Red
            , [ Text
                  ( "Fatal error: exception Typeshell.Parser.MenhirBasics.Error"
                  , { position = { x = 0; y = 100 } } )
              ] )
        ; Colored (Red, [ Text ("_", { position = { x = String.length line; y = 466 } }) ])
        ]
        view )
;;

let quick_test (name, test) = name, `Quick, test
let quick_tests tests = List.map quick_test tests

let () =
  Alcotest.run
    "IDE lib"
    [ ( "Typing events"
      , quick_tests [ example_contains_text_after_typing; example_snapshot_hello_world ] )
    ]
;;
