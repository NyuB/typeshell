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
  | { lines = s :: t; cursor } as text ->
    if cursor.col = 0
    then text
    else (
      let l = String.length s in
      let left = String.sub s 0 (cursor.col - 1)
      and right = String.sub s cursor.col (l - cursor.col) in
      { lines = Printf.sprintf "%s%s" left right :: t
      ; cursor = { cursor with col = cursor.col - 1 }
      })
;;

let add c = function
  | { lines = []; cursor = _ } ->
    { lines = [ String.make 1 c ]; cursor = make_cursor 0 1 }
  | { lines = "" :: t; cursor } ->
    { lines = String.make 1 c :: t; cursor = make_cursor cursor.line 1 }
  | { lines = s :: t; cursor } ->
    let l = String.length s in
    let left = String.sub s 0 cursor.col
    and right = String.sub s cursor.col (l - cursor.col) in
    { lines = Printf.sprintf "%s%c%s" left c right :: t
    ; cursor = { line = cursor.line; col = cursor.col + 1 }
    }
;;

let newline t = { lines = "" :: t.lines; cursor = { line = t.cursor.line + 1; col = 0 } }
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
    else { t with cursor = { t.cursor with col = next_col } }
;;

let cursor_right t =
  match t.lines with
  | [] | [ "" ] -> t
  | _ ->
    let line_length = String.length (line_at t.cursor.line t) in
    if line_length = t.cursor.col
    then (
      let next_line = t.cursor.line + 1 in
      if next_line = List.length t.lines
      then t
      else { t with cursor = make_cursor next_line 0 })
    else { t with cursor = { t.cursor with col = t.cursor.col + 1 } }
;;

module Tests = struct
  let rec repr_with_cursor acc cursor current_line lines =
    if current_line = cursor.line + 1
    then (
      let cursor_line = String.make cursor.col '-' ^ "^" in
      List.rev (cursor_line :: acc) @ lines)
    else (
      match lines with
      | [] ->
        if current_line > cursor.line + 1
        then failwith (Printf.sprintf "Cursor not found %d %d" cursor.line cursor.col)
        else repr_with_cursor acc cursor (current_line + 1) []
      | h :: t -> repr_with_cursor (h :: acc) cursor (current_line + 1) t)
  ;;

  let print_for_test t =
    List.iter print_endline (repr_with_cursor [] t.cursor 0 (List.rev t.lines))
  ;;

  let add_chars s text = s |> String.to_seq |> Seq.fold_left (fun t c -> add c t) text

  type test_instruction =
    | Newline
    | Chars of string
    | Left
    | Right
    | Del

  let apply_instructions i t =
    let rec aux text = function
      | [] -> text
      | Chars s :: t -> aux (add_chars s text) t
      | Newline :: t -> aux (newline text) t
      | Left :: t -> aux (cursor_left text) t
      | Right :: t -> aux (cursor_right text) t
      | Del :: t -> aux (del text) t
    in
    aux t i
  ;;

  let of_instructions i = apply_instructions i empty

  let%expect_test "Single line" =
    let text = of_instructions [ Chars "this is one single line" ] in
    print_for_test text;
    [%expect {|
    this is one single line
    -----------------------^ |}]
  ;;

  let%expect_test "Single with newline" =
    let text = of_instructions [ Chars "this is one single line"; Newline ] in
    print_for_test text;
    [%expect {|
    this is one single line

    ^ |}]
  ;;

  let%expect_test "Single char then cursor left" =
    let text = of_instructions [ Chars "a"; Left ] in
    print_for_test text;
    [%expect {|
    a
    ^ |}]
  ;;

  let%expect_test "Left twice then right" =
    let text = of_instructions [ Chars "abc"; Left; Left; Right ] in
    print_for_test text;
    [%expect {|
    abc
    --^ |}]
  ;;

  let%expect_test "Right at eol without next line" =
    let text = of_instructions [ Chars "abc"; Right ] in
    print_for_test text;
    [%expect {|
    abc
    ---^ |}]
  ;;

  let%expect_test "Right at eol with next line" =
    let text =
      of_instructions [ Chars "abc"; Newline; Chars "def"; Left; Left; Left; Left ]
    in
    print_for_test text;
    [%expect {|
    abc
    ---^
    def |}];
    let right = apply_instructions [ Right ] text in
    print_for_test right;
    [%expect {|
    abc
    def
    ^ |}]
  ;;

  let%expect_test "Three chars then cursor left twice" =
    let text = of_instructions [ Chars "abc"; Left; Left ] in
    print_for_test text;
    [%expect {|
    abc
    -^ |}]
  ;;

  let%expect_test "Three chars then newline then del" =
    let text = of_instructions [ Chars "abc"; Newline; Del ] in
    print_for_test text;
    [%expect {|
    abc
    ---^ |}]
  ;;

  let%expect_test "Del empty" =
    print_for_test empty;
    [%expect {|
    ^ |}];
    let text = of_instructions [ Del; Del ] in
    print_for_test text;
    [%expect {|
    ^ |}]
  ;;

  let%expect_test "Type then left then type" =
    let text = of_instructions [ Chars "insert here <>"; Left; Chars "inserted" ] in
    print_for_test text;
    [%expect {|
    insert here <inserted>
    ---------------------^ |}]
  ;;

  let%expect_test "Type then left then del" =
    let text = of_instructions [ Chars "delete here <del>"; Left; Del; Del; Del ] in
    print_for_test text;
    [%expect {|
    delete here <>
    -------------^ |}]
  ;;
end
