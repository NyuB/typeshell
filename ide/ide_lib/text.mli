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
