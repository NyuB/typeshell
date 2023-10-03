open Js_of_ocaml
module Usf = Js.Unsafe

let jstr = Js.string
let je = Usf.js_expr
let ( ~~ ) = je
let ( |? ) opt default = Js.Opt.get opt default
let force () = assert false
let ( ! ) opt = opt |? force

let () =
  let d = Dom_html.document in
  let _ = !(d##getElementById (jstr "renderer"))
  and renderer_canvas = !(d##getElementById (jstr "renderer_canvas")) in
  let ctx = Usf.meth_call renderer_canvas "getContext" [| ~~"\"2d\"" |] in
  Usf.meth_call ctx "fillText" [| ~~"\"Hello there !\""; ~~"50"; ~~"50" |]
;;
