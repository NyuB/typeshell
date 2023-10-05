open Js_of_ocaml
open Ide_lib

let jstr = Js.string
let ( |? ) opt default = Js.Opt.get opt default
let force () = assert false
let ( ! ) opt = opt |? force
let fps = 60.0

let () =
  let d = Dom_html.document in
  let renderer = !(d##getElementById (jstr "renderer"))
  and renderer_canvas = !(d##getElementById (jstr "renderer_canvas")) in
  let module I = struct
    let canvas = renderer_canvas
    let canvas_parent = renderer
  end
  in
  let module R = Js_renderer.MakeRenderer (I) in
  Tea.loop_app (module App.IDE) (module R) fps
;;
