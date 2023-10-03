let () =
  let fps = 60.0 in
  let module R = Graphics_renderer in
  Tea.loop_app (module App.IDE) (module R) fps
;;
