let () =
  let fps = 60.0 in
  let module R = Graphics_renderer in
  Ide_lib.Tea.loop_app (module Ide_lib.App.IDE) (module R) fps
;;
