module G = Graphics

let () =
  print_endline "\n\n";
  print_endline "Starting IDE ...";
  G.set_window_title "TypeShell IDE";
  G.open_graph ":0 500x300-0+0";
  print_endline "IDE Started";
  print_endline (Printf.sprintf "Displaying %d x %d" (G.size_x ()) (G.size_y ()));
  G.set_color 0x0000FF;
  G.fill_circle 50 50 50;
  while true do
    print_char '.';
    flush stdout;
    Unix.sleep 5
  done
;;
