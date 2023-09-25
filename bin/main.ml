open Typeshell
open Lexing

let apply_phases program =
  program
  |> Lang.Assignments.interpret_program Lang.phase_env
  |> Lang.Function_Calls.interpret_program Lang.BashStdLib.stdlib
;;

type args =
  { transpiled_filename : string option
  ; source_file : string
  }

let usage = "typeshell [-t <transpiled_filename>] source_file"

let parse_args = function
  | "-t" :: transpiled_filename :: [ source_file ]
  | source_file :: "-t" :: [ transpiled_filename ] ->
    { transpiled_filename = Some transpiled_filename; source_file }
  | [ source_file ] -> { source_file; transpiled_filename = None }
  | args ->
    raise
    @@ Invalid_argument
         (Printf.sprintf
            "Invalid arguments %s\nUsage : %s"
            (String.concat " " args)
            usage)
;;

let protected_in filename f =
  let ic = open_in filename in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f ic)
;;

let protected_out filename f =
  let oc = open_out filename in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)
;;

let () =
  try
    let args =
      parse_args (Array.sub Sys.argv 1 (Array.length Sys.argv - 1) |> Array.to_list)
    in
    protected_in args.source_file (fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = args.source_file };
      let program = Parser.prog Lexer.read lexbuf |> apply_phases in
      match args.transpiled_filename with
      | None ->
        let env = Sys.getenv_opt in
        Lang.Interpreter.interpret_program env program
      | Some output_file ->
        protected_out output_file (fun oc ->
          List.iter
            (fun line ->
              Out_channel.output_string oc line;
              Out_channel.output_char oc '\n')
            (Lang.Bash.interpret_program Lang.phase_env program);
          Out_channel.flush oc))
  with
  | exn ->
    print_endline
      (Printf.sprintf "Fatal error: exception %s" (Printexc.to_string_default exn));
    exit 2
;;
