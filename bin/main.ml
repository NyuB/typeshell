open Typeshell
open Lexing

let result_bind f r = Result.bind r f

let protected_in filename f =
  let ic = open_in filename in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f ic)
;;

let protected_out filename f =
  let oc = open_out filename in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)
;;

let apply_phases (program : Lang.program) : Lang.program_result =
  program
  |> Assignments.interpret_program ()
  |> result_bind (Compiler.Function_Calls.interpret_program Compiler.BashStdLib.stdlib)
;;

let transpile_to_file output_file program =
  protected_out output_file (fun oc ->
    List.iter
      (fun line ->
        Out_channel.output_string oc line;
        Out_channel.output_char oc '\n')
      (Compiler.Bash.interpret_program () program);
    Out_channel.flush oc)
;;

let interpret program =
  let env = Sys.getenv_opt in
  Compiler.Interpreter.interpret_program env program
;;

type args =
  { transpiled_filename : string option
  ; source_file : string
  }

let main args program =
  match args.transpiled_filename with
  | None -> interpret program
  | Some output_file -> transpile_to_file output_file program
;;

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

let print_exceptions_and_exit exn_list exn_printer code =
  List.iter (fun exn -> print_endline (exn_printer exn)) exn_list;
  exit code
;;

let () =
  try
    let args =
      parse_args (Array.sub Sys.argv 1 (Array.length Sys.argv - 1) |> Array.to_list)
    in
    protected_in args.source_file (fun ic ->
      let lexbuf = Lexing.from_channel ic in
      let () =
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = args.source_file }
      in
      match Parser.prog Lexer.read lexbuf |> apply_phases with
      | Ok program -> main args program
      | Error exn_list -> print_exceptions_and_exit exn_list Exceptions.exn_print 2)
  with
  | exn -> print_exceptions_and_exit [ exn ] Exceptions.default_print 2
;;
