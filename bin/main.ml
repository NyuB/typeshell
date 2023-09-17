open Typeshell
open Lexing

let () =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let env = Sys.getenv_opt in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let program = Parser.prog Lexer.read lexbuf in
      program
      |> Lang.Verifier.interpret_program Lang.phase_env
      |> Lang.Interpreter.interpret_program env)
;;
