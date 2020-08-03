open Ast
open Vm

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let interp s = s |> parse |> eval Env.empty

let () =
  let ast2 = parse "(fun x -> fun y -> x y) (fun z -> z z) (fun x -> x)" in
  ast2 |> show_expr |> print_string;
  print_newline ();
  ast2 |> eval Env.empty
  |> (fun (Closure (fp, body, _)) -> show_expr (Fun (fp, body)))
  |> print_string;
  print_newline ()
