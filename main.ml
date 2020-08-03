open Ast
open Vm

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let interp s = s |> parse |> eval

let () =
  let ast1 = parse "(fun x -> fun y -> x y) y" in
  ast1 |> show_expr |> print_string;
  print_newline ();
  ast1 |> eval |> show_expr |> print_string;
  print_newline ();

  let ast2 = parse "(fun x -> fun y -> x y) (fun z -> z z) (fun x -> x)" in
  ast2 |> show_expr |> print_string;
  print_newline ();
  ast2 |> eval |> show_expr |> print_string;
  print_newline ()
