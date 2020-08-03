open Ast
module Env = Map.Make (String)

(* in pure lambda calculus, the only value is function,
   In the environment model, that is a closure *)
(* fp body env *)
type value = Closure of string * expr * env

and env = value Env.t

let unbound_var_err = "Unbound variable"

let apply_non_fn_err = "Cannot apply non-function"

type scope = Dynamic | Lexical

let scope = Lexical

let rec eval env expr =
  match expr with
  | Var name ->
      if Env.mem name env then Env.find name env else failwith unbound_var_err
  | App (e1, e2) -> app env e1 e2
  | Fun (fp, body) -> Closure (fp, body, env)

and app env e1 e2 =
  let (Closure (fp, body, defenv)) = eval env e1 in
  let ap = eval env e2 in
  let base_env = match scope with Dynamic -> env | Lexical -> defenv in
  let actual_env = Env.add fp ap base_env in
  eval actual_env body
