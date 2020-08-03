open Ast
module VarSet = Set.Make (String)

let unbound_var_err = "Unbound variable"

let apply_non_fn_err = "Cannot apply non-function"

type eval_strategy = CBV | CBN

let strategy = CBV

(* in order to do beta reduction, we need to know which vars are free *)
let rec fv = function
  | Var name -> VarSet.singleton name
  | Fun (arg, body) -> VarSet.(diff (fv body) (singleton arg))
  | App (e1, e2) -> VarSet.union (fv e1) (fv e2)

(* in order to do alpha conversion, we need a way to generate new var names *)
let genvar =
  let counter = ref 0 in
  fun name ->
    incr counter;
    "$" ^ name ^ "_" ^ string_of_int !counter

(* do alpha conversion *)
(* rename all free var named [name] inside expr *)
let rec alpha_conv name new_name expr =
  match expr with
  | Var x -> if x = name then Var new_name else expr
  | App (e1, e2) ->
      App (alpha_conv name new_name e1, alpha_conv name new_name e2)
  | Fun (formal_param, body) ->
      let fp = if formal_param = name then new_name else formal_param in
      Fun (fp, alpha_conv name new_name body)

(* substitute all free vars named [name] with [value] *)
let rec subst name value expr =
  match expr with
  | Var vn -> if vn = name then value else expr
  | App (e1, e2) -> App (subst name value e1, subst name value e2)
  | Fun (formal_param, body) ->
      if (* no free vars named [name] *) name = formal_param then expr
      else if
        (* after substitution, free vars inside value will be captured *)
        VarSet.(mem formal_param (fv value))
      then
        let new_name = genvar name in

        let new_body = alpha_conv formal_param new_name body in
        Fun (new_name, subst name value new_body)
      else (* safe subst *) Fun (formal_param, subst name value body)

(* in pure lambda calculus, the only value is abstraction *)
(* let is_value = function Fun _ -> true | _ -> false *)
let rec eval expr =
  match expr with
  | Var _ -> expr
  (* | Var _ -> failwith unbound_var_err *)
  | Fun _ -> expr
  | App (e1, e2) -> app e1 e2

and app e1 e2 =
  match eval e1 with
  | Fun (name, body) ->
      let actual_param = match strategy with CBV -> eval e2 | CBN -> e2 in
      subst name actual_param body |> eval
  | _ -> failwith apply_non_fn_err
