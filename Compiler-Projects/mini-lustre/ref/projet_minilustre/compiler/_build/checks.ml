(* Vérification des transformations *)

open Asttypes
open Clocked_ast
open Clocked_ast_utils


(* Vérification de la normalisation *)

exception Normalization of c_node

let atom expr =
  begin match expr.cexpr_desc with
  | CE_const c -> true
  | CE_ident x -> true
  | CE_unop (_, _)
  | CE_binop (_, _, _)
  | CE_app (_, _)
  | CE_prim (_, _)
  | CE_if (_, _, _)
  | CE_when (_, _, _)
  | CE_fby (_, _)
  | CE_merge (_, _)
  | CE_tuple _
  | CE_print _ -> false
  end

let rec bexpr expr =
  if atom expr then true
  else
    begin match expr.cexpr_desc with
    | CE_unop (op, e) -> bexpr e
    | CE_binop (op, e1, e2) -> bexpr e1 && bexpr e2
    | CE_if (e, e1, e2) -> bexpr e && bexpr e1 && bexpr e2
    | CE_tuple (el) -> List.for_all bexpr el
    | CE_app (_, _)
    | CE_prim (_, _)
    | CE_fby (_, _)
    | CE_when (_, _, _)
    | CE_merge (_, _)
    | CE_print _ -> false
    | CE_const _
    | CE_ident _ -> assert false
    end

let normalized_expr expr =
  if bexpr expr then true
  else
    begin match expr.cexpr_desc with
    | CE_app (_, el) | CE_prim (_, el) | CE_print (el) -> List.for_all bexpr el
    | CE_fby (c, { cexpr_desc = CE_tuple el} ) -> List.for_all atom el
    | CE_fby (c, e) -> atom e
    | CE_when (e1, _, e2) -> atom e1 && atom e2
    | CE_merge (e, c_e_list) -> List.fold_left (fun b (_, e) -> b && (atom e)) (atom e) c_e_list
    | CE_const _
    | CE_ident _
    | CE_unop (_, _)
    | CE_binop (_, _, _)
    | CE_if (_, _, _)
    | CE_tuple _ -> assert false
    end

let normalized_node n =
  List.for_all (fun eq -> normalized_expr eq.ceq_expr) n.cn_equs

let normalization f =
  try
    List.iter
      (fun n -> if not (normalized_node n) then raise (Normalization n))
      f
  with Normalization n ->
    Format.eprintf "Warning: node %s is not in normal form.@." n.cn_name


(* Vérification de l'ordonnancement *)

exception Scheduling of c_node

let defs_of_patt patt acc =
  List.fold_left (fun acc x -> Scheduling.S.add x acc) acc patt.cpatt_desc

let deps_of_expr =
  let rec deps_of_expr expr acc =
    match expr.cexpr_desc with
    | CE_ident x -> expr, Scheduling.S.add x acc
    | CE_fby (c, e) -> expr, acc
    | CE_const _
    | CE_unop (_, _)
    | CE_binop (_, _, _)
    | CE_app (_, _)
    | CE_prim (_, _)
    | CE_if (_, _, _)
    | CE_when (_, _, _)
    | CE_merge (_, _)
    | CE_tuple _
    | CE_print _ -> expr_map_fold deps_of_expr expr acc
  in
  fun expr ->
    snd (deps_of_expr expr Scheduling.S.empty)

let scheduled_node node =
  let defs =
    List.fold_left
      (fun acc (x, _, _) -> Scheduling.S.add x acc)
      Scheduling.S.empty
      node.cn_input
  in
  let _ =
    List.fold_left
      (fun defs eq ->
        let deps = deps_of_expr eq.ceq_expr in
        if Scheduling.S.subset deps defs then
          defs_of_patt eq.ceq_patt defs
        else
          raise (Scheduling node))
      defs
      node.cn_equs
  in
  ()

let scheduling f =
  try
    List.iter scheduled_node f
  with Scheduling n ->
    Format.eprintf "Warning: node %s is not scheduled.@." n.cn_name

