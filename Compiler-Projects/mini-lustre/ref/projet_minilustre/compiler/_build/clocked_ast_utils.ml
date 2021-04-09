open Clocked_ast

(** [expr_map f expr] applique la fonction [f] aux sous-expressions
    directes de [expr]. Cette fonction de fait pas de parcours en
    profondeur.
*)
let rec expr_map f expr =
  let desc =
    match expr.cexpr_desc with
    | CE_const c -> CE_const c
    | CE_ident x -> CE_ident x
    | CE_unop (op, e) -> CE_unop (op, f e)
    | CE_binop (op, e1, e2) ->
        let e1 = f e1 in
        let e2 = f e2 in
        CE_binop (op, e1, e2)
    | CE_app (n, el) -> CE_app (n, List.map f el)
    | CE_prim (n, el) -> CE_prim (n, List.map f el)
    | CE_if (e, e1, e2) ->
        let e = f e in
        let e1 = f e1 in
        let e2 = f e2 in
        CE_if (e, e1, e2)
    | CE_when (e1, c, e2) ->
        let e1 = f e1 in
        let e2 = f e2 in
        CE_when (e1, c, e2)
    | CE_fby (c, e) -> CE_fby (c, f e)
    | CE_tuple el -> CE_tuple (List.map f el)
    | CE_print el -> CE_print (List.map f el)
    | CE_merge (e, cel) -> CE_merge (f e, List.map (fun (c,e) -> (c, f e)) cel)
  in
  { expr with cexpr_desc = desc }


(** [expr_map f expr acc] applique la fonction [f] avec l'argument
    [acc] aux sous-expressions directes de [expr]. Cette fonction de
    fait pas de parcours en profondeur.
*)
let expr_map_fold f expr acc =
  let desc, acc =
    match expr.cexpr_desc with
    | CE_const c -> CE_const c, acc
    | CE_ident x -> CE_ident x, acc
    | CE_unop (op, e) ->
        let e, acc = f e acc in
        CE_unop (op, e), acc
    | CE_binop (op, e1, e2) ->
        let e1, acc = f e1 acc in
        let e2, acc = f e2 acc in
        CE_binop (op, e1, e2), acc
    | CE_app (n, el) ->
        let rev_el, acc =
          List.fold_left
            (fun (rev_el, acc) e -> let e, acc = f e acc in e :: rev_el, acc)
            ([], acc) el
        in
        CE_app (n, List.rev rev_el), acc
    | CE_prim (n, el) ->
        let rev_el, acc =
          List.fold_left
            (fun (rev_el, acc) e -> let e, acc = f e acc in e :: rev_el, acc)
            ([], acc) el
        in
        CE_prim (n, List.rev rev_el), acc
    | CE_if (e, e1, e2) ->
        let e, acc = f e acc in
        let e1, acc = f e1 acc in
        let e2, acc = f e2 acc in
        CE_if (e, e1, e2), acc
    | CE_when (e1, c, e2) ->
        let e1, acc = f e1 acc in
        let e2, acc = f e2 acc in
        CE_when (e1, c, e2), acc
    | CE_fby (c, e) ->
        let e, acc = f e acc in
        CE_fby (c, e), acc
    | CE_tuple el ->
        let rev_el, acc =
          List.fold_left
            (fun (rev_el, acc) e -> let e, acc = f e acc in e :: rev_el, acc)
            ([], acc) el
        in
        CE_tuple (List.rev rev_el), acc
    | CE_print el ->
        let rev_el, acc =
          List.fold_left
            (fun (rev_el, acc) e -> let e, acc = f e acc in e :: rev_el, acc)
            ([], acc) el
        in
        CE_print (List.rev rev_el), acc
    | CE_merge (e, cel) ->
    let e, acc = f e acc in
    let rev_cel, acc =
      List.fold_left
        (fun (rev_cel, acc) (c, e) -> let e, acc = f e acc in (c, e) :: rev_cel, acc)
        ([], acc) cel
    in
    CE_merge (e, List.rev rev_cel), acc
  in
  { expr with cexpr_desc = desc }, acc
