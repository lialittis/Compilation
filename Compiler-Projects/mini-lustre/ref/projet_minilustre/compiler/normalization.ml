open Clocked_ast

let new_local =
  let cpt = ref 0 in fun () -> incr cpt; "aux'"^(string_of_int !cpt)

(** [new_pat e] prend en argument une expression [e] et retourne une
    variable (ou une liste de variable si [e] est un tuple) du même
    type et même clock que [e].

    Plus précisément, cette fonction retourne un triplet [(decl, patt, expr)],
    où
    [decl] est la déclaration de la variable (ou une liste de déclaration
    si [e] est un tuple),
    [patt] est la variable (ou le tuple de variables) vue comme un motif et
    [expr] est la variable (ou le tuple de variables) vue comme une expression.
*)
let new_pat ({ cexpr_type= ty; cexpr_loc = loc; cexpr_clock = ck } as e) =
  match ty, ck with
  | [t], ClockTuple [ck] | [t], ck ->
    begin
      let x = new_local() in
      let decl = [x,t, ClockTuple [ck]] in
      let patt = { cpatt_desc = [x]; cpatt_type = ty; cpatt_loc = loc; cpatt_clock = ClockTuple [ck] } in
      let expr = { e with cexpr_desc = CE_ident x } in
      decl, patt, expr
    end
  | lt, ClockTuple lck ->
    begin
      let lx = List.map (fun _ -> new_local()) lt in
      let lxt = List.combine lx lt in
      let decl = List.map (fun ((x, t), ck) -> (x, t, ClockTuple [ck])) (List.combine lxt lck) in
      let patt = { cpatt_desc = lx; cpatt_type = ty; cpatt_loc = loc; cpatt_clock = ClockTuple lck } in
      let le =
        List.map
          (fun (x,t, ck) ->
          { cexpr_desc = CE_ident x; cexpr_type = [t]; cexpr_loc = loc; cexpr_clock = ck })
          decl
      in
      decl, patt, { e with cexpr_desc = CE_tuple le }
    end
  | _, _ -> assert false

(** [normalize ctx e] met l'expression [e] en forme normale (en <bexpr>)
    et ajoute à [ctx] les équations introduites lors de la normalisation.
*)
let rec normalize ctx e =
  match e.cexpr_desc with
  | CE_const _ | CE_ident _ -> ctx, e

  | CE_unop(op,e1) ->
      let ctx, e1' = normalize ctx e1 in
      ctx, { e with cexpr_desc = CE_unop(op,e1') }

  | CE_binop(op,e1,e2) ->
      (* DONE *)
      let ctx, e1' = normalize ctx e1 in
      let ctx, e2' = normalize ctx e2 in
      ctx, { e with cexpr_desc = CE_binop(op,e1',e2') }

  | CE_app(n,le) ->
      let (new_vars,new_eqs), le' = normalize_list ctx le in
      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { ceq_patt = x_patt;
          ceq_expr = { e with cexpr_desc = CE_app(n,le') }; }
      in
      (x_decl@new_vars, x_eq::new_eqs), x_expr

  | CE_prim(n,le) ->
      let (new_vars,new_eqs), le' = normalize_list ctx le in
      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { ceq_patt = x_patt;
          ceq_expr = { e with cexpr_desc = CE_prim(n,le') }; }
      in
      (x_decl@new_vars, x_eq::new_eqs), x_expr

  | CE_print le ->
      let (new_vars,new_eqs), le' = normalize_list ctx le in
      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { ceq_patt = x_patt;
          ceq_expr = { e with cexpr_desc = CE_print(le') }; }
      in
      (x_decl@new_vars, x_eq::new_eqs), x_expr

  | CE_if(e1,e2,e3) ->
      let ctx, e1' = normalize ctx e1 in
      let ctx, e2' = normalize ctx e2 in
      let ctx, e3' = normalize ctx e3 in
      ctx, { e with cexpr_desc = CE_if(e1',e2',e3') }

  | CE_merge(e, c_e_list) ->
      (* merge e (C1 -> e1) ... (Cn -> en) => 
      x, 
      {x = merge a (C1 -> a1) ... (Cn -> an);
       a = normalize e;
       a1 = normalize e1;
       ...
       an = normalize en} *)
      let ctx, e' = normalize ctx e in
      let (new_vars,new_eqs), c_e'_list = 
        List.fold_left 
        (fun (cxt, l) (c, e) -> let cxt, e' = normalize cxt e in (cxt, (c, e')::l))
        (ctx, []) c_e_list
      in

      let a_decl, a_patt, a_expr = new_pat e' in
      let a_eq =
        { ceq_patt = a_patt;
          ceq_expr = e'; }
      in
      let a_expr = { a_expr with cexpr_desc = CE_tuple [a_expr] } in

      let eq_list, decl_list, c_expr_list =
        List.fold_left
        (fun (eq_list, decl_list, c_expr_list) (c, e')->
          let e_decl, e_patt, e_expr = new_pat e' in
          let e_eq =
            { ceq_patt = e_patt;
              ceq_expr = e'; }
          in
          let e_expr = { e_expr with cexpr_desc = CE_tuple [e_expr] } in
          (e_eq::eq_list, e_decl@decl_list, (c, e_expr)::c_expr_list)
        )
        ([], [], [])
        c_e'_list
      in

      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { ceq_patt = x_patt;
          ceq_expr = { e with cexpr_desc = CE_merge(a_expr, c_expr_list) }; }
      in
      
      (x_decl@a_decl@decl_list@new_vars, eq_list@(x_eq::a_eq::new_eqs)), x_expr

  | CE_when(e1,c,e2) ->
      (* e1 when C(e2) => x, { x = a when C(y); a = normalize e1; y = normalize e2 } *)
      let ctx, e1' = normalize ctx e1 in
      let (new_vars,new_eqs), e2' = normalize ctx e2 in

      let a_decl, a_patt, a_expr = new_pat e1' in
      let a_eq =
        { ceq_patt = a_patt;
          ceq_expr = e1'; }
      in
      let a_expr = { a_expr with cexpr_desc = CE_tuple [a_expr] } in

      let y_decl, y_patt, y_expr = new_pat e2' in
      let y_eq =
        { ceq_patt = y_patt;
          ceq_expr = e2'; }
      in
      let y_expr = { y_expr with cexpr_desc = CE_tuple [y_expr] } in


      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { ceq_patt = x_patt;
          ceq_expr = { e with cexpr_desc = CE_when(a_expr,c,y_expr) }; }
      in

      (x_decl@a_decl@y_decl@new_vars, x_eq::a_eq::y_eq::new_eqs), x_expr

  | CE_tuple l ->
      let ctx, l' = normalize_list ctx l in
      ctx, { e with cexpr_desc = CE_tuple l'}

  | CE_fby(c,e1) ->
      (* c fby e1 => x, { x = c fby y; y = normalize e1; } *)
      (* DONE *)
      let (new_vars,new_eqs), e1' = normalize ctx e1 in

      let y_decl, y_patt, y_expr = new_pat e1' in
      let y_eq =
        { ceq_patt = y_patt;
          ceq_expr = e1'; }
      in
      let y_expr = match y_expr.cexpr_desc with 
        | CE_tuple _ ->  y_expr
        | _ -> { y_expr with cexpr_desc = CE_tuple [y_expr] }
      in

      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { ceq_patt = x_patt;
          ceq_expr = { e with cexpr_desc = CE_fby(c,y_expr) }; }
      in

      (x_decl@y_decl@new_vars, x_eq::y_eq::new_eqs), x_expr



and normalize_list ctx l =
  let ctx, l =
    List.fold_left
      (fun (ctx,l) e ->
	let ctx, e' = normalize ctx e in
	ctx, e'::l ) (ctx,[]) l
  in ctx, List.rev l

let normalize_equation node e =
  let (locals, new_eqs), e' = normalize ([],[]) e.ceq_expr in
  { node with
    cn_local = locals@node.cn_local;
    cn_equs = { e with ceq_expr = e' } :: (List.rev new_eqs) @ node.cn_equs }

let file =
  List.map
    (fun n ->
      let n =
	List.fold_left normalize_equation { n with cn_equs=[] } n.cn_equs
      in
      { n with cn_equs = List.rev n.cn_equs })
