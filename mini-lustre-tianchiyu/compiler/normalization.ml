open Typed_ast

let new_local =
  let cpt = ref 0 in fun () -> incr cpt; "aux'"^(string_of_int !cpt)

(** [new_pat e] prend en argument une expression [e] et retourne une
    variable (ou une liste de variable si [e] est un tuple) du même
    type que [e].

    Plus précisément, cette fonction retourne un triplet [(decl, patt, expr)],
    où
    [decl] est la déclaration de la variable (ou une liste de déclaration
    si [e] est un tuple),
    [patt] est la variable (ou le tuple de variables) vue comme un motif et
    [expr] est la variable (ou le tuple de variables) vue comme une expression.
*)
let new_pat ({ texpr_type= ty; texpr_loc = loc } as e) =
  match ty with
  | [t] ->
      let x = new_local() in
      let decl = [x,t] in
      let patt = { tpatt_desc = [x]; tpatt_type = ty; tpatt_loc = loc } in
      let expr = { e with texpr_desc = TE_ident x } in
      decl, patt, expr
  | lt ->
      let lx = List.map (fun _ -> new_local()) lt in
      let decl = List.combine lx lt in
      let patt = { tpatt_desc = lx; tpatt_type = ty; tpatt_loc = loc } in
      let le =
	List.map
	  (fun (x,t) ->
            { texpr_desc = TE_ident x; texpr_type = [t]; texpr_loc = loc}
          ) decl
      in
      decl, patt, { e with texpr_desc = TE_tuple le }

(** [normalize ctx e] met l'expression [e] en forme normale (en <bexpr>)
    et ajoute à [ctx] les équations introduites lors de la normalisation.
*)
let rec normalize ctx e =
  match e.texpr_desc with
  | TE_const _ | TE_ident _ -> ctx, e

  | TE_unop(op,e1) ->
      let ctx, e1' = normalize ctx e1 in
      ctx, { e with texpr_desc = TE_unop(op,e1') }

  | TE_binop(op,e1,e2) ->
	  (*ctx, e*)
	  let ctx, e1' = normalize ctx e1 in
	  let ctx, e2' = normalize ctx e2 in
	  ctx, {e with texpr_desc = TE_binop(op,e1',e2')}
      
	  (* DONE *)

  | TE_app(n,le) ->
      let (new_vars,new_eqs), le' = normalize_list ctx le in
      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { teq_patt = x_patt;
          teq_expr = { e with texpr_desc = TE_app(n,le') }; }
      in
      (x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_prim(n,le) ->
      let (new_vars,new_eqs), le' = normalize_list ctx le in
      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { teq_patt = x_patt;
          teq_expr = { e with texpr_desc = TE_prim(n,le') }; }
      in
      (x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_print le ->
      let (new_vars,new_eqs), le' = normalize_list ctx le in
      let x_decl, x_patt, x_expr = new_pat e in
      let x_eq =
        { teq_patt = x_patt;
          teq_expr = { e with texpr_desc = TE_print(le') }; }
      in
      (x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_if(e1,e2,e3) ->
      let ctx, e1' = normalize ctx e1 in
      let ctx, e2' = normalize ctx e2 in
      let ctx, e3' = normalize ctx e3 in
      ctx, { e with texpr_desc = TE_if(e1',e2',e3') }

  | TE_tuple l ->
      let ctx, l' = normalize_list ctx l in
      ctx, { e with texpr_desc = TE_tuple l'}

  | TE_fby(c,e1) ->
  	  (*ctx,e*) 
	  let (new_vars,new_eqs), e1' = normalize ctx e1 in
      let e1_decl, e1_patt, e1_expr = new_pat e1' in
      let e1_eq ={ teq_patt = e1_patt;teq_expr = e1'; } in
      (*transfer e1_expr.texpr_desc to TE_tuple type*)
      let e1_expr = match e1_expr.texpr_desc with
        | TE_tuple _ -> e1_expr
        | _ -> { e1_expr with texpr_desc = TE_tuple [e1_expr]}
      in

      let e_decl, e_patt, e_expr = new_pat e in
      let e_eq = { teq_patt = e_patt;teq_expr = {e with texpr_desc = TE_fby(c,e1_expr) };}in
      (e_decl@e1_decl@new_vars, e_eq::e1_eq::new_eqs), e_expr
      
	  (* c fby e1 => x, { x = c fby y; y = normalize e1; } *)
      (* DONE *)
  | TE_when(e1,c,e2) ->
      let ctx, e1' = normalize ctx e1 in
      let e1_decl, e1_patt, e1_expr = new_pat e1' in
      let e1_eq = { teq_patt = e1_patt;teq_expr = e1'; } in
      let e1_expr = { e1_expr with texpr_desc = TE_tuple [e1_expr] } in

      let (new_vars, new_eqs), e2' = normalize ctx e2 in
      let e2_decl, e2_patt, e2_expr = new_pat e2' in
      let e2_eq ={ teq_patt = e2_patt;teq_expr = e2'; } in
      let e2_expr = { e2_expr with texpr_desc = TE_tuple [e2_expr] } in

      let e_decl, e_patt, e_expr = new_pat e in
      let e_eq = { teq_patt = e_patt;
                   teq_expr = { e with texpr_desc = TE_when(e1_expr,c,e2_expr) }; } in
      (e_decl@e1_decl@e2_decl@new_vars, e_eq::e1_eq::e2_eq::new_eqs), e_expr
      (*DONE*)
  
  | TE_merge(e1, cel) ->
      let ctx, e1' = normalize ctx e1 in
      let e1_decl, e1_patt, e1_expr = new_pat e1' in
      let e1_eq = { teq_patt = e1_patt; teq_expr = e1'; } in
      let e1_expr = { e1_expr with texpr_desc = TE_tuple [e1_expr] } in

      let (new_vars,new_eqs), cel' = List.fold_left
        (fun (ctx, l) (c, e) -> let ctx, e' = normalize ctx e in (ctx, (c, e')::l))
        (ctx, []) cel in

      let cel_decl, cel_expr, cel_eq = List.fold_left
        (fun ( cel_decl, cel_expr, cel_eq ) (c, e')->
          let e_decl, e_patt, e_expr = new_pat e' in
          let e_eq ={ teq_patt = e_patt; teq_expr = e'; } in
          let e_expr = { e_expr with texpr_desc = TE_tuple [e_expr] } in
          ( e_decl@cel_decl, (c, e_expr)::cel_expr,e_eq::cel_eq))
        ([], [] ,[]) cel' in

      let e_decl, e_patt, e_expr = new_pat e in
      let e_eq ={ teq_patt = e_patt;
                  teq_expr = { e with texpr_desc = TE_merge(e1_expr, cel_expr) }; }
      in
      
      (e_decl@e1_decl@cel_decl@new_vars, cel_eq@(e_eq::e1_eq::new_eqs)), e_expr
 

and normalize_list ctx l =
  let ctx, l =
    List.fold_left
      (fun (ctx,l) e ->
	let ctx, e' = normalize ctx e in
	ctx, e'::l ) (ctx,[]) l
  in ctx, List.rev l

let normalize_equation node e =
  let (locals, new_eqs), e' = normalize ([],[]) e.teq_expr in
  { node with
    tn_local = locals@node.tn_local;
    tn_equs = { e with teq_expr = e' } :: (List.rev new_eqs) @ node.tn_equs }

let file =
  List.map
    (fun n ->
      let n =
	List.fold_left normalize_equation { n with tn_equs=[] } n.tn_equs
      in
      { n with tn_equs = List.rev n.tn_equs })
