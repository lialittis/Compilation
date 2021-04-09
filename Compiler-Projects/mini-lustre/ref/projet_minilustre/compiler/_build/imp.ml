open Asttypes
open Clocked_ast
open Imp_ast

let rec untulify el =
  List.fold_right
    (fun e acc ->
      match e.cexpr_desc with
      | CE_tuple el -> (untulify el @ acc)
      | _ -> e :: acc)
    el []

let empty_mem = { fby_mem = []; node_mem = [] }

let empty_init = { fby_init = []; node_init = [] }

let gen_next_id =
  let cpt = ref 0 in
  fun n -> incr cpt; n^"_next"^(string_of_int !cpt)

let gen_mem_id =
  let cpt = ref 0 in
  fun n -> incr cpt; n^"_mem"^(string_of_int !cpt)

let compile_patt {cpatt_loc = loc; cpatt_desc = d; cpatt_type = t; cpatt_clock = ck } =
  match ck with
  | ClockTuple ckl -> (try List.map (fun ((d, t), ck) -> (d, t, ck)) (List.combine (List.combine d t) ckl) with Invalid_argument _ -> assert false)
  | _ -> assert false (* impossible car après clocking cela devrait être un tuple *)

let rec compile_base_expr e =
  let desc =
    match e.cexpr_desc with
    | CE_const c -> ME_const c
    | CE_ident x -> ME_ident x
    | CE_unop (op, e) -> ME_unop(op, compile_base_expr e)
    | CE_binop (op, e1, e2) ->
        let me1 =  compile_base_expr e1 in
        let me2 =  compile_base_expr e2 in
        ME_binop (op, me1, me2)
    | CE_if (e1, e2, e3) -> (* DONE *)
        let me1 =  compile_base_expr e1 in
        let me2 =  compile_base_expr e2 in
        let me3 =  compile_base_expr e3 in
        ME_if (me1, me2, me3)
    | CE_when _ -> assert false (* impossible car en forme normale *)
    | CE_merge (e, c_e_list) -> 
        let me = compile_base_expr e in
        let c_me_list = List.map (fun (c, e) -> (c, compile_base_expr e)) c_e_list in
        ME_merge (me, c_me_list)
    | CE_tuple el -> ME_tuple (List.map compile_base_expr el)
    | CE_print el -> ME_print (List.map compile_base_expr el)
    | CE_fby _ -> assert false (* impossible car en forme normale *)
    | CE_app _ -> assert false (* impossible car en forme normale *)
    | CE_prim(f, el) ->
        let _, f_out_ty =
          try List.assoc f Typing.Delta.prims
          with Not_found ->
            Printf.fprintf stderr "not a prim : %s" f;
            assert false
        in
        ME_prim(f, List.map compile_base_expr el, List.length f_out_ty)
  in
  { mexpr_desc = desc; mexpr_type = e.cexpr_type; }

let compile_atom a =
  match a.cexpr_desc with
  | CE_const c -> Const c
  | CE_ident x -> Ident x
  | _ -> assert false

let compile_atoms a =
  match a.cexpr_desc with
  | CE_tuple el -> List.map compile_atom el
  | CE_const _ | CE_ident _ -> [compile_atom a]
  | _ -> assert false

let compile_equation
    {ceq_patt = p; ceq_expr = e}
    ((mem_acc: Imp_ast.mem),
     (init_acc: Imp_ast.init),
     (compute_acc: Imp_ast.m_equation list),
     (update_acc: (string * Imp_ast.atom) list),
     (refs_acc: Clocked_ast.typed_clocked_var list)) =
  let tvars = compile_patt p in
  match e.cexpr_desc with
  | CE_fby(c,e1) -> (* DONE *)
  begin
    match e1.cexpr_desc with
      | CE_tuple [] ->
          mem_acc, init_acc, compute_acc, update_acc, refs_acc
      | CE_tuple e1_l ->
      begin
        let (mem_acc, init_acc, update_acc, desc_list) = List.fold_left
            ( fun (mem_acc, init_acc, update_acc, desc_list) ((e, c), (s, b_ty, ck)) -> 
              let next_id = gen_next_id s in
              let mem_acc =
                { fby_mem = (next_id, b_ty, ck)::mem_acc.fby_mem;
                  node_mem = mem_acc.node_mem; }
              in
              let init_acc = 
                { fby_init = (next_id, c)::init_acc.fby_init;
                  node_init = init_acc.node_init; }
              in
              let update_acc = (next_id, compile_atom e)::update_acc in
              let desc_list = { mexpr_desc = ME_mem next_id; mexpr_type = [b_ty]; } :: desc_list in
              (mem_acc, init_acc, update_acc, desc_list)
            )
            (mem_acc, init_acc, update_acc, [])
            (List.combine (List.combine e1_l c) tvars)
        in
        let desc = ME_tuple desc_list in
        let eq = 
          { meq_patt = tvars;
            meq_expr = { mexpr_desc = desc;
                         mexpr_type = e.cexpr_type; }; }
        in
        mem_acc, init_acc, eq::compute_acc, update_acc, refs_acc
      end
      | _ -> assert false (* impossible car après normalisation cela devrait être un tuple *)
  end
      
  | CE_app(n,el) -> (* DONE *)
  begin
    let mem_id = gen_mem_id n in
    let mem_acc =
      { fby_mem = mem_acc.fby_mem;
        node_mem = (mem_id, n)::mem_acc.node_mem; }
    in
    let init_acc = 
      { fby_init = init_acc.fby_init;
        node_init = (mem_id, n)::init_acc.node_init; }
    in
    let desc = ME_app(n, mem_id, (List.map compile_base_expr el)) in
    let eq = 
      { meq_patt = tvars;
        meq_expr = { mexpr_desc = desc;
                     mexpr_type = e.cexpr_type; }; }
    in
    mem_acc, init_acc, eq::compute_acc, update_acc, refs_acc
  end

  | CE_when(e1, c, e2) ->
    begin
      let me1 =  compile_base_expr e1 in
      let me2 =  compile_base_expr e2 in
      let meq_when = {  mexpr_desc = ME_when(me1, c, me2);
                        mexpr_type = e.cexpr_type;} in
      let eq_when = {meq_patt = tvars; meq_expr = meq_when} in

      let list_of_const =
        List.map (fun t -> begin match t with
                            | Tunit -> Cunit
                            | Tbool -> Cbool true
                            | Tint -> Cint 0
                            | Tfloat -> Cfloat 0.0
                            | Tstring -> Cstring "" end) e.cexpr_type
      in
      let meq_ref =  {  mexpr_desc = ME_ref(list_of_const);
                        mexpr_type = e.cexpr_type;} in
      let eq_ref = {meq_patt = tvars; meq_expr = meq_ref} in
      let refs_acc = tvars@refs_acc in

      mem_acc, init_acc, eq_ref::eq_when::compute_acc, update_acc, refs_acc
    end
  | _ ->
      let eq = {meq_patt = tvars; meq_expr = compile_base_expr e} in
      mem_acc, init_acc, eq::compute_acc, update_acc, refs_acc


let compile_equations l =
  List.fold_right compile_equation l (empty_mem,empty_init,[],[], [])

let compile_node n =
  let input_step = n.cn_input in
  let output_step = n.cn_output in
  let (mem , init , compute , update, refs) = compile_equations n.cn_equs in
  { mn_name = n.cn_name;
    mn_input_step = input_step;
    mn_output_step = output_step;
    mn_local = n.cn_local;
    mn_mem = mem;
    mn_init = init;
    mn_compute = compute, refs;
    mn_update = update }

let compile = List.map compile_node

let gen_node_id =
  let cpt = ref 0 in
  fun s -> incr cpt ; s^"'_"^(string_of_int !cpt)

let rename_expr env e =
  match e.mexpr_desc with
  | ME_app(f,mem,args) ->
      { e with mexpr_desc = ME_app(List.assoc f env, mem, args) }
  | _ -> e

let rename_equation env eq =
  { eq with meq_expr = rename_expr env eq.meq_expr; }

let rename_node env n =
  let id = gen_node_id n.mn_name in
  let mem =
    { n.mn_mem with
      node_mem =
        List.map (fun (x,t) -> (x, List.assoc t env)) n.mn_mem.node_mem; }
  in
  let init =
    { n.mn_init with
      node_init =
        List.map
          (fun (x,f) -> (x, List.assoc f env))
          n.mn_init.node_init; }
  in
  let compute =
    List.map (rename_equation env) (fst n.mn_compute)
  in
  ((n.mn_name, id)::env),
  { n with mn_name = id;
           mn_mem = mem;
           mn_init = init;
           mn_compute = compute , (snd n.mn_compute); }

let rename_nodes f main =
  let env , f' =
    List.fold_left
      (fun (env,f) n -> let env', n' = rename_node env n in (env', n'::f))
      ([],[]) f
  in
  main := (try List.assoc !main env with Not_found -> "");
  List.rev f'
