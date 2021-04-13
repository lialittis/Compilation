open Asttypes
open Typed_ast
open Imp_ast

let rec untulify el =
  List.fold_right
    (fun e acc ->
      match e.texpr_desc with
      | TE_tuple el -> (untulify el @ acc)
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

let compile_patt {tpatt_desc = d ; tpatt_type = t} =
  try List.combine d t with Invalid_argument _ -> assert false

let rec compile_base_expr e =
  let desc =
    match e.texpr_desc with
    | TE_const c -> ME_const c
    | TE_ident x -> ME_ident x
    | TE_unop (op, e) -> ME_unop(op, compile_base_expr e)
    | TE_binop (op, e1, e2) ->
        let ce1 =  compile_base_expr e1 in
        let ce2 =  compile_base_expr e2 in
        ME_binop (op, ce1, ce2)
    | TE_if (e1, e2, e3) ->
		(*assert false*)
		let ce1 = compile_base_expr e1 in
		let ce2 = compile_base_expr e2 in
		let ce3 = compile_base_expr e3 in
		ME_if (ce1,ce2,ce3)
		
		(* DONE *)
    | TE_tuple el -> ME_tuple (List.map compile_base_expr el)
    | TE_print el -> ME_print (List.map compile_base_expr el)
    | TE_fby _ -> assert false (* impossible car en forme normale *)
    | TE_app _ -> assert false (* impossible car en forme normale *)
    | TE_prim(f, el) ->
        let _, f_out_ty =
          try List.assoc f Typing.Delta.prims
          with Not_found ->
            Printf.fprintf stderr "not a prim : %s" f;
            assert false
        in
        ME_prim(f, List.map compile_base_expr el, List.length f_out_ty)
  in
  { mexpr_desc = desc; mexpr_type = e.texpr_type; }

let compile_atom a =
  match a.texpr_desc with
  | TE_const c -> Const c
  | TE_ident x -> Ident x
  | _ -> assert false

let compile_atoms a =
  match a.texpr_desc with
  | TE_tuple el -> List.map compile_atom el
  | TE_const _ | TE_ident _ -> [compile_atom a]
  | _ -> assert false

let compile_equation
    {teq_patt = p; teq_expr = e}
    ((mem_acc: Imp_ast.mem),
     (init_acc: Imp_ast.init),
     (compute_acc: Imp_ast.m_equation list),
     (update_acc: (string * Imp_ast.atom) list)) =
  let tvars = compile_patt p in
  match e.texpr_desc with
  | TE_fby(c,e1) ->
	(* mem_acc, init_acc, compute_acc, update_acc*)	
	begin
      match e1.texpr_desc with
      | TE_tuple [] ->
        mem_acc, init_acc, compute_acc, update_acc
      | TE_tuple e1_l ->
      begin
        let ce1 = compile_base_expr e1 (*return type is m_expr*) in
        let para1 = (mem_acc,init_acc,update_acc,[]) in
        let para2 = (List.combine (List.combine e1_l c) tvars) in
        (*note that tvars is type {tpatt_desc,tpatt_type}*)
        let (new_mem_acc, new_init_acc, new_update_acc, cfby) = List.fold_left
            ( fun (mem_acc, init_acc, update_acc,cfby) ((e,c),(d,t)) ->
              let init_name = gen_next_id d in
              let new_fby_name = (init_name, t) in
              let new_mem_acc = {fby_mem = new_fby_name::mem_acc.fby_mem ; node_mem = mem_acc.node_mem;} in
              let new_init_name = (init_name, c) in
              let new_init_acc = {fby_init=new_init_name::init_acc.fby_init;node_init=init_acc.node_init;} in
              let new_atom = (init_name,compile_atom e) in
              let new_update_acc = new_atom::update_acc in
              let cfby =  { mexpr_desc = ME_mem init_name; mexpr_type = [t]}::cfby in
              (new_mem_acc, new_init_acc, new_update_acc,cfby)
            ) para1 para2
        in
        let eq = {meq_patt = tvars; meq_expr = {mexpr_desc = ME_tuple cfby ; mexpr_type = e.texpr_type}} in
        new_mem_acc, new_init_acc, eq::compute_acc,new_update_acc
      end
      | _ -> assert false
    end

      (* Old Version*)
(*
	  (* compile the base expression e1 and e2*)
	  (* note that here e1 is const list and e2 is t_expr *)
	 
      (*init only need the initial value which means e1*)
      let ce2 = compile_base_expr e2 (*return type is m_expr*) in
      let init_names = List.map (fun m_const -> gen_next_id "fby") e1 in
	  let add_fby_init = List.combine init_names e1 in

(*
      let add_fby_mem1 = List.combine init_names e.texpr_type in
      let add_fby_mem = List.append (List.map (fun m_type -> (gen_next_id "x", m_type)) ce2.mexpr_type) add_fby_mem1 in 

*)
      let add_fby_mem = List.combine init_names e.texpr_type in
      
	  let new_mem_acc = {fby_mem = List.append add_fby_mem mem_acc.fby_mem; node_mem = mem_acc.node_mem} in
	  let new_init_acc = {fby_init = List.append add_fby_init init_acc.fby_init; node_init = init_acc.node_init} in
	  (*let eq2 = {meq_patt = new_mem_acc.fby_mem; meq_expr = ce2 } in*)
      let cfby =  { mexpr_desc = 
        ME_tuple(List.map (fun e' -> {mexpr_desc = ME_ident(e'); mexpr_type = ce2.mexpr_type}) init_names); mexpr_type = ce2.mexpr_type} in
      let ce1 = { mexpr_desc = 
        ME_tuple(List.map (fun e' -> {mexpr_desc = ME_const(e'); mexpr_type = ce2.mexpr_type}) e1); mexpr_type = ce2.mexpr_type} in
	  let eq2 = {meq_patt = tvars; meq_expr = ce2} in
	  let eq1 = {meq_patt = tvars; meq_expr = ce1} in
      let eq = {meq_patt = tvars; meq_expr = cfby} in
	  (* a problem here, because the eq need the value of init fby*)
	  let ae2 = compile_atoms e2 in
	  let update_ae2 = List.combine init_names ae2 in
      let new_update_acc = List.append update_ae2 update_acc in
      
	  (*
	  let ae2 = compile_atom e2 in
	  let update_ae2 = ("fby", ae2) in
	  *)

	  new_mem_acc, new_init_acc, eq::compute_acc, new_update_acc (* DONE *)
	end
*)


  | TE_app(n,el) ->
	(*mem_acc, init_acc, compute_acc, update_acc*)
	
	begin
      (* generate a new node mem*)
      let mem_name = gen_mem_id n in
	  let new_node_mem = (mem_name, n) in
	  (* adding it to the record of mem_acc *)
	  let new_mem_acc = {fby_mem = mem_acc.fby_mem ; node_mem = new_node_mem::(mem_acc.node_mem) } in
	  let new_el = List.map (fun e1 -> compile_base_expr e1) el in
      let desc = ME_app(n,mem_name, new_el) in
      let new_eq = {meq_patt = tvars; meq_expr = { mexpr_desc = desc ; mexpr_type = e.texpr_type;} ;} in
(*      let new_eq_l = List.map (fun e -> {meq_patt = tvars; meq_expr = e;}) new_el in *)
      let new_init_acc = {fby_init = init_acc.fby_init ; node_init = new_node_mem::init_acc.node_init} in
      let new_compute_acc = new_eq::compute_acc in
      
      (* To update the update_acc variable*)
(*
      let ael = List.map compile_atom el in 
      (*List.map (fun e1 -> compile_atoms e1) el in*)
	  let update_ael = List.map (fun atom -> (mem_name, atom)) ael  in
	  (*let update_ael = List.map (fun ae -> List.map (fun atom -> (n, atom)) ae ) ael in*)

      (*(*not used*)let new_update_acc = List.map (fun ae -> (mem_name,ae)) update_ael in*)
      let new_update_acc = List.append update_ael update_acc in
*)
      new_mem_acc, new_init_acc, new_compute_acc, update_acc (* DONE *)
	end
	

  | _ ->
      let eq = {meq_patt = tvars; meq_expr = compile_base_expr e} in
      mem_acc, init_acc, eq::compute_acc, update_acc


let compile_equations l =
  List.fold_right compile_equation l (empty_mem,empty_init,[],[])

let compile_node n =
  let input_step = n.tn_input in
  let output_step = n.tn_output in
  let (mem , init , compute , update) = compile_equations n.tn_equs in
  { mn_name = n.tn_name;
    mn_input_step = input_step;
    mn_output_step = output_step;
    mn_local = n.tn_local;
    mn_mem = mem;
    mn_init = init;
    mn_compute = compute;
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
    List.map (rename_equation env) n.mn_compute
  in
  ((n.mn_name, id)::env),
  { n with mn_name = id;
           mn_mem = mem;
           mn_init = init;
           mn_compute = compute; }

let rename_nodes f main =
  let env , f' =
    List.fold_left
      (fun (env,f) n -> let env', n' = rename_node env n in (env', n'::f))
      ([],[]) f
  in
  main := (try List.assoc !main env with Not_found -> "");
  List.rev f'
