open Clocked_ast

exception Causality


module S = Set.Make(String)
module Graph = Set.Make(
  struct
    type t = string * S.t * c_equation
    let compare (x1,s1,_) (x2,s2,_) =
      let c = compare x1 x2 in
      if c<>0 then c else S.compare s1 s2
  end)


(** [add_vars_of_patt s patt] ajoute à l'ensemble [s] les variables
    introduites par le motif [patt]. *)
let add_vars_of_patt s {cpatt_desc=p} =
  List.fold_left (fun s x -> S.add x s) s p


(** [add_vars_of_exp s exp] ajoute à l'ensemble [s] les variables
    dont l'expression [exp] dépend instantanément. *)
let rec add_vars_of_exp s {cexpr_desc=e} =
  match e with
  | CE_const _ -> s
  | CE_ident x -> S.add x s (* DONE *)
  | CE_fby (c, e1) -> s (* DONE *)
  | CE_unop (_,e') -> add_vars_of_exp s e'
  | CE_binop (_,e1,e2) ->
      let s = add_vars_of_exp s e1 in
      add_vars_of_exp s e2
  | CE_if(e1,e2,e3) -> (* DONE *)
      let s = add_vars_of_exp s e1 in
      let s = add_vars_of_exp s e2 in
      add_vars_of_exp s e3
  | CE_app (_,l) | CE_prim (_,l) | CE_print l ->
      List.fold_left add_vars_of_exp s l
  | CE_tuple l -> List.fold_left add_vars_of_exp s l
  | CE_when (e1, c, e2) -> 
      let s = add_vars_of_exp s e1 in
      add_vars_of_exp s e2
  | CE_merge (e, c_e_list) -> 
      let s = add_vars_of_exp s e in
      List.fold_left (fun s (_, e) -> add_vars_of_exp s e) s c_e_list


let schedule_equs inputs equs =
  (* Construction du graphe de dépendance entre les variables. *)
  let g =
    List.fold_left
      (fun g eq ->
	 let vp = add_vars_of_patt S.empty eq.ceq_patt in
	 let ve = add_vars_of_exp S.empty eq.ceq_expr in
	 S.fold (fun x g -> Graph.add (x,ve,eq) g) vp g)
      Graph.empty equs
  in
  (* Suppression des dépendances aux entrées. *)
  let g =
    let s_inputs =
      List.fold_left (fun acc (x, _, _) -> S.add x acc) S.empty inputs
    in
    Graph.fold
      (fun (y,s,e) g -> Graph.add (y,S.diff s s_inputs,e) g)
      g
      Graph.empty
  in
  (* Tri topologique des equations *)
  let rec exists_loop topo g =
    if Graph.is_empty g then List.rev topo
    else
      let g1 , g2 = Graph.partition (fun (_,s,_) -> S.is_empty s) g in
      if Graph.is_empty g1 then raise Causality;
      let sv =
        Graph.fold (fun (x,_,_) s -> S.add x s) g1 S.empty
      in
      let g =
	Graph.fold
          (fun (y,s,e) g -> Graph.add (y,S.diff s sv,e) g)
          g2 Graph.empty
      in
      let topo =
	Graph.fold
          (fun (_,_,e) l -> if List.mem e l then l else e::l)
          g1 topo
      in
      exists_loop topo g
  in
  exists_loop [] g

let schedule_node n =
  let equs = schedule_equs n.cn_input n.cn_equs in
  { n with cn_equs = equs; }

let schedule =
  List.map schedule_node
