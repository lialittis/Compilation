open Asttypes
open Ast
open Clocked_ast
open Clocked_ast_printer
open Typed_ast
open Format

module S = Set.Make(String)
module M = Map.Make(String)

(* GESTION DES ERREURS *)
type error =
  | ExpectedClockTuple of clock
  | ExpectedClockBase of clock
  | ExpectedClockVar of clock
  | ExpectedCompatible of clock * clock
  | ExpectedClockApp of clock
  | UnboundVar of string
  | UnboundNode of string
  | TooFewArguments
  | TooManyArguments
  | Clash of string
  | ConstantExpected
  | Other of string
  | FlatTuple
  | UndefinedOutputs of string list
  | InputVar of string
  | Causality
  | BadMain of clock

exception Error of location * error
exception ErrorMessage of string
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let error loc e = raise (Error (loc, e))
let errors loc s = error loc (Other s)

let report fmt = function
  | UnboundVar id -> fprintf fmt "unbound variable %s" id
  | UnboundNode id -> fprintf fmt "unbound node %s" id
  | ExpectedClockTuple ck -> 
      fprintf fmt
      "this expression is clocked %a but should be clocked as a tuple"
      print_clock ck
  | ExpectedClockVar ck -> 
      fprintf fmt
      "this expression is clocked %a but should be clocked as a variable clock"
      print_clock ck
  | ExpectedCompatible(ck1,ck2) -> 
      fprintf fmt 
      "this expression is clocked %a but should be compatible with %a"
      print_clock ck1
      print_clock ck2
  | ExpectedClockBase ck -> 
      fprintf fmt 
     "this expression is clocked %a but is expected to be as a base clock (variable clock, sampled or not base clocked"
      print_clock ck
  | ExpectedClockApp ck -> 
      fprintf fmt
      "this expression is clocked %a but should be clocked as an application"
      print_clock ck
  | Clash id -> fprintf fmt "The variable %s is defined several times" id
  | TooFewArguments -> fprintf fmt "too few arguments"
  | TooManyArguments -> fprintf fmt "too many arguments"
  | ConstantExpected -> fprintf fmt "this expression sould be a constant"
  | Other s -> fprintf fmt "%s" s
  | FlatTuple -> fprintf fmt "nested tuples are forbidden"
  | UndefinedOutputs l -> 
      fprintf fmt "those output variables are undefined:%a" 
      (fun fmt -> List.iter (fun x -> fprintf fmt "%s " x)) l
  | InputVar s -> fprintf fmt "%s is an input variable" s
  | Causality -> fprintf fmt "problem of causality"
  | BadMain ck ->
      fprintf fmt "The main node is clocked %a but is expected to be clocked (base) -> (base)" 
      print_clock ck

(* VÉRIFICATION DE LA CAUSALITÉ *)

let check_causality loc inputs equs = 
  begin try ignore (Scheduling.schedule_equs inputs equs)
  with Scheduling.Causality -> error loc Causality
  end

(* STOCKAGE DES APPELS AUX AUTRES NOEUDS
STOCKAGE DES CONDITIONS À SATISFAIRE POUR UN NOEUD *)
module Node = struct

  type t = ((string * (clock list)) list) M.t
  type t' = ((clock list -> bool) list) M.t

  let curr_node = ref ""
  let in_notclocked = ref []

  let curr_node_calls = ref []
  let curr_node_conditions = ref []
  let curr_node_fby = ref []

  let nodes_calls = ref M.empty
  let nodes_conditions = ref M.empty

  let init n =
    curr_node := n.tn_name;
    curr_node_calls := [];
    curr_node_conditions := [];
    in_notclocked := List.map (fun (s, t) -> NotClocked) n.tn_input

  let add_call call = curr_node_calls := call :: !curr_node_calls

  let add_calls () = nodes_calls := M.add !curr_node !curr_node_calls !nodes_calls

  let find_calls name = try M.find name !nodes_calls with Not_found -> []

  let add_condition cond = curr_node_conditions := cond :: !curr_node_conditions

  let add_conditions () = nodes_conditions := M.add !curr_node !curr_node_conditions !nodes_conditions

  let find_condition name = try M.find name !nodes_conditions with Not_found -> []

  let add_fby v = curr_node_fby := v :: !curr_node_fby

  let is_fby v = List.mem v !curr_node_fby

  let rem_fby v = curr_node_fby := List.filter (fun s -> v <> s) !curr_node_fby

end

(* SÉPARATION DES INPUT DE L'ENSEMBLE DES VARIABLES *)
type io = Vinput | Vpatt

(* ASSOCIATION DES VARIABLES À LEUR CLOCK *)
module VarClock = struct

  type t = (clock * io) M.t

  let var_to_clock_init = ref M.empty
  let var_to_clock_next = ref M.empty

  let change_clock_init loc v ck = 
    if M.mem v !var_to_clock_init
    then var_to_clock_init := M.update v (fun c -> Some (ck,Vpatt)) !var_to_clock_init
    else error loc (UnboundVar v)  

  let change_clock_next loc v ck = 
    if M.mem v !var_to_clock_next
    then var_to_clock_next := M.update v (fun c -> Some (ck,Vpatt)) !var_to_clock_next
    else error loc (UnboundVar v)  

  let add loc v io inputs = 
    if M.mem v !var_to_clock_init then error loc (Clash v);
    if M.mem v !var_to_clock_next then error loc (Clash v);
    match io with
    | Vpatt ->
        var_to_clock_init := M.add v (NotClocked, Vpatt) !var_to_clock_init;
        var_to_clock_next := M.add v (NotClocked, Vpatt) !var_to_clock_next
    | Vinput ->
        let input_ck = (ClockBase
                          (ClockVar 
                            ((fun ckl ->
                                try match List.assoc v (List.combine (List.map (fun (s, _) -> s) inputs) ckl) with
                                | NotClocked -> NotClockedBase
                                | ClockBase ck -> ck
                                | ck ->  error loc (ExpectedClockBase ck)
                                with 
                                  | Not_found -> error loc (UnboundVar v)
                                  | Invalid_argument(_) -> error loc (Other ("clock checker : the clock-function of the input " ^ v ^ " of the indicated node has a problem."))
                              ),
                            List.length inputs)
                          ),
                        Vinput)
        in
        var_to_clock_init := M.add v input_ck !var_to_clock_init;
        var_to_clock_next := M.add v input_ck !var_to_clock_next    

  let adds loc inputs outputs locals = 
    List.iter (fun (v, _) -> add loc v Vinput inputs) inputs;
    List.iter (fun (v, _) -> add loc v Vpatt  inputs) outputs;
    List.iter (fun (v, _) -> add loc v Vpatt  inputs) locals

  let init n = 
    var_to_clock_init := M.empty;
    var_to_clock_next := M.empty;
    adds n.tn_loc n.tn_input n.tn_output n.tn_local

  let find_init loc v = try M.find v !var_to_clock_init with Not_found ->  error loc (UnboundVar v)

  let find_next loc v = try M.find v !var_to_clock_next with Not_found ->  error loc (UnboundVar v)
end

(* ASSOCIATION DES VARIABLES À LEUR ÉQUATION DE DÉFINITION *)
module VarEq = struct

  type t = t_equation M.t

  let var_to_eq = ref M.empty

  let add loc eq = 
    let vl =  eq.teq_patt.tpatt_desc in
    var_to_eq := List.fold_left
      (fun env v ->
        if M.mem v env then error loc (Clash v) else M.add v eq env)
      !var_to_eq vl;

    match eq.teq_expr.texpr_desc with
      | TE_fby(_,_) ->
          List.iter (fun v -> Node.add_fby v) vl;
          List.iter (fun v -> VarClock.change_clock_init loc v (ClockBase (ClockVar ((fun ckl ->  Base), List.length !Node.in_notclocked)))) vl
      | _ -> ()

  let adds loc eql = 
    List.iter (fun eq -> add loc eq) eql

  let init n = 
    var_to_eq := M.empty;
    adds n.tn_loc n.tn_equs

  let find loc v = try 
    M.find v !var_to_eq
  with Not_found ->  error loc (UnboundVar v)
end


(* CHECK SI LES CLOCK SONT COMPATIBLES *)
let rec normalize_clock ck = (* enlève le ClockTuple en trop pour les variables *)
  begin match ck with
    | ClockBase bck -> ClockBase (normalize_base_clock bck)
    | ClockTuple [ck'] -> normalize_clock ck'
    | _ -> ck
  end

and normalize_base_clock bck =
  begin match bck with
    | Sampled (ClockVar (f, i), c, e) -> ClockVar ((fun ckl -> Sampled (normalize_base_clock (f ckl), c, e)), i)
    | _ -> bck
  end

let rec compatible_base add actual_ck expected_ck inputs low_clock =
  begin match normalize_base_clock actual_ck, normalize_base_clock expected_ck with

    | ClockVar (f1, i1), ClockVar (f2, i2) when add -> 
      Node.add_condition (fun ckl -> 
        compatible_base false (f1 ckl) (f2 ckl) ckl low_clock);

      compatible_base false (f1 inputs) (f2 inputs) inputs low_clock

    | ClockVar (f1, i1), ClockVar (f2, i2) -> 
      compatible_base false (f1 inputs) (f2 inputs) inputs low_clock

    | Base, _ -> true

    | Sampled (ck1, c1, ce1), Sampled (ck2, c2, ce2)
    when (compatible_base add ck1 ck2 inputs low_clock) && (c1 = c2) -> true

    | ck, Sampled (ck',_ ,_ )-> compatible_base add ck ck' inputs low_clock
    | NotClockedBase, NotClockedBase -> true 
    | NotClockedBase, ck -> not low_clock 
    | ck, NotClockedBase -> low_clock 
    | _, _ -> actual_ck = expected_ck
  end

(* expected_ck <= actual_ck *)
and compatible add actual_ck expected_ck inputs low_clock = 
  begin match normalize_clock actual_ck, normalize_clock expected_ck with
    | ClockTuple ckl1, ClockTuple ckl2 ->
      begin try List.fold_left2
          (fun well_ck ac_ck ex_ck -> well_ck && (compatible add ac_ck ex_ck inputs low_clock))
          true ckl1 ckl2
      with Invalid_argument _ -> false end
    | ClockBase bck1, ClockBase bck2 -> compatible_base add bck1 bck2 inputs low_clock
    | NotClocked, NotClocked -> true 
    | NotClocked, ck -> not low_clock 
    | ck, NotClocked -> low_clock
    | ck1, ck2 -> false 
  end

let rec lowest_clock ckl = match ckl with
  | [ck] -> ck
  | ck::ckl ->  let ck' = lowest_clock ckl in
                if compatible false ck ck' !Node.in_notclocked true then ck' else
                (if compatible false ck' ck !Node.in_notclocked true then ck else
                error dummy_loc (ExpectedCompatible (ck, ck')))
  | [] -> ClockBase (ClockVar ((fun _ -> Base), 0))


(* GESTION DES FONCTIONS PRIMITIVES ET NOEUDS *)
module Delta = struct
  let one_one =
    ClockApp (begin fun ckl -> 
    match ckl with
      | [ck] -> ClockTuple [ck]
      | [] -> error dummy_loc TooFewArguments
      | _ -> error dummy_loc TooManyArguments
    end,
    1)

  let one_two = 
    ClockApp (begin fun ckl -> 
    match ckl with
      | [ck] -> ClockTuple [ck; ck]
      | [] -> error dummy_loc TooFewArguments
      | _ -> error dummy_loc TooManyArguments
    end,
    1)

  let two_one = 
    ClockApp (begin fun ckl -> 
    match ckl with
      | [ck1; ck2] -> ClockTuple [lowest_clock [ck1; ck2]]
      | [] | [_] -> error dummy_loc TooFewArguments
      | _ -> error dummy_loc TooManyArguments
    end,
    2)

  let three_one = 
    ClockApp (begin fun ckl -> 
    match ckl with
      | [ck1; ck2; ck3] -> ClockTuple [lowest_clock [ck1; ck2; ck3]]
      | [] | [_] | [_;_] -> error dummy_loc TooFewArguments
      | _ -> error dummy_loc TooManyArguments
    end,
    3)

  let four_one = 
    ClockApp (begin fun ckl -> 
    match ckl with
      | [ck1; ck2; ck3; ck4] -> ClockTuple [lowest_clock [ck1; ck2; ck3; ck4]]
      | [] | [_] | [_;_] | [_;_;_] -> error dummy_loc TooFewArguments
      | _ -> error dummy_loc TooManyArguments
    end,
    4)

  let prims = [
    "int_of_float", one_one ;
    "float_of_string", one_one ;
    "int_of_string", one_one ;
    "bool_of_string", one_one ;
    "float_of_int", one_one ;
    "read", one_one ;
    "random_int", one_one ;
    "random_float", one_one ;
    "cos", one_one ;
    "sin", one_one ;
    "draw_point", two_one ;
    "draw_line", four_one ;
    "draw_circle", three_one ;
    "draw_rect", four_one ;
    "fill_rect", four_one ;
    "get_mouse", one_two ;
    "print_endline", one_one;
    "string_of_int", one_one ]
    
  let nodes = Hashtbl.create 97
      
  let is_primitive f = List.mem_assoc f prims

  let is_print f = (f = "print")

  let find n = 
    try Hashtbl.find nodes n , false with 
  Not_found -> List.assoc n prims , true

  let add = Hashtbl.replace nodes

  let save () = Hashtbl.fold (fun key ck env -> (key,ck)::env) nodes []
end

(* ANNOTATION AVEC DES CLOCK *)
module Clocking = struct

  (* ANNOTATION DES PATTERN ET EXPRESSION AVEC DES CLOCK *)
  let rec clock_patt p = 
    let ckl = List.map (clock_patt_var p.tpatt_loc) p.tpatt_desc in
    { cpatt_desc = p.tpatt_desc;
      cpatt_type = p.tpatt_type;
      cpatt_clock = ClockTuple ckl;
      cpatt_loc = p.tpatt_loc; }

  and clock_expr e =
    let desc, ck = clock_expr_desc e.texpr_loc e.texpr_desc in
    { cexpr_desc = desc;
      cexpr_type = e.texpr_type;
      cexpr_clock = ck;
      cexpr_loc = e.texpr_loc; }

  and clock_patt_var loc v =
    try match VarClock.find_init loc v with
      | NotClocked , Vpatt ->
        begin
          let eq = VarEq.find loc v in
          let p_desc = eq.teq_patt.tpatt_desc in
          let e_desc = eq.teq_expr.texpr_desc in
          let eq_loc = eq.teq_patt.tpatt_loc in
          let _, exp_ck = clock_expr_desc eq_loc e_desc in
          match exp_ck with
            | ClockTuple ckl ->
              List.iter (fun (v, ck) -> VarClock.change_clock_init loc v (normalize_clock ck)) (List.combine p_desc ckl);
              List.iter (fun (v, ck) -> VarClock.change_clock_next loc v (normalize_clock ck)) (List.combine p_desc ckl);
              normalize_clock (List.assoc v (List.combine p_desc ckl))
            | _ -> error eq_loc (ExpectedClockTuple exp_ck)
        end

      | ck , _  when Node.is_fby v -> Node.rem_fby v;
        begin match VarClock.find_next loc v with
          | NotClocked , Vpatt ->
              begin
                let eq = VarEq.find loc v in
                let p_desc = eq.teq_patt.tpatt_desc in
                let e_desc = eq.teq_expr.texpr_desc in
                let eq_loc = eq.teq_patt.tpatt_loc in
                let _, exp_ck = clock_expr_desc eq_loc e_desc in
                match exp_ck with
                  | ClockTuple ckl ->
                    List.iter (fun (v, ck) -> VarClock.change_clock_next loc v (normalize_clock ck)) (List.combine p_desc ckl);
                    ck
                  | _ -> error eq_loc (ExpectedClockTuple exp_ck)
              end
          | _, _ -> ck
        end

      | ck, _ -> ck
    with Not_found -> error loc (UnboundVar v)

  and clock_expr_desc loc desc = 
    match desc with
    | TE_const c ->
        CE_const c , ClockTuple [ClockBase (ClockVar ((fun ckl -> Base), List.length !Node.in_notclocked))]

    | TE_ident x ->
        let ck = clock_patt_var loc x in
        CE_ident x , ClockTuple [normalize_clock ck]

    | TE_unop (op, e) ->
        let ce = clock_expr e in
        CE_unop (op, ce) , ce.cexpr_clock

    | TE_binop (op, e1, e2) ->
        let ce1 = clock_expr e1 in
        let ce2 = clock_expr e2 in
        let ckl = [ce1.cexpr_clock; ce2.cexpr_clock] in 
        let ck = lowest_clock ckl in
        CE_binop (op, ce1, ce2), ck 

    | TE_prim (f, el) | TE_app (f, el) -> 
        begin try 
          let ck , is_prim = Delta.find f in 
          match ck with 
          | ClockApp(ckf, i) when i = (List.length el) ->
          begin
            let cel = List.map clock_expr el in
            let actual_clocks = List.map (fun ce -> normalize_clock ce.cexpr_clock) cel in
            Node.add_call (f, actual_clocks);
            let ck_out = (try ckf actual_clocks with  ErrorMessage s -> error loc (Other s)) in
            (if is_prim then CE_prim(f, cel) else CE_app(f, cel)), ck_out
          end
          | ClockApp(ckf, i) when i > (List.length el) -> error loc TooFewArguments
          | ClockApp(ckf, i) when i < (List.length el) -> error loc TooManyArguments
          | _ -> error loc (ExpectedClockApp ck)
        with Not_found -> error loc (UnboundNode f)
        end

    | TE_print el ->
        let cel = List.map clock_expr el in
        CE_print(cel), (ClockTuple (List.map (fun ce -> normalize_clock ce.cexpr_clock) cel))

    | TE_if (e1, e2, e3) ->
        let ce1 = clock_expr e1 in
        let ce2 = clock_expr e2 in
        let ce3 = clock_expr e3 in
        let ck = ce1.cexpr_clock in
        begin match ck, ce2.cexpr_clock, ce3.cexpr_clock with
          
          | ClockTuple [ClockBase (ClockVar (f, i))], ClockTuple ckl1, ClockTuple ckl2 
            when (List.fold_left (fun b ck1 -> b && (compatible true ck1 (ClockBase (ClockVar ((fun ckl -> Sampled (f ckl, Cbool true, ce1)), i))) !Node.in_notclocked false)) true ckl1)
            && (List.fold_left (fun b ck2 -> b && (compatible true ck2 (ClockBase (ClockVar ((fun ckl -> Sampled (f ckl, Cbool false, ce1)), i))) !Node.in_notclocked false)) true ckl2)
            ->
              CE_if (ce1, ce2, ce3), ClockTuple (List.fold_left (fun l _ -> ClockBase (ClockVar (f, i)) :: l) [] ckl2)
          
          | ClockTuple [ClockBase (ClockVar (f, i))], ClockTuple ckl1, ClockTuple ckl2 
            when (List.fold_left (fun b ck2 -> b && (compatible true ck2 (ClockBase (ClockVar ((fun ckl -> Sampled (f ckl, Cbool false, ce1)), i))) !Node.in_notclocked false)) true ckl2)
            -> 
              error e2.texpr_loc (ExpectedCompatible (ClockTuple ckl1, (ClockBase (ClockVar ((fun ckl -> Sampled (f ckl, Cbool true, ce1)), i)))))
          
          | ClockTuple [ClockBase (ClockVar (f, i))], ck1, ck2
            ->
              error e3.texpr_loc (ExpectedCompatible (ck2, (ClockBase (ClockVar ((fun ckl -> Sampled (f ckl, Cbool false, ce1)), i)))))
          
          | _, _, _ -> error loc (ExpectedClockVar ck)
        end

    | TE_fby (e1, e2) ->
        let ce2 = clock_expr e2 in
        CE_fby (e1, ce2), ce2.cexpr_clock
    
    | TE_tuple el ->
        let cel = List.map clock_expr el in
        CE_tuple cel, 
        (ClockTuple (List.map (fun e -> normalize_clock e.cexpr_clock) cel))

    | TE_when (e1, c, e2) ->
        let ce1 = clock_expr e1 in
        let ce2 = clock_expr e2 in
        let ck1 = ce1.cexpr_clock in
        let ck2 = ce2.cexpr_clock in
        begin match ck1, ck2 with
        | ClockTuple [ClockBase (ClockVar (f1, i1))],  ClockTuple [ClockBase (ClockVar (f2, i2))] when compatible true ck1 ck2 !Node.in_notclocked false ->
          CE_when (ce1, c, ce2), ClockTuple [ClockBase (ClockVar ((fun ckl -> Sampled (normalize_base_clock(f1 ckl), c, ce2)), i1))]
        | ClockTuple [ClockBase (ClockVar (f1, i1))],  ClockTuple [ClockBase (ClockVar (f2, i2))] -> 
          error loc (ExpectedCompatible (ck1, ck2))
        | _, _ -> error loc (ExpectedClockVar ck1)
        end

    | TE_merge (e, c_e_list) -> 
        let ce = clock_expr e in
        let ck = ce.cexpr_clock in
        match ck with
        | ClockTuple [ClockBase (ClockVar (f, i))] ->
          begin
            let c_ce_list =
              List.map
              (fun (c, e') ->
                let ce' = clock_expr e' in
                let ck' = ce'.cexpr_clock in
                if compatible true ck' (ClockTuple [ClockBase (ClockVar ((fun ckl -> Sampled (f ckl, c, ce)), i))]) !Node.in_notclocked false
                then (c, ce')
                else error e'.texpr_loc (ExpectedCompatible (ck', ClockTuple [ClockBase (ClockVar ((fun ckl -> Sampled (f ckl, c, ce)), i))]))
              )
              c_e_list
            in
            CE_merge (ce, c_ce_list), ck
          end
        | _ ->  error loc (ExpectedClockVar ck)


  (* ANNOTATION DES ÉQUATIONS AVEC DES CLOCK *)
  let clock_equations eql = List.fold_left
    (fun acc eq ->  let expr = clock_expr eq.teq_expr in
                    let patt = clock_patt eq.teq_patt in
                    { ceq_patt = patt; ceq_expr = expr; }::acc) [] eql

  (* ANNOTATION DES NOEUDS AVEC DES CLOCK *)
  let clock_node n =
    Node.init n;
    VarClock.init n;
    VarEq.init n;

    let equs = clock_equations n.tn_equs in

    Node.add_conditions ();
    Node.add_calls ();

    let typed_clocked_var_in = List.map 
      (fun (s, ty) ->
        let ck, _ = VarClock.find_next n.tn_loc s in
        (s, ty, ClockTuple [normalize_clock ck]))
      n.tn_input
    in
    let typed_clocked_var_local = List.map 
      (fun (s, ty) ->
        let ck, _ = VarClock.find_next n.tn_loc s in
        (s, ty, ClockTuple [normalize_clock ck]))
      n.tn_local
    in
    let typed_clocked_var_out = List.map 
      (fun (s, ty) ->
        let ck, _ = VarClock.find_next n.tn_loc s in
        (s, ty, ClockTuple [normalize_clock ck])
      )
      n.tn_output
    in
    let c_out = List.map (fun (_, _, ck) -> normalize_clock ck) typed_clocked_var_out in
    
    
    let node_clock = ClockApp 
      ((fun ckl ->
        ClockTuple 
        (List.map 
          (fun ck -> match ck with 
            | ClockBase ClockVar (f, i) -> 
              begin match f ckl with 
                | Base -> lowest_clock ckl
                | ck -> ClockBase ck
              end
            | _ -> ck) (* quand c'est une constante c'est base*)
        c_out)),
      List.length (n.tn_input))
    in
        
    check_causality n.tn_loc typed_clocked_var_in equs;
    Delta.add n.tn_name node_clock;
    { cn_name = n.tn_name;
      cn_input = typed_clocked_var_in;
      cn_output = typed_clocked_var_out;
      cn_local = typed_clocked_var_local; 
      cn_equs = equs;
      cn_loc = n.tn_loc; }
end

let check_main fc main =
  let ck, is_prim = 
    try Delta.find main with Not_found -> error dummy_loc (UnboundNode main)
  in
  match ck, is_prim with
  | ClockApp (f, 1), false ->
      if not (compatible false (f [ClockBase Base]) (ClockTuple [ClockBase Base]) [ClockBase Base] false) then(
            let n = List.find (fun n -> n.cn_name = main) (List.rev fc) in
            error n.cn_loc (BadMain ck))
  | _ -> errors dummy_loc "The main node cannot be a primitive function"


let check_file main = 
  let match_node_conditions node inputs =
    let rec check_inputs_conditions cond_list = match cond_list with
      | [] -> true
      | cond::tail ->  (cond inputs) && (check_inputs_conditions tail) 
    in
    check_inputs_conditions (Node.find_condition node)
  in

  let rec check_node node node_inputs = 
    check_conditions node node_inputs (Node.find_calls node)

  and check_conditions node node_inputs node_calls = match node_calls with
    | [] -> true
    | (node_h, clocks)::nodes_t ->
      let node_h_inputs = List.map 
        (fun ck -> match ck with
          | ClockBase ClockVar (f, i) -> ClockBase (f node_inputs)
          | _ -> ck) (* quand c'est une constante c'est base*)
        clocks
      in

      (check_node node_h node_h_inputs) &&
      (check_conditions node node_inputs nodes_t) &&
      (match_node_conditions node_h node_h_inputs)
  in

  check_node main [ClockBase Base] 

let clock_file ft main =
  let fc = List.map Clocking.clock_node ft in
  if main <> "" then check_main fc main;
  if (check_file main) then fc else errors dummy_loc "The file is not well clocked, be carful when calling nodes"