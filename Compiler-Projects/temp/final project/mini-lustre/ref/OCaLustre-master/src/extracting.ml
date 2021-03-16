open Parsetree
open Asttypes
open Longident
open Parsing_ast
open Imperative_ast
open Clocking_ast
open Compiling
open Ast_helper

let str_of_pattern p =
  match p.p_desc with
  | Ident v -> v
  | _ -> failwith "not a variable"

let lid_of_ident ?(prefix="") ?(suffix="") i =
  {
    txt = Lident (prefix^i^suffix);
    loc = Location.none
  }

let rec tocaml_imperative_updates e el =
  let rec loop l =
    match l with
    | [] -> e
    | (e1,e2)::t ->
      let e1 = tocaml_expression e1 in
      let e2 = tocaml_expression e2 in
      Exp.sequence [%expr [%e e].([%e e1]) <- [%e e2]] (loop t)
  in
  loop el
and tocaml_expression e =
  match e with
  | IValue (String s) -> Ast_convenience.str s
  | IValue (Nil) -> [%expr Obj.magic () ]
  | IValue (Enum s) -> Ast_convenience.constr s []
  | IValue (Integer i) -> Ast_convenience.int i
  | IValue (Float f) -> Ast_convenience.float f
  | IValue (Bool true) -> Ast_convenience.constr "true" []
  | IValue (Bool false) ->  Ast_convenience.constr "false" []
  | IETuple t -> Ast_convenience.tuple (List.map (fun i -> tocaml_expression i) t)
  | IVariable i -> Ast_convenience.evar i
  | IArray el -> Exp.array (List.map tocaml_expression el)
  | IArray_fold (e,f,acc) ->
    [%expr Array.fold_left [%e f] [%e tocaml_expression acc] ([%e tocaml_expression e]) ]
  | IArray_map (e,f) ->
    [%expr Array.map [%e f] [%e tocaml_expression e] ]
  | IArray_get (e,e') ->
    [%expr [%e tocaml_expression e].([%e tocaml_expression e']) ]
  | IImperative_update (e,pe) ->
    let e = tocaml_expression e in
    [%expr [%e tocaml_imperative_updates e pe] ]
  | IRef i -> [%expr ![%e Exp.ident (lid_of_ident i) ]  ]
  | IRefDef e -> [%expr ref [%e tocaml_expression e ] ]
  | IPrefixOp (INot, e) -> [%expr not [%e tocaml_expression e ] ]
  | IPrefixOp (INeg, e) -> [%expr ~- [%e tocaml_expression e ] ]
  | IPrefixOp (INegf, e) -> [%expr ~-. [%e tocaml_expression e ] ]
  | IInfixOp (IDiff,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] <> [%e tocaml_expression e2 ]]
  | IInfixOp (IEquals,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] = [%e tocaml_expression e2 ]]
  | IInfixOp (IMod,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] mod [%e tocaml_expression e2 ]]
  | IInfixOp (IPlus,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] + [%e tocaml_expression e2 ]]
  | IInfixOp (IMinus,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] - [%e tocaml_expression e2 ]]
  | IInfixOp (ITimes,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] * [%e tocaml_expression e2 ]]
  | IInfixOp (IDiv,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] / [%e tocaml_expression e2 ]]
  | IInfixOp (IPlusf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] +. [%e tocaml_expression e2 ]]
  | IInfixOp (IMinusf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] -. [%e tocaml_expression e2 ]]
  | IInfixOp (ITimesf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] *. [%e tocaml_expression e2 ]]
  | IInfixOp (IDivf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ]  /. [%e tocaml_expression e2 ]]
  | IInfixOp (IInf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] < [%e tocaml_expression e2 ]]
  | IInfixOp (IInfe,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] <= [%e tocaml_expression e2 ]]
  | IInfixOp (ISup,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] > [%e tocaml_expression e2 ]]
  | IInfixOp (ISupe,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] >= [%e tocaml_expression e2 ]]
  | IInfixOp (IOr,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] || [%e tocaml_expression e2 ]]
  | IInfixOp (IAnd,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] && [%e tocaml_expression e2 ]]
  | IApplication (id, num, e) ->
    let e' = tocaml_expression e in
    let n = string_of_int num in
    [%expr [%e (Exp.ident (lid_of_ident (id^n^"_step")))] [%e e' ]]
  | IApplication_init (id,e) ->
    let e' = tocaml_expression e in
    [%expr [%e (Exp.ident (lid_of_ident (id)))] [%e e' ]]
  | IAlternative (e1,e2,e3) ->
    [%expr [%e Exp.ifthenelse
        [%expr [%e (tocaml_expression e1) ]]
        [%expr  [%e (tocaml_expression e2) ] ]
        (Some ( [%expr  [%e tocaml_expression e3 ] ] ))
    ]
    ]
  | IUnit -> [%expr ()]
  | IConstr _ -> [%expr ()]
  | ICall e ->
    [%expr [%e e ]]





let stringloc_of_ident ?(prefix="") ?(suffix="") i =
  {
    txt = prefix^i^suffix;
    loc = Location.none;
  }


let stringloc_of_pattern ?(prefix="") ?(suffix="") p =
  match p.p_desc with
  | Ident i ->
    {
      txt = prefix^i^suffix;
      loc = Location.none;
    }
  | _ -> failwith "no tuple !"


let tocaml_updates node outs =
  let rec aux { i_pattern = p; i_expression = e } acc =
    match p.p_desc with
    | Ident i ->
      [%expr [%e Exp.ident (lid_of_ident i) ] := ([%e tocaml_expression e]) ;
        [%e acc ]]
    | Tuple t -> assert false
    | PUnit -> assert false
    | Typed (p,s) -> aux {i_pattern=p; i_expression = e} acc



  in
  List.fold_left (fun acc u -> aux u acc) outs node.i_step_fun.i_updates

let rec lident_of_pattern ?(prefix="") ?(suffix="") p =
  match p.p_desc with
  | Ident i ->
    {
      txt = Lident (prefix^i^suffix);
      loc = Location.none
    }
  | Tuple t -> failwith "no tuple !"
  | PUnit -> failwith "no unit"
  | Typed (p,s) -> lident_of_pattern p

let rec lident_of_string s =
  {
    txt = Lident s;
    loc = Location.none
  }

let rec pexp_of_pat p =
  match p.p_desc with
  | Ident i ->
    { pexp_desc = Pexp_ident (lident_of_pattern p) ;
      pexp_loc = p.p_loc ;
      pexp_attributes = [] }
  | Tuple t ->
    let tl = List.map (fun p -> pexp_of_pat p) t in
    { pexp_desc = Pexp_tuple tl ;
      pexp_loc = p.p_loc ;
      pexp_attributes = [] }
  | PUnit ->   { pexp_desc = Pexp_construct (lident_of_string "()" ,None) ;
                 pexp_loc = p.p_loc ;
                 pexp_attributes = [] }
  | Typed (p,s) -> pexp_of_pat p

let tocaml_outputs_expr node =
  let aux ol =
    List.map pexp_of_pat ol
  in
  match node.i_outputs.p_desc with
  | PUnit -> [%expr () ]
  | Ident x  -> [%expr [%e pexp_of_pat node.i_outputs ]]
  | Tuple t -> [%expr [%e Exp.tuple (aux t) ] ]
  | Typed (p,s) ->  [%expr [%e pexp_of_pat node.i_outputs ]]


let tocaml_inputs_expr node =
  let aux ol =
    List.map pexp_of_pat ol
  in
  match node.i_inputs.p_desc with
  | PUnit -> [%expr () ]
  | Ident x  -> [%expr [%e pexp_of_pat node.i_inputs ]]
  | Tuple t -> [%expr [%e Exp.tuple (aux t) ] ]
  | Typed (p,s) ->  [%expr [%e pexp_of_pat node.i_inputs ]]


let rec pat_of_pattern p =
  match p.p_desc with
  | Ident i -> { ppat_desc = Ppat_var (stringloc_of_pattern p) ;
                 ppat_loc = p.p_loc ;
                 ppat_attributes = [] }
  | Tuple t ->
    let tl = List.map (fun p -> pat_of_pattern p) t in
    { ppat_desc = Ppat_tuple tl ;
      ppat_loc = p.p_loc ;
      ppat_attributes = [] }
  | PUnit -> { ppat_desc = Ppat_construct (lident_of_string "()" ,None);
               ppat_loc = p.p_loc ;
               ppat_attributes = [] }
  | Typed (p,s) ->
    let core_type = {
       ptyp_desc = Ptyp_constr(lident_of_string s,[]);
     ptyp_loc =  p.p_loc ;
     ptyp_attributes = [];
    }
    in
    {
      ppat_desc = Ppat_constraint (pat_of_pattern p, core_type) ;
      ppat_loc = p.p_loc;
      ppat_attributes = []}

let tocaml_eq_list el acc =
  let tocaml_eq e acc =
    let x = e.i_pattern in
    let ppat = pat_of_pattern x in
    let pexpr = tocaml_expression e.i_expression in
    [%expr let [%p ppat ] = ( [%e pexpr ] ) in  [%e acc ] ]
  in
  List.fold_left (fun l e -> tocaml_eq e l) acc el

let tocaml_app_inits inits acc =
  let aux {i_pattern = p ; i_expression = e} acc =
    match e with
    | IApplication (i,num,el) ->
      let listexp = [(Nolabel, [%expr [%e tocaml_expression el ]])] in
      let n = string_of_int num in
      [%expr let [%p Pat.var (stringloc_of_ident ~suffix:"_step" (i^n))] =
               [%e Exp.apply
                   (Exp.ident (lid_of_ident i)) listexp ] in
        (*
        let [%p Pat.var (stringloc_of_pattern p)] =
          [%e Exp.apply
                   (Exp.ident (lid_of_ident ~suffix:"_step" (i^n))) listexp ]
          in
*)
        [%e acc] ]

    | _ -> assert false
  in
  List.fold_left (fun acc i -> aux i acc) acc inits

let tocaml_fby_inits inits acc =
  let aux { i_pattern = p ;  i_expression = e} acc =
    match e with
    | IRefDef e ->  [%expr let [%p pat_of_pattern p ] =
                             ref [%e tocaml_expression e ] in [%e acc] ]
    | _ -> assert false
  in
  List.fold_left (fun acc i -> aux i acc) acc inits

let tocaml_inits inits acc =
  let aux {i_pattern = p ; i_expression = e}  acc =
    match e with
   (* | IApplication (i,num,el) ->
      let listexp = [(Nolabel, [%expr [%e tocaml_expression el ]])] in
      let n = string_of_int num in
      [%expr let [%p Pat.var (stringloc_of_ident ~suffix:"_step" (i^n))] =
               [%e Exp.apply
                   (Exp.ident (lid_of_ident i)) listexp ] in

(*
        let [%p (pat_of_pattern p)] =
          [%e Exp.apply
              (Exp.ident (lid_of_ident ~suffix:"_step" (i^n))) listexp ]
        in
*)
        [%e acc] ]
   *)

    | _ ->
      [%expr let [%p pat_of_pattern p] =
               [%e tocaml_expression e] in [%e acc] ]
  in
  List.fold_left (fun acc i -> aux i acc) acc inits


let tocaml_inputs node pname acc =
  let aux il =
    List.map (fun i -> pat_of_pattern i) il
  in
  let inputs = node.i_inputs in
  match inputs.p_desc with
  |  PUnit ->
    [%expr let [%p Pat.var pname ] =
             fun () -> [%e acc ]
      in
      [%e Exp.ident (lid_of_ident ~suffix:"_step" (str_of_pattern node.i_name)) ]]
  | Ident x  ->
    [%expr let [%p Pat.var pname] =
             fun [%p pat_of_pattern inputs] -> [%e acc ]
      in
      [%e Exp.ident (lid_of_ident ~suffix:"_step" (str_of_pattern node.i_name))]]
  | Tuple t ->
    [%expr let [%p Pat.var pname] =
             fun [%p Pat.tuple (aux t) ] -> [%e acc]
      in
      [%e Exp.ident (lid_of_ident ~suffix:"_step" (str_of_pattern node.i_name))]]
  | Typed(p,s) ->
    [%expr let [%p Pat.var pname] =
             fun [%p pat_of_pattern inputs] -> [%e acc ]
      in
      [%e Exp.ident (lid_of_ident ~suffix:"_step" (str_of_pattern node.i_name))]]

let tocaml_step node =
  let pname = stringloc_of_ident ~suffix:"_step" (str_of_pattern node.i_name) in
  let outs = tocaml_outputs_expr node in
  let ups = tocaml_updates node outs in
  let eqs = tocaml_eq_list (List.rev node.i_step_fun.i_equations) ups in
  tocaml_inputs node pname eqs

let expr_of_string s =
  [%expr [%e Exp.ident (lid_of_ident s)] ]

let tocaml_main inode =
  let name = str_of_pattern inode.i_name in
  let oc = open_out (name^"_io.ml") in
  let input = Printf.sprintf "let input_%s () = (* TODO *) \n" name in
  output_string oc input
  (* [%stri
   *   let () =
   *     let open IO in
   *          let main = [%e (Exp.ident (lid_of_ident name))] ()  in
   *          while true do
   *          let [%p pat_of_pattern inode.i_inputs ] = [%e Exp.ident (lid_of_ident ~suffix:"_inputs" name)] () in
   *          let [%p pat_of_pattern inode.i_outputs ] = main  [%e tocaml_inputs_expr inode ] in
   *          [%e Exp.ident (lid_of_ident ~suffix:"_outputs" name)] [%e tocaml_outputs_expr inode ]
   *        done ] *)

let tocaml_node inode =
  let name = stringloc_of_ident (str_of_pattern inode.i_name) in
  let inits = List.rev inode.i_inits in
  [%stri let [%p Pat.var name] =
           fun () ->
             [%e tocaml_inits inits (tocaml_step inode) ]
  ]
  (* match inode.i_inputs.p_desc with
   * | PUnit ->
   *   [%stri let [%p Pat.var name] =
   *            fun () ->
   *              [%e tocaml_inits inits (tocaml_step inode) ]
   *   ]
   * | Ident x  ->
   *   [%stri let [%p Pat.var name] =
   *            fun [%p pat_of_pattern inode.i_inputs] ->
   *              [%e tocaml_inits inits (tocaml_step inode) ]
   *   ]
   * | Tuple t  ->
   *   [%stri let [%p Pat.var name] =
   *            fun [%p Pat.tuple (List.map pat_of_pattern t) ] ->
   *              [%e tocaml_inits inits (tocaml_step inode) ]
   *   ]
   * | Typed (p,s) ->
   *
   *    [%stri let [%p Pat.var name] =
   *            fun [%p pat_of_pattern inode.i_inputs] ->
   *              [%e tocaml_inits inits (tocaml_step inode) ]
   *   ] *)
