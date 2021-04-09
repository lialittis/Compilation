(* code d'Adrien Guatto *)

open Format
open Asttypes
open Clocked_ast

let rec print_list f sep fmt l = match l with
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s@ %a" f h sep (print_list f sep) t

let rec print_list_eol f sep fmt l = match l with
  | [] -> ()
  | [x] -> fprintf fmt "%a%s" f x sep
  | h :: t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol f sep) t

let print_const fmt c = match c with
  | Cunit -> fprintf fmt "()"
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Cfloat f -> fprintf fmt "%f" f
  | Cstring s -> fprintf fmt "\"%s\"" s

let print_unop fmt op = match op with
  | Unot -> fprintf fmt "~"
  | Uminus -> fprintf fmt "-"
  | Uminus_f -> fprintf fmt "-."

let print_binop fmt op = match op with
  | Beq -> fprintf fmt "eq"
  | Bneq -> fprintf fmt "neq"
  | Blt -> fprintf fmt "lt"
  | Ble -> fprintf fmt "le"
  | Bgt -> fprintf fmt "gt"
  | Bge -> fprintf fmt "ge"
  | Badd -> fprintf fmt "add"
  | Bsub -> fprintf fmt "sub"
  | Bmul -> fprintf fmt "mul"
  | Bdiv -> fprintf fmt "div"
  | Bmod -> fprintf fmt "mod"
  | Badd_f -> fprintf fmt "add_f"
  | Bsub_f -> fprintf fmt "sub_f"
  | Bmul_f -> fprintf fmt "mul_f"
  | Bdiv_f -> fprintf fmt "div_f"
  | Band -> fprintf fmt "and"
  | Bor -> fprintf fmt "or"

let print_name fmt name = fprintf fmt "%s" name

let rec print_exp fmt e = match e.cexpr_desc with
  | CE_const c -> print_const fmt c
  | CE_ident s -> fprintf fmt "%s" s
  | CE_unop (op, e) -> fprintf fmt "%a(%a)" print_unop op print_exp e
  | CE_binop (op, l, r) ->
      fprintf fmt "(@[%a %a@ %a@])" print_binop op print_exp l print_exp r
  | CE_app (name, e_list) | CE_prim (name, e_list) ->
      fprintf fmt "%a(@[%a@])" print_name name print_arg_list e_list
  | CE_if (cond, cons, altr) ->
      fprintf fmt "@[if %a@ then %a@ else %a@]"
        print_exp cond
        print_exp cons
        print_exp altr
  | CE_when (e1, cons, e2) ->
      fprintf fmt "@[%a@ when %a@(%a@)]"
        print_exp e1
        print_const_exp [cons]
        print_exp e2
  | CE_merge (e, c_e_list) ->
      fprintf fmt "@[merge %a %a@]" 
      print_exp e
      print_const_expr_list c_e_list
  | CE_fby (l, r) ->
      fprintf fmt "@[(@[%a@]) fby %a@]" print_const_exp l print_exp r
  | CE_tuple e_list ->
      fprintf fmt "(@[%a@])" print_tuple_arg_list e_list
  | CE_print e_list ->
      fprintf fmt "print (@[%a@])" print_arg_list e_list

and print_arg_list fmt e_list = match e_list with
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_tuple_arg_list fmt e_list = match e_list with
  | [] -> assert false
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_const_exp fmt ce_list = match ce_list with
  | [] -> assert false
  | [c] -> fprintf fmt "%a" print_const c
  | h :: t -> fprintf fmt "%a,@ %a" print_const h print_const_exp t

and print_const_expr_list fmt ce_list = match ce_list with
  | [] -> assert false
  | [c, e] -> fprintf fmt "(%a ->@ %a)" print_const c print_exp e
  | (c_h, e_h) :: t -> fprintf fmt "(%a ->@ %a)@ %a" print_const c_h print_exp e_h print_const_expr_list t

let print_eq fmt eq =
  fprintf fmt "@[(%a) = @[%a@]@]"
    (print_list print_name ",") eq.ceq_patt.cpatt_desc
    print_exp eq.ceq_expr

let print_base_type fmt bty = match bty with
  | Tunit -> fprintf fmt "unit"
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Tfloat -> fprintf fmt "float"
  | Tstring -> fprintf fmt "string"

(* let print_type = print_list print_cbase_type "*" *)

let rec print_base_clock fmt = function
  | Base -> fprintf fmt "base"
  | Sampled (ck, c, ce) -> fprintf fmt "%a on %a(%a)" print_base_clock ck print_const_exp [c] print_exp ce
  | ClockVar (f, i) -> 
      let inputs = List.init i (fun _ -> NotClocked) in
      let bck = f inputs in
      print_clock fmt (ClockTuple (List.init i (fun _ -> NotClocked)));
      fprintf fmt " -> ";
      print_base_clock fmt bck;
  | NotClockedBase -> fprintf fmt "CK"

and print_clock fmt = function
  | ClockApp (f, i) -> 
      let ck_in = List.init i (fun _ -> NotClocked) in
      let ck_out = f ck_in in
      print_clock fmt (ClockTuple (List.init i (fun _ -> NotClocked)));
      fprintf fmt " -> ";
      print_clock fmt ck_out
  | ClockTuple [] -> fprintf fmt "empty tuple"
  | ClockTuple (ck::ckl) -> 
      fprintf fmt "(";
      print_clock fmt ck;
      List.iter (fun ck -> fprintf fmt " * %a" print_clock ck) ckl;
      fprintf fmt ")" 
  | ClockBase ck -> print_base_clock fmt ck
  | NotClocked -> fprintf fmt "CK"

let print_var_dec fmt (name, ty, ck) =
  fprintf fmt "%s : %a (%a)" name print_base_type ty print_clock ck

let rec print_var_dec_list = print_list print_var_dec ";"

let print_node fmt nd =
  fprintf fmt
    "@[node %s(@[%a@]) returns (@[%a@])@\nvar @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    nd.cn_name
    print_var_dec_list nd.cn_input
    print_var_dec_list nd.cn_output
    print_var_dec_list nd.cn_local
    (print_list_eol print_eq ";") nd.cn_equs

let print_node_list_std ndl =
  List.iter (fun nd -> Format.printf "%a@\n@." print_node nd) ndl
