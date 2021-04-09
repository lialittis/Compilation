(* Arbres de syntaxe abstraite typés *)

open Typed_ast
open Asttypes

type c_expr =
    { cexpr_desc: c_expr_desc;
      cexpr_clock: clock;
      cexpr_type:  ty;
      cexpr_loc: location; }

and c_expr_desc =
  | CE_const of const
  | CE_ident of string
  | CE_unop of unop * c_expr
  | CE_binop of binop * c_expr * c_expr
  | CE_app of string * c_expr list
  | CE_prim of string * c_expr list
  | CE_if of c_expr * c_expr * c_expr
  | CE_fby of const list * c_expr
  | CE_tuple of c_expr list
  | CE_print of c_expr list
  | CE_when of c_expr * const * c_expr
  | CE_merge of c_expr * ((const * c_expr) list)

and base_clock = 
  | Base
  | Sampled of base_clock * const * c_expr
  | NotClockedBase
  | ClockVar of (clock list -> base_clock) * int (* dans les noeuds, les clocks des variables sont 
                                                  des fonctions des clocks des inputs du noeuds *)

and clock =
  | ClockApp of ((clock list) -> clock) * int   (* pour les applications *)
  | ClockTuple of clock list                    (* pour les équations : cexpr_clock et cpatt_clock *)
  | NotClocked                                  (* pour les variables non initialisées *)
  | ClockBase of base_clock                     

type typed_clocked_var = string * base_ty * clock

type c_patt =
    { cpatt_desc: string list;
      cpatt_clock: clock;
      cpatt_type: ty;
      cpatt_loc: location; }

type c_equation =
    { ceq_patt: c_patt; 
      ceq_expr: c_expr; }

type c_node =
    { cn_name: string;
      cn_input: typed_clocked_var list;
      cn_output: typed_clocked_var list;
      cn_local: typed_clocked_var list;
      cn_equs: c_equation list;
      cn_loc: location; }

type c_file = c_node list
