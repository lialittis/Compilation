# Mini-lustre compiler

## Requirements

- OCaml (>=4.05);
- mini lustre;

## Description

Target language : OCaml

The Lustre to OCaml language compiler add parallel execution of instruction.

********ref*********

Synchronous extension of OCaml in the style of the Lustre synchronous programming language.

The OCaml language is extended with Lustre "nodes". These nodes can be viewed as synchronous functions,
which run at every instant. An instant is the atomic unit of time at which the main node of the program computes outputs from inputs.

Inputs and outputs are considered as data flows, i.e. streams of values that can change through time. For example, the constant 2 is considered as the flow 2,2,2,2,...

Here is an OcaLustre node that takes two streams a and b, and produces a stream c that is the sum of a and b :

```ocaml
let%node example (a,b) ~return:(c) =
  c = a + b
```

********ref*******


### Syntax abstract types

There some important types, which are needed to be explained here.

TE_fby : const list * t_expr

So we know that, the expression for TE_fby is a list of const and a t_expr type.


#### imp_ast

### normalization.ml

The types for now used in this file are defined by *Typed_ast* module.

**TODO**

The function we need to complete is *normalize ctx e*, e is the expression in the normal
form and the ctx contains the equations introduced by the normalisation.
Note that, the return of normalize is a tuple of vars * eqs and one expression.

The patches are seperated by the type of *e.texpr_desc*

- [x] *TE_binop(op,e1,e2)*

For this we only need to normalize the two expressions orderly and return *ctx* and *e*.

- [x] *TE_fby(c,e1)*

The first element is *const list* and the second element is *t_expr*.
It is more likely with *TE_app(n,le)*.

Introduce *new_pat* firstly. It takes one expression e and return a triple with decl, 
patt and expression. **Why this ?** The point is that it will update the decl and loc
of expression to small pieces.

So the first element of returns of normalization could be updated, like this *(x_decl@new_vars,
x_eq::new_eqs)*, where *x_decl* is obtained by *new_pat* and *x_eq* is obtained by the
combination of *x_patt* and updated expression by *x_expr* after normalization.

**still have questions**

### Schedule.ml

Using module Set

After the normalization, equations are scheduled according to data-dependences.

A (non trusted) scheduling function:
schedule: eq list →  eq list
• Define what is a well scheduled sequence of equations:
• An equation x = ca must appear before any read of x (data-dependence)
• An equation x = v fby a must appear after any read of x (anti-dependence)
• Prove that the semantics of a set of equations does not depend on the relative order between them.
• Check that the set of equations is well scheduled.
By separating the scheduling function from the verification that the result is well scheduled, it is easier to implement a clever scheduling function.

**TODO**

We need to complete the *add_vars_of_exp s e* function :

When the type of e is :
- [x] *TE_ident* x:
if S.mem x s then s else S.add x s
- [x] *TE_fby (c, e1)* :
s
- [x] *TE_if (e1,e2,e3)*:
add expression to s in order.

### imp.ml

Pour génrérer le "target code" on doit créer 3 types qui sont : une mémoire "mem", une initialisation "init" et des equations. Dans le type mem, il ya deux types de sous-mémoires: une mémoire pour les variables issues de fby : fby_mem et une mémoire pour les variables issues d'un appel de noeud : node_mem et dans imp.ml il faut que tu complètes la fonction compile equation pour le cas où l'équation est de type "fby" et pour le cas où l'équation est un appel de noeud donc de type "app".

Dans le cas où ton équation c'est "e =TE_fby(e1,e2)", il faut que tu ajoutes à mem.fby_mem la variable ("e_next"; type(e)) où type(e) correspond au type de e pour créer e_next il suffit d'appliquer la fonction gen_next_id à e et pour node_mem c'est légerement différent car node_mem est une liste de couple de string. Et il faut appliquer gen_mem_id Pour te donner une idée voici deux lignes dans le cas TE_app de la fonction compile equation.

The output of the compile_base_expr e is m_expr = { mexpr_desc: m_expr_desc; mexpr_type: base_ty list; } (but here it may be not a list, it denpends)

The output of compile_equation is ((mem_acc: Imp_ast.mem),(init_acc: Imp_ast.init),(compute_acc: Imp_ast.m_equation list), (update_acc: (string * Imp_ast.atom) list)), where Imp_ast.mem contains two parts fby_mem and node_mem and Imp_ast.init contains two parts as well fby_init and node_init. 

TE_app is string * t_expr list, so n is the node and el is a list of t_expr, then we know that we need to update the node_mem by gen_mem_id n. We let new_el = List.map (fun e1 -> compile_base_expr e1) el. Note that List.map f [a1; ...; an] applies function f to a1, ..., an, and builds the list [f a1; ...; f an] with the results returned by f. So new_el is the type of m_expr list.

init_acc.node_init is (string * string) list, so to concatenate two list, we need to use List.append. new_compute_acc = List.append new_el compute_acc.

Question: why the node_init is (string * string) list but not only (string * string) ?

**TODO**

1. Complete *compile_base_expr e* function, return *desc*

Match *e.texpr_desc* with

- [x] *TE_if(e1,e2,e3)*:
compile the base expr in order and return to *ME_if*


2. Complete *compile_equation*

We need to understand tvars firstly, it is obtained by compile_patt eq.teq_patt 
and it contains tpatt_desc and tpatt_type;




Match *e.texpr_desc* with

- [x] *TE_fby (e1,e2)*:
i
Firstly, change (e1,e2) to (c,e1);
e1.texpr_desc is TE_tuple after normalization;

```
mem_acc: node_mem, fby_name <- (update the next_name
								<- (gen_next_id by tvars.tpatt_desc, type of tvars.tpatt_type ))
By this, the name of variables in the path after fby could be add onto the name of fby.

init_acc: node_mem, fby_name <- (update the init_name by next_name and const element)

update_acc: add atom <- (compile_atom for each expr element)

compute_acc: meq_patt <- tvars,
			 meq_expr <- (mexpr_desc <- ME_tuple a list of m_expr similar with fby_name,
			 			  mexpr_type <- the texpr_type of expr element)
```


Question: 

If the length of c is the same with e1 ?

YES

If the lenth of tvars is the same with c or e1 ?

YES



- [x] *TE_app(n,el)*

```
mem_acc: fby_mem, node_mem <- (update the mem_id)

init_acc: fby_init, node_init <- (add mem_id)

compute_acc: eq list <- (add eq with 
						{meq_patt <- (tvars); 
						 meq_expr <- ( 
						 			   mexpr_desc <- (ME_app(n,mem_name,mexpr list))
									   mexpr_type <- (e.texpr_type})
									 )
						)
: This variable is used to compute the return value of the called node;

update_acc: update_acc
```


## Compilation



## Cleaning Directory



## Execution


## TEST

test.mls -> test.ml

```
  1 node integr (t: float) returns (y: float);
  2 let
  3   y =  0.0 fby t;
  4 tel
  5
  6
  7 node test (i: unit) returns (o: unit);
  8 var x0, x1, x2: float;
  9 let
 10   x0 = 1.0;
 11   x1 = 0.0 fby x0;
 12   x2 = integr(x1);
 13   o = print("coucou\n");
 14 tel

```

OUTPUT

```ocaml
 25 type integr'_1_mem = {
 26     mutable aux'2_next1: float;
 27   }
 28
 29 let integr'_1_init () = {
 30     aux'2_next1 = 0.;
 31   }
 32
 33 let integr'_1_step mem' (t) =
 34   let (aux'1) = t in
 35   let (aux'2) = (mem'.aux'2_next1) in
 36   let (y) = aux'2 in
 37   mem'.aux'2_next1 <- aux'1;
 38   (y)
 39
 40
 41 type test'_2_mem = {
 42     mutable aux'4_next2: float;
 43     integr_mem1: integr'_1_mem;
 44   }
 45
 46 let test'_2_init () = {
 47     aux'4_next2 = 0.;
 48     integr_mem1 = integr'_1_init ();
 49   }
 50
 51 let test'_2_step mem' (i) =
 52   let (aux'4) = (mem'.aux'4_next2) in
 53   let (aux'6) = (
 54     (print_string "coucou\n");
 55     flush_all()) in
 56   let (x0) = 1. in
 57   let (aux'3) = x0 in
 58   let (o) = aux'6 in
 59   let (x1) = aux'4 in
 60   let (aux'5) = x1 in
 61   let (x2) = aux'5 in
 62   mem'.aux'4_next2 <- aux'3;
 63   (o)
 64
```









##


