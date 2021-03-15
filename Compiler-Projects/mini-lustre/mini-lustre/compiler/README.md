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

### imp.ml

Pour génrérer le "target code" on doit créer 3 types qui sont : une mémoire "mem", une initialisation "init" et des equations. Dans le type mem, il ya deux types de sous-mémoires: une mémoire pour les variables issues de fby : fby_mem et une mémoire pour les variables issues d'un appel de noeud : node_mem et dans imp.ml il faut que tu complètes la fonction compile equation pour le cas où l'équation est de type "fby" et pour le cas où l'équation est un appel de noeud donc de type "app".

Dans le cas où ton équation c'est "e =TE_fby(e1,e2)", il faut que tu ajoutes à mem.fby_mem la variable ("e_next"; type(e)) où type(e) correspond au type de e pour créer e_next il suffit d'appliquer la fonction gen_next_id à e et pour node_mem c'est légerement différent car node_mem est une liste de couple de string. Et il faut appliquer gen_mem_id Pour te donner une idée voici deux lignes dans le cas TE_app de la fonction compile equation.


The output of the compile_base_expr e is m_expr = { mexpr_desc: m_expr_desc; mexpr_type: base_ty list; } (but here it may be not a list, it denpends)

The output of compile_equation is ((mem_acc: Imp_ast.mem),(init_acc: Imp_ast.init),(compute_acc: Imp_ast.m_equation list), (update_acc: (string * Imp_ast.atom) list)), where Imp_ast.mem contains two parts fby_mem and node_mem and Imp_ast.init contains two parts as well fby_init and node_init. 


TE_app is string * t_expr list, so n is the node and el is a list of t_expr, then we know that we need to update the node_mem by gen_mem_id n. We let new_el = List.map (fun e1 -> compile_base_expr e1) el. Note that List.map f [a1; ...; an] applies function f to a1, ..., an, and builds the list [f a1; ...; f an] with the results returned by f. So new_el is the type of m_expr list.

init_acc.node_init is (string * string) list, so to concatenate two list, we need to use List.append. new_compute_acc = List.append new_el compute_acc.

Question: why the node_init is (string * string) list but not only (string * string) ?






## Compilation


## Cleaning Directory



## Execution


##


