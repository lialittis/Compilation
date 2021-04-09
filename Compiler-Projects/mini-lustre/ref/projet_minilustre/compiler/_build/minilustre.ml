
(* Programme principal *)

open Format
open Lexing
open Lexer
open Parser
open Ast

let usage = "usage: "^Sys.argv.(0)^" [options] file.mls"

let parse_only = ref false
let type_only = ref false
let clock_only = ref false
let norm_only = ref false
let sched_only = ref false
let lucy_printer = ref false
let ocaml_printer = ref true
let main_node = ref ""
let verbose = ref false

let spec =
  ["-parse-only", Arg.Set parse_only, "  stops after parsing";
   "-type-only", Arg.Set type_only, "  stops after typing";
   "-clock-only", Arg.Set clock_only, "  stops after clocking";
   "-norm-only", Arg.Set norm_only, "  stops after normalization";
   "-sched-only", Arg.Set sched_only, "  stops after scheduling";
   "-main", Arg.Set_string main_node, "<name>  main node";
   "-verbose", Arg.Set verbose, "print intermediate transformations";
   "-v", Arg.Set verbose, "print intermediate transformations";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".mls") then
      raise (Arg.Bad "no .mls extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    let ft = Typing.type_file f !main_node in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Typed ast                          */@.";
      Format.printf "/**************************************/@.";
      Typed_ast_printer.print_node_list_std ft
    end;
    if !type_only then exit 0;
    let fc = Clocking.clock_file ft !main_node in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Clocked ast                          */@.";
      Format.printf "/**************************************/@.";
      Clocked_ast_printer.print_node_list_std fc
    end;
    if !clock_only then exit 0;
    let fc = Normalization.file fc in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Normalized ast                     */@.";
      Format.printf "/**************************************/@.";
      Clocked_ast_printer.print_node_list_std fc
    end;
    Checks.normalization fc;
    if !norm_only then exit 0;
    let fc = Scheduling.schedule fc in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Scheduled ast                      */@.";
      Format.printf "/**************************************/@.";
      Clocked_ast_printer.print_node_list_std fc;
    end;
    Checks.scheduling fc;
    if !sched_only then exit 0;
    let imp_prg = Imp.compile fc in
    let imp_prg = Imp.rename_nodes imp_prg main_node in
    let ml = (Filename.chop_suffix file ".mls") ^ ".ml" in
    let c = open_out ml in
    Ocaml_printer.output_ocaml c imp_prg !main_node;
    close_out c;
    exit 0
  with
    | Lexical_error s ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s\n@." s;
	exit 1
    | Parsing.Parse_error ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error\n@.";
	exit 1
    | Typing.Error(l,e) ->
  report_loc l;
  eprintf "%a\n@." Typing.report e;
  exit 1
    | Clocking.Error(l,e) ->
  report_loc l;
  eprintf "%a\n@." Clocking.report e;
  exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2
