
open Ast
open Llvm
(* open Llvm_executionengine *)
open Llvm_target
open Llvm_scalar_opts
open Ctypes
(* open PosixTypes *)
open Foreign
        
let function_call the_function ee =
  let exec = Llvm_executionengine.get_function_address
      (Llvm.value_name the_function) (funptr (void @-> returning float)) ee
  in
  exec ()
    

let rec main_loop the_fpm the_execution_engine =
  print_string "\n# ";
  flush stdout;
  begin
    try begin
      let e = Parser.top Lexer.main (Lexing.from_channel stdin) in
      if (Array.length (Sys.argv) > 1) then
        if (Sys.argv.(1) = "-ast") then
          (print_string (show_toplevel e); flush stdout);
      match e with
      | Def e ->
          dump_value (Codegen.codegen_func the_fpm e)
      | Ext e ->
          dump_value (Codegen.codegen_proto e)
      | Exp e ->
           let the_function = Codegen.codegen_func the_fpm e in
          dump_value the_function;
          (*
          let function_name = Llvm.value_name the_function in
          let result = Llvm_executionengine.get_function_address
          function_name
          (funptr (void @-> returning float))
          the_execution_engine in
             *)
          print_string "Evaluated to ";
          print_float (function_call the_function the_execution_engine);
          print_newline ()
      end
    with Parser.Error -> print_endline "parser error"
  end;
  main_loop the_fpm the_execution_engine

let _ =
  ignore (Llvm_executionengine.initialize ());
  let the_execution_engine = Llvm_executionengine.create Codegen.the_module in
  let the_fpm = PassManager.create_function Codegen.the_module in

  (*
  Llvm_target.DataLayout.add_to_pass_manager the_fpm ;
  add_instruction_combination the_fpm;
  add_reassociation the_fpm;
  add_gvn the_fpm;
  add_cfg_simplification the_fpm;*)
  try 
    main_loop the_fpm the_execution_engine
  with e ->
    dump_module Codegen.the_module
    