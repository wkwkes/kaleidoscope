
open Llvm
open Ast

exception Error of string

let context = global_context ()
let the_module = create_module context "myjit"
let builder = builder context
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10
let float_type = float_type context

let rec codegen_expr = function
  | Number  n -> const_float float_type n
  | Variable name ->
      (try (Hashtbl.find named_values name) with
       | Not_found -> raise (Error "unknown variable name"))
  | Binary_c (op, lhs, rhs) | Binary_l (op, lhs, rhs) ->
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      begin
        match op with
        | "+" -> build_fadd lhs_val rhs_val "faddtmp" builder
        | "-" -> build_fsub lhs_val rhs_val "fsubtmp" builder
        | "*" -> build_fmul lhs_val rhs_val "fmultmp" builder
        | "<" -> let i = build_fcmp Fcmp.Ult lhs_val rhs_val "lttmp"
                     builder in build_uitofp i float_type "boollttmp"
              builder
        | ">" -> let i = build_fcmp Fcmp.Ugt lhs_val rhs_val "gttmp"
                     builder in build_uitofp i float_type "boolgttmp"
              builder
        | _ -> raise (Error "error binary")
      end
  | Call (callee, args) -> 
      let callee =
        match lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise (Error "unknown function referenced")
      in let params = params callee in
      if Array.length params == Array.length args then () else
        raise (Error "incorrect # argumentes passed");
      let args = Array.map codegen_expr args in
      build_call callee args "calltmp" builder

let codegen_proto = function
  | Prototype (name, args) -> 
      let floats = Array.make (Array.length args) float_type in
      let ft = function_type float_type floats in
      let f = 
        match lookup_function name the_module with
        | None -> print_string (name^" is being checked\n"); 
          define_function name ft the_module;
        | Some f -> 
            if block_begin f <> At_end f then
              raise (Error "redefinition of function");
            if element_type (type_of f) <> ft then
              raise (Error "redefinition of funciton with
                            different # args");
            f
      in Array.iteri (fun i a -> 
          let n = args.(i) in
          set_value_name n a;
          Hashtbl.add named_values n a;
        ) (params f);
      f

let codegen_func the_fpm = function
  | Function (proto, body) ->
      Hashtbl.clear named_values;
      let the_function = codegen_proto proto in
      let bb = append_block context "entry" the_function in
      position_at_end bb builder;
      try
        let ret_val = codegen_expr body in
        let _ = build_ret ret_val builder in
        Llvm_analysis.assert_valid_function the_function;
        let _ = PassManager.run_function the_function the_fpm in
        the_function
      with e ->
        delete_function the_function;
        raise e
