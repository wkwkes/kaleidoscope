
open Llvm
open Syntax

exception Error of string

let context = global_context ()
let the_module = create_module context "myjit"
let builder = builder context
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let rec codegen_expr = function
    | Number  n -> const_float double_type n
    | Variable name ->
            (try (Hashtbl.find named_values name) with
              | Not_found -> raise (Error "unknown variable name"))
    | Binary (op, lhs, rhs) ->
            let lhs_val = codegen_expr lhs in
            let rhs_val = codegen_expr rhs in
            begin
                match op with
                | "+" -> build_add lhs_val rhs_val "addtmp" builder
                | "-" -> build_sub lhs_val rhs_val "subtmp" builder
                | "*" -> build_mul lhs_val rhs_val "multmp" builder
                | "<" -> let i = build_fcmp Fcmp.Ult lhs_val rhs_val "lttmp"
                            builder in build_uitofp i double_type "boollttmp"
                            builder
                | ">" -> let i = build_fcmp Fcmp.Ugt lhs_val rhs_val "gttmp"
                            builder in build_uitofp i double_type "boolgttmp"
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
            let doubles = Array.make (Array.length args) double_type in
            let ft = function_type double_type doubles in
            let f = 
                match lookup_function name the_module with
                | None -> declare_function name ft the_module
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

let codegen_func = function
    | Function (proto, body) ->
            Hashtbl.clear named_values;
            let the_function = codegen_proto proto in
            let bb = append_block context "entry" the_function in
            position_at_end bb builder;

            try
                let ret_val = codegen_expr body in
                let _ = build_ret ret_val builder in
                Llvm_analysis.assert_valid_function the_function;
            with e ->
                delete_function the_function;
                raise e
