open Syntax

let rec read_eval_print () =
    print_string "\n# ";
    flush stdout;
    try begin
        match Parser.top Lexer.main (Lexing.from_channel stdin) with
        | Def _ as e ->
                print_endline "parsed a function definition";
                dump_value (Codegen.codegen_func e);
        | Ext _ as e ->
                print_endline "parsed an extern";
                dump_value (Codegen.codegen_proto e);
        | Exp _ as e ->
                print_endline "parsed a toplevel expr";
                dump_value (Codegen.codegen_func e);
        end
    with Parser.Error -> print_endline "parser error";
        read_eval_print ()




let _ = read_eval_print ()

