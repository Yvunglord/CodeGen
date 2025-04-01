open Ocaml_parser.Parser

let lw = Printf.sprintf "lw %s, %s\n"
let li = Printf.sprintf "li %s, %d\n"
let push = Printf.sprintf "addi sp, sp, -4\nsw %s, 0(sp)\n"

let binop op =
  Printf.sprintf
    "lw t0, 0(sp)\nlw t1, 4(sp)\n%s t2, t1, t0\naddi sp, sp, 4\nsw t2, 0(sp)\n"
    op

let generate_expression_asm (expression : expr) : string =
  let rec helper = function
    | Add (e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (helper e2) (binop "add")
    | Sub (e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (helper e2) (binop "sub")
    | Mul (e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (helper e2) (binop "mul")
    | Div (e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (helper e2) (binop "div")
    | Int n -> li "t0" n ^ push "t0"
    | Var v -> lw "t0" v ^ push "t0"
    | Let (var, e1, e2) ->
        Printf.sprintf "%s%s%s" (helper e1) (lw "t0" var ^ push "t0") (helper e2)
  in
  helper expression

let generate_asm data =
  if List.length data.args_names <> List.length data.args then
    failwith "Number of arguments does not match";
  
  let expression_asm = generate_expression_asm data.function_body in
  let args_asm =
    List.map2
      (fun name value -> Printf.sprintf "%s: .word %d" name value)
      data.args_names data.args
  in
  Printf.sprintf
    ".section .data\n\
    \ .align 4\n\
     %s\n\
     .section .text\n\
     .globl _start\n\
     _start:\n\
     %s\n\
     addi sp, sp, 4\n\
     mv a0, t2\n\
     call print_int\n\
     li a0, 0\n\
     li a7, 93\n\
     ecall\n"
    (String.concat "\n" args_asm) expression_asm

let write_asm_to_file filename asm_code =
  let oc = open_out filename in
  Printf.fprintf oc "%s" asm_code;
  close_out oc

let () =
  let input =
    if Array.length Sys.argv < 2 then exit 1
    else
      let filename = Sys.argv.(1) in
      let ic = open_in filename in
      let input = really_input_string ic (in_channel_length ic) in
      close_in ic;
      input
  in
  match parse (List.of_seq (String.to_seq input)) with
  | Parsed (data, _) ->
      let asm_code = generate_asm data in
      write_asm_to_file "output.s" asm_code
  | Failed -> exit 1