open Ocaml_parser.Parser
open Alcotest

let string_to_char_list s =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (s.[i] :: acc)
  in
  aux (String.length s - 1) []

let program_data_eq a b =
  a.args_names = b.args_names &&
  a.args = b.args &&
  fold_constants a.function_body = fold_constants b.function_body

let pp_program_data fmt data =
  let args_names = String.concat ", " data.args_names in
  let args = String.concat ", " (List.map string_of_int data.args) in
  Format.fprintf fmt "{ args_names = [%s]; args = [%s]; body = %s }"
    args_names args
    (match fold_constants data.function_body with
     | Int n -> string_of_int n
     | _ -> "complex expression") 

let program_data_testable = testable pp_program_data program_data_eq

let test_parser name input expected =
  match parse (string_to_char_list input) with
  | Failed -> Alcotest.fail "Parsing failed"
  | Parsed (data, _) ->
      Alcotest.check program_data_testable name expected data

let test_cases = [
  ("test_add_fold",
   "let f = 2 + 3 \nlet main = print_int (f)",
   { args_names = []; function_body = Int 5; args = [] });
  ("test_sub_fold",
   "let f = 7 - 3 \nlet main = print_int (f)",
   { args_names = []; function_body = Int 4; args = [] });
  ("test_mul_fold",
   "let f = 2 * 3 \nlet main = print_int (f)",
   { args_names = []; function_body = Int 6; args = [] });
  ("test_div_fold",
   "let f = 6 / 3 \nlet main = print_int (f)",
   { args_names = []; function_body = Int 2; args = [] });
  ("test_nested_fold",
   "let f = (2 + 3) * (4 - 1) \nlet main = print_int (f)",
   { args_names = []; function_body = Int 15; args = [] });
]

let rec count_nodes = function
  | Add(a,b) | Sub(a,b) | Mul(a,b) | Div(a,b) -> 1 + count_nodes a + count_nodes b
  | Let(_,a,b) -> 1 + count_nodes a + count_nodes b
  | Int _ | Var _ -> 1

let performance_test () =
  let test_expr = 
    Mul(
      Add(Int 5, Sub(Int 10, Int 3)),
      Div(
        Mul(Int 2, Int 7),
        Add(Int 1, Int 1)
      )
    )
  in
  
  let time f =
    let start = Unix.gettimeofday () in
    let res = f () in
    let stop = Unix.gettimeofday () in
    (res, stop -. start)
  in
  
  let orig_nodes = count_nodes test_expr in
  let opt_nodes = count_nodes (fold_constants test_expr) in
  let _, opt_time = time (fun () -> ignore(fold_constants test_expr)) in
  
  Printf.printf "\nPerformance results:\n";
  Printf.printf "Original expr size: %d nodes\n" orig_nodes;
  Printf.printf "Optimized expr size: %d nodes\n" opt_nodes;
  Printf.printf "Optimization time: %.6f seconds\n" opt_time;
  Printf.printf "Reduction: %.1f%%\n" 
    (100. *. float_of_int (orig_nodes - opt_nodes) /. float_of_int orig_nodes)

(* Запуск тестов *)
let () =
  let alcotest_suite = List.map (fun (name, input, expected) ->
    (name, `Quick, (fun () -> test_parser name input expected))
  ) test_cases in
  
  let bench_test = ("Performance test", `Slow, performance_test) in
  
  Alcotest.run "Compiler Tests" [
    ("Parser tests", alcotest_suite);
    ("Optimization tests", [bench_test]);
  ]