open Ocaml_parser.Parser
open Alcotest

let pp_program_data fmt data =
  let args_names = String.concat ", " data.args_names in
  let args = String.concat ", " (List.map string_of_int data.args) in
  Fmt.pf fmt "{ args_names = [%s]; args = [%s] }" args_names args

let program_data = testable pp_program_data ( = )

let test_parser name input expected =
  match parse (List.of_seq (String.to_seq input)) with
  | Failed -> Alcotest.fail "Parsing failed"
  | Parsed (data, _) -> Alcotest.check program_data name expected data

  let test_cases =
    [
      ( "test_case_1",
        "let f x y z = 0 \nlet main = print_int (f 82 13 4)",
        {
          args_names = [ "x"; "y"; "z" ];
          function_body = Int 0;
          args = [ 82; 13; 4 ];
        } );
      ( "test_case_2",
        "let f a b = a + b \nlet main = print_int (f 5 10)",
        {
          args_names = [ "a"; "b" ];
          function_body = Add (Var "a", Var "b");
          args = [ 5; 10 ];
        } );
      ( "test_case_3",
        "let f x y z = x * y - z \nlet main = print_int (f 7 8 9)",
        {
          args_names = [ "x"; "y"; "z" ];
          function_body = Sub (Mul (Var "x", Var "y"), Var "z");
          args = [ 7; 8; 9 ];
        } );
      ( "test_case_4",
        "let f a b c = a * (b + c) \nlet main = print_int (f 1 2 3)",
        {
          args_names = [ "a"; "b"; "c" ];
          function_body = Mul (Var "a", Add (Var "b", Var "c"));
          args = [ 1; 2; 3 ];
        } );
      ( "test_case_5",
        "let f n = n / 2 \nlet main = print_int (f 10)",
        {
          args_names = [ "n" ];
          function_body = Div (Var "n", Int 2);
          args = [ 10 ];
        } );
      ( "test_case_6",
        "let f a b = (a + b) * (a - b) \nlet main = print_int (f 6 4)",
        {
          args_names = [ "a"; "b" ];
          function_body = Mul (Add (Var "a", Var "b"), Sub (Var "a", Var "b"));
          args = [ 6; 4 ];
        } );
        ( "test_case_7",
        "let f x = x * x * x \nlet main = print_int (f 3)",
        {
          args_names = [ "x" ];
          function_body = Mul (Var "x", Mul (Var "x", Var "x"));  
          args = [ 3 ];
        } );
      ( "test_case_8",
        "let f = 42 \nlet main = print_int (f)",
        {
          args_names = [];
          function_body = Int 42;
          args = [];
        } );
      ( "test_case_9",
        "let f a b = (a * 2) + (b * 3) \nlet main = print_int (f 1 2)",
        {
          args_names = [ "a"; "b" ];
          function_body = Add (Mul (Var "a", Int 2), Mul (Var "b", Int 3));
          args = [ 1; 2 ];
        } );
      ( "test_case_10",
        "let f x y = (x + y) / 2 \nlet main = print_int (f 10 20)",
        {
          args_names = [ "x"; "y" ];
          function_body = Div (Add (Var "x", Var "y"), Int 2);
          args = [ 10; 20 ];
        } );
    ]

let () = 
  let tests = List.map (fun (name, input, expected) ->
    (name, `Quick, fun () -> test_parser name input expected)
  ) test_cases in
  Alcotest.run "Parser Tests" [ ("parser", tests) ]
