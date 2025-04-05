type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Int of int
  | Var of string
  | Let of string * expr * expr
  | Shl of expr * int

type program_data = {
  args_names : string list;
  function_body : expr;
  args : int list;
}

type 'a parser_result = Failed | Parsed of 'a * char list
type 'a parser = char list -> 'a parser_result

let ( >>= ) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
 fun input ->
  match p input with Failed -> Failed | Parsed (v, rest) -> (f v) rest

let ( >>| ) (p : 'a parser) (f : 'a -> 'b) : 'b parser =
 fun input ->
  match p input with Parsed (v, rest) -> Parsed (f v, rest) | Failed -> Failed

let ( *> ) (p : 'a parser) (q : 'b parser) : 'b parser = p >>= fun _ -> q

let ( <* ) (p : 'a parser) (q : 'b parser) : 'a parser =
  p >>= fun v ->
  q >>= fun _ -> fun rest -> Parsed (v, rest)

let ( <|> ) (p : 'a parser) (q : 'a parser) : 'a parser =
 fun input ->
  match p input with Failed -> q input | Parsed (v, rest) -> Parsed (v, rest)

let many (p : 'a parser) : 'a list parser =
  let rec helper acc input =
    match p input with
    | Parsed (v, rest) -> helper (v :: acc) rest
    | Failed -> Parsed (List.rev acc, input)
  in
  helper []

let many1 (p : 'a parser) : 'a list parser =
  p >>= fun v ->
  many p >>| fun acc -> v :: acc

let char (c : char) : char parser = function
  | x :: xs when x = c -> Parsed (c, xs)
  | _ -> Failed

let string (s : string) : unit parser =
  let chars = List.of_seq (String.to_seq s) in
  List.fold_left (fun acc c -> acc *> char c) (char (List.hd chars)) (List.tl chars)
  >>| fun _ -> ()

let ws = many (char ' ' <|> char '\t' <|> char '\n')
let eof = function [] -> Parsed ((), []) | _ -> Failed

let digit : int parser = function
  | x :: xs when '0' <= x && x <= '9' ->
      Parsed (int_of_char x - int_of_char '0', xs)
  | _ -> Failed

let number : int parser =
  many1 digit >>| List.fold_left (fun acc x -> (acc * 10) + x) 0

let variable : string parser =
  let letter = function
    | x :: xs when 'a' <= x && x <= 'z' -> Parsed (x, xs)
    | _ -> Failed
  in
  many1 letter >>| List.to_seq >>| String.of_seq

let rec expr input =
  let binop op f =
    term >>= fun e1 ->
    ws *> op *> ws *> expr >>| fun e2 -> f e1 e2
  in
  (binop (char '+') (fun e1 e2 -> Add (e1, e2))
  <|> binop (char '-') (fun e1 e2 -> Sub (e1, e2))
  <|> term)
    input

and term input =
  let binop op f =
    factor >>= fun e1 ->
    ws *> op *> ws *> term >>| fun e2 -> f e1 e2
  in
  (binop (char '*') (fun e1 e2 -> Mul (e1, e2))
  <|> binop (char '/') (fun e1 e2 -> Div (e1, e2))
  <|> factor)
    input

and factor input =
  (number >>| (fun n -> Int n)
  <|> (variable >>| fun v -> Var v)
  <|> (char '(' *> ws *> expr <* ws <* char ')')
  <|> (string "let" *> ws *> variable >>= fun var ->
       ws *> char '=' *> ws *> expr >>= fun e1 ->
       ws *> string "in" *> ws *> expr >>| fun e2 ->
       Let (var, e1, e2)))
    input

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Int n1, Int n2 -> n1 = n2
  | Var v1, Var v2 -> v1 = v2
  | Add(a1, b1), Add(a2, b2) -> expr_eq a1 a2 && expr_eq b1 b2
  | Sub(a1, b1), Sub(a2, b2) -> expr_eq a1 a2 && expr_eq b1 b2
  | Mul(a1, b1), Mul(a2, b2) -> expr_eq a1 a2 && expr_eq b1 b2
  | Div(a1, b1), Div(a2, b2) -> expr_eq a1 a2 && expr_eq b1 b2
  | Let(v1, e1a, e1b), Let(v2, e2a, e2b) ->
      v1 = v2 && expr_eq e1a e2a && expr_eq e1b e2b
  | _ -> false

let is_power_of_two n = n > 0 && (n land (n - 1)) = 0

let log2 n = 
  let rec aux acc = function
    | 1 -> acc
    | n -> aux (acc + 1) (n lsr 1)
in
aux 0 n

let rec fold_constants = function
  | Add (e1, e2) -> 
    let e1' = fold_constants e1 in
    let e2' = fold_constants e2 in
    (match (e1', e2') with
    | Int a, Int b -> Int (a + b)
    | _, Sub(e3, e4) when expr_eq e4 e1' -> e3
    | Sub(e3, e4), _ when expr_eq e4 e2' -> e3
    | _ -> Add (e1', e2'))
| Sub (e1, e2) -> 
    let e1' = fold_constants e1 in
    let e2' = fold_constants e2 in
    (match (e1', e2') with
    | Int a, Int b -> Int (a - b)
    | Add(a, b), e when expr_eq e a -> b
    | Add(a, b), e when expr_eq e b -> a
    | _ -> Sub (e1', e2'))
| Mul (e1, e2) ->
    let e1' = fold_constants e1 in
    let e2' = fold_constants e2 in
    (match (e1', e2') with
    | Int a, Int b -> Int (a * b)
    | Int n, e when is_power_of_two n -> Shl (e, log2 n)
    | e, Int n when is_power_of_two n -> Shl (e, log2 n)
    | _ -> Mul (e1', e2'))
| Div (e1, e2) -> 
    let e1' = fold_constants e1 in
    let e2' = fold_constants e2 in
    (match (e1', e2') with
    | Int a, Int b when b <> 0 -> Int (a / b)
    | _ -> Div (e1', e2'))
| Let (var, e1, e2) ->
    Let (var, fold_constants e1, fold_constants e2)
| Shl (e, k) ->
    let e' = fold_constants e in
    (match  e' with
    | Int n -> Int (n lsl k)
    | _ -> Shl (e', k))
| e -> e
  
let program =
  string "let f" *> ws *> many (variable <* ws) >>= fun args_names ->
  ws *> char '=' *> ws *> expr >>= fun function_body ->
  ws
  *> string "let main = print_int ("
  *> ws *> char 'f' *> ws
  *> many (number <* ws)
  >>= fun args ->
  char ')' *> ws *> eof >>= fun _ ->
  fun rest -> Parsed ({ args_names; function_body; args }, rest)

let parse input = program input