type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Int of int
  | Var of string
  | Let of string * expr * expr

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