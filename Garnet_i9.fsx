module Garnet_i9 // таб не удалялалсь + в Run не надо писать Garnet_i9.run 

open System
open System.Text.RegularExpressions

// Определение типов токенов
type LexicalUnit =
  | Numeric of int
  | Logical of bool
  | Identifier of string
  | LetKeyword
  | LetRecKeyword
  | InKeyword
  | IfKeyword
  | ThenKeyword
  | ElseKeyword
  | FunctionKeyword
  | ArrowSymbol
  | BinaryOperator of string
  | Assignment
  | LeftParenthesis
  | RightParenthesis
  | CommaSeparator

// Абстрактное синтаксическое дерево
type SyntaxTree =
  | Number of int
  | Boolean of bool
  | Variable of string
  | LetBinding of string * SyntaxTree * SyntaxTree
  | LetRecBinding of string * SyntaxTree * SyntaxTree
  | Conditional of SyntaxTree * SyntaxTree * SyntaxTree
  | FunctionDef of string list * SyntaxTree
  | FunctionCall of SyntaxTree * SyntaxTree list
  | BinaryOperation of string * SyntaxTree * SyntaxTree
  | EmptyList
  | ListCons of SyntaxTree * SyntaxTree

// Значения для интерпретатора
type RuntimeValue =
  | Integer of int
  | BooleanValue of bool
  | Closure of string list * SyntaxTree * Environment
  | RecursiveClosure of string * string list * SyntaxTree * Environment
  | EmptyListValue
  | ConsValue of RuntimeValue * RuntimeValue
  | Thunk of (unit -> RuntimeValue)

and Environment = Map<string, RuntimeValue>

// Функция токенизации
let lex input =
  let tokenPatterns = [
    ("letrec", fun (_: Match) -> LetRecKeyword)
    ("let", fun (_: Match) -> LetKeyword)
    ("in", fun (_: Match) -> InKeyword)
    ("if", fun (_: Match) -> IfKeyword)
    ("then", fun (_: Match) -> ThenKeyword)
    ("else", fun (_: Match) -> ElseKeyword)
    ("fun", fun (_: Match) -> FunctionKeyword)
    ("true", fun (_: Match) -> Boolean true)
    ("false", fun (_: Match) -> Boolean false)
    ("->", fun (_: Match) -> ArrowSymbol)
    ("\\+", fun (_: Match) -> BinaryOperator "+")
    ("-", fun (_: Match) -> BinaryOperator "-")
    ("\\*", fun (_: Match) -> BinaryOperator "*")
    ("=", fun (_: Match) -> Assignment)
    ("\\(", fun (_: Match) -> LeftParenthesis)
    ("\\)", fun (_: Match) -> RightParenthesis)
    (",", fun (_: Match) -> CommaSeparator)
    ("[0-9]+", fun (m: Match) -> Numeric (int m.Value))
    ("[a-zA-Z_][a-zA-Z0-9_]*", fun (m: Match) -> Identifier m.Value)
  ]

  let rec lexer pos =
    if pos >= input.Length then []
    else
      let remaining = input.Substring(pos)
      let whitespaceMatch = Regex.Match(remaining, @"^\s+")
      if whitespaceMatch.Success then
        lexer (pos + whitespaceMatch.Length)
      else
        let rec matchToken patterns =
          match patterns with
          | [] -> None
          | (pattern, f)::rest ->
              let m = Regex.Match(remaining, "^" + pattern)
              if m.Success then Some(f m, m.Length) else matchToken rest
        match matchToken tokenPatterns with
        | Some(token, len) -> token :: lexer (pos + len)
        | None -> failwithf "Unexpected character at position %d: '%c'" pos input.[pos]

  lexer 0

// Парсер выражений
let rec parseExpression tokens =
  match tokens with
  | LetKeyword :: Identifier v :: Assignment :: rest ->
      let firstExpr, remainingTokens = parseExpression rest
      match remainingTokens with
      | InKeyword :: remainingTokens2 ->
          let secondExpr, remainingTokens3 = parseExpression remainingTokens2
          (LetBinding(v, firstExpr, secondExpr), remainingTokens3)
      | _ -> failwith "Expected 'in' after let binding"
  | LetRecKeyword :: Identifier v :: Assignment :: rest ->
      let firstExpr, remainingTokens = parseExpression rest
      match remainingTokens with
      | InKeyword :: remainingTokens2 ->
          let secondExpr, remainingTokens3 = parseExpression remainingTokens2
          (LetRecBinding(v, firstExpr, secondExpr), remainingTokens3)
      | _ -> failwith "Expected 'in' after letrec binding"
  | IfKeyword :: rest ->
      let condition, remainingTokens = parseExpression rest
      match remainingTokens with
      | ThenKeyword :: remainingTokens2 ->
          let thenExpr, remainingTokens3 = parseExpression remainingTokens2
          match remainingTokens3 with
          | ElseKeyword :: remainingTokens4 ->
              let elseExpr, remainingTokens5 = parseExpression remainingTokens4
              (Conditional(condition, thenExpr, elseExpr), remainingTokens5)
          | _ -> failwith "Expected 'else' after then"
      | _ -> failwith "Expected 'then' after if"
  | FunctionKeyword :: rest ->
      let rec parseParameters toks acc =
        match toks with
        | Identifier v :: rest' -> parseParameters rest' (v::acc)
        | ArrowSymbol :: rest' -> (List.rev acc, rest')
        | _ -> failwith "Expected parameters and '->' in function"
      let parameters, remainingTokens = parseParameters rest []
      let body, remainingTokens2 = parseExpression remainingTokens
      (FunctionDef(parameters, body), remainingTokens2)
  | _ -> parseBinaryOperation tokens

and parseBinaryOperation tokens =
  let rec parseLoop left toks =
    match toks with
    | BinaryOperator op :: rest ->
        let right, remainingTokens = parsePrimary rest
        let combined = BinaryOperation(op, left, right)
        parseLoop combined remainingTokens
    | LeftParenthesis :: _ ->
        let args, remainingTokens = parseCall toks
        match left with
        | Variable v -> parseLoop (FunctionCall(left, args)) remainingTokens
        | _ -> failwith "Can only call variables as functions"
    | _ -> (left, toks)
  let left, remainingTokens = parsePrimary tokens
  parseLoop left remainingTokens

and parsePrimary tokens =
  match tokens with
  | Numeric i :: rest -> (Number i, rest)
  | Boolean b :: rest -> (Boolean b, rest)
  | Identifier v :: rest -> (Variable v, rest)
  | LeftParenthesis :: rest ->
      let expr, remainingTokens = parseExpression rest
      match remainingTokens with
      | RightParenthesis :: remainingTokens2 -> (expr, remainingTokens2)
      | _ -> failwith "Expected ')'"
  | _ -> failwith "Unexpected token in primary"

and parseCall tokens =
  let rec parseArguments toks acc =
    match toks with
    | RightParenthesis :: rest -> (List.rev acc, rest)
    | _ ->
        let expr, remainingTokens = parseExpression toks
        match remainingTokens with
        | CommaSeparator :: rest2 -> parseArguments rest2 (expr::acc)
        | RightParenthesis :: rest2 -> (List.rev (expr::acc), rest2)
        | _ -> failwith "Expected ',' or ')' in function call arguments"
  match tokens with
  | LeftParenthesis :: rest ->
      let args, remainingTokens = parseArguments rest []
      (args, remainingTokens)
  | _ -> ([], tokens)

// Интерпретатор
let rec evaluate env expr =
  match expr with
  | Number i -> Integer i
  | Boolean b -> BooleanValue b
  | Variable x ->
      if env.ContainsKey x then forceValue env.[x]
      else failwithf "Unbound variable %s" x
  | LetBinding(x, e1, e2) ->
      let v1 = evaluate env e1
      let env' = env.Add(x, v1)
      evaluate env' e2
  | LetRecBinding(f, e1, e2) ->
      let recEnv = ref env
      let closure =
        match e1 with
        | FunctionDef(args, body) -> RecursiveClosure(f, args, body, recEnv.Value)
        | _ -> failwith "LetRec expects a lambda"
      recEnv.Value <- env.Add(f, closure)
      evaluate recEnv.Value e2
  | Conditional(cond, thenE, elseE) ->
      match evaluate env cond with
      | BooleanValue true -> evaluate env thenE
      | BooleanValue false -> evaluate env elseE
      | _ -> failwith "Condition must be boolean"
  | FunctionDef(args, body) -> Closure(args, body, env)
  | FunctionCall(fnExpr, argsExprs) ->
      let fnVal = evaluate env fnExpr |> forceValue
      let argVals = argsExprs |> List.map (fun e -> Thunk(fun () -> evaluate env e))

      match fnVal with
      | Closure(args, body, cloEnv) when args.Length = argVals.Length ->
          let newEnv =
              List.zip args argVals
              |> List.fold (fun acc (k,v) -> Map.add k v acc) cloEnv
          evaluate newEnv body
      | RecursiveClosure(f, args, body, cloEnv) when args.Length = argVals.Length ->
          let recEnv = ref cloEnv
          let closure = RecursiveClosure(f, args, body, recEnv.Value)
          recEnv.Value <- cloEnv.Add(f, closure)
          let newEnv =
              List.zip args argVals
              |> List.fold (fun acc (k,v) -> Map.add k v acc) recEnv.Value
          evaluate newEnv body
      | _ -> failwith "Trying to call non-function or wrong arg count"
  | BinaryOperation(op, e1, e2) ->
      let v1 = evaluate env e1 |> forceValue
      let v2 = evaluate env e2 |> forceValue
      match v1, v2 with
      | Integer i1, Integer i2 ->
          match op with
          | "+" -> Integer (i1 + i2)
          | "-" -> Integer (i1 - i2)
          | "*" -> Integer (i1 * i2)
          | _ -> failwithf "Unknown operator %s" op
      | _ -> failwith "Binary operations only supported for integers"
  | EmptyList -> EmptyListValue
  | ListCons(h, t) ->
      let vh = evaluate env h |> forceValue
      let vt = evaluate env t |> forceValue
      ConsValue(vh, vt)

and forceValue v =
  match v with
  | Thunk thunk -> thunk()
  | _ -> v

let execute code =
  let tokens = lex code
  let expr, rest = parseExpression tokens
  if rest <> [] then failwith "Unparsed tokens remain"
  evaluate Map.empty expr
