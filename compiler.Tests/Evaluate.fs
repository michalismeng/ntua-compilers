namespace Compiler.Tests

open System
open Compiler

module Evaluate =
    let rec Expr expr =
      match expr with
      | PCL.LExpression e ->
        match e with
          | PCL.Identifier x -> int(x.[0]) + 100
          | PCL.Brackets (l, e') -> printfn "Doing brackets %A %A" l e'; Expr <| PCL.LExpression l |> ignore; 500 + Expr e'
          | PCL.Dereference e' -> printfn "Doing dereference %A" e'; Expr e' * 4
          | _ -> 0
      | PCL.RExpression e -> 
        match e with
        | PCL.Binop (e1, op, e2) -> 
          match op with
          | PCL.Add  -> Expr e1 + Expr e2
          | PCL.Sub  -> Expr e1 - Expr e2
          | PCL.Mult -> Expr e1 * Expr e2
          | PCL.Div  -> Expr e1 / Expr e2
          | _ -> 0
        | PCL.IntConst i -> i
        | PCL.RParens e' -> Expr (PCL.RExpression e')
        | PCL.AddressOf e' -> match e' with
                                | PCL.LExpression x -> printfn "Doing addressOF %A" x; 1000 + Expr (PCL.LExpression x)
                                | PCL.RExpression _ -> raise <| Exception "Cannot get address of r-value"
        | _ -> 0


    let Statement stmt =
      match stmt with
      | PCL.Assign (lval, expr) -> printfn "%A" <| Expr expr
      | _ -> ()

    let Program program =
      let name, body = program
      let decls, stmts = body
      for s in stmts do
        Statement s
      ()