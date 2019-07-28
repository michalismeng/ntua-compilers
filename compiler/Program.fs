namespace Compiler

open FSharp.Text.Lexing

open LLVMSharp
open System

module Program =

  let rec evalExpr expr =
    match expr with
    | PCL.LExpression e ->
      match e with
        | PCL.Identifier x -> int(x.[0]) + 100
        | PCL.Brackets (l, e') -> printfn "Doing brackets %A %A" l e'; evalExpr <| PCL.LExpression l |> ignore; 500 + evalExpr e'
        | PCL.Dereference e' -> printfn "Doing dereference %A" e'; evalExpr e' * 4
        | _ -> 0
    | PCL.RExpression e -> 
      match e with
      | PCL.Binop (e1, op, e2) -> 
        match op with
        | PCL.Add  -> evalExpr e1 + evalExpr e2
        | PCL.Sub  -> evalExpr e1 - evalExpr e2
        | PCL.Mult -> evalExpr e1 * evalExpr e2
        | PCL.Div  -> evalExpr e1 / evalExpr e2

        | PCL.And   -> if evalExpr e1 = 1 && evalExpr e2 = 1 then 1 else 0
        | PCL.Or    -> if evalExpr e1 = 1 || evalExpr e2 = 1 then 1 else 0

        | PCL.Equals -> if evalExpr e1 = evalExpr e2 then 1 else 0
        | PCL.NotEquals -> if evalExpr e1 <> evalExpr e2 then 1 else 0
        | PCL.Less -> if evalExpr e1 < evalExpr e2 then 1 else 0
        | PCL.LessEquals -> if evalExpr e1 <= evalExpr e2 then 1 else 0
        | PCL.Greater -> if evalExpr e1 > evalExpr e2 then 1 else 0
        | PCL.GreaterEquals -> if evalExpr e1 >= evalExpr e2 then 1 else 0
        | _ -> 0
      | PCL.Unop (op, e) ->
        match op with
        | PCL.Positive -> evalExpr e
        | PCL.Negative -> -evalExpr e
        | PCL.Not      -> printfn "%A" e; if evalExpr e = 0 then 1 else 0
      | PCL.IntConst i -> i
      | PCL.RParens e' -> evalExpr (PCL.RExpression e')
      | PCL.AddressOf e' -> match e' with
                              | PCL.LExpression x -> printfn "Doing addressOF %A" x; 1000 + evalExpr (PCL.LExpression x)
                              | PCL.RExpression _ -> raise <| Exception "Cannot get address of r-value"
      | _ -> 0


  let evalStatement stmt =
    match stmt with
    | PCL.Assign (lval, expr) -> printfn "%A" <| evalExpr expr
    | _ -> ()

  let eval program =
    let name, body = program
    let decls, stmts = body
    for s in stmts do
      evalStatement s
    ()


  [<EntryPoint>]
  let main argv =
      let parse input =
        let lexbuf = LexBuffer<_>.FromString input
        Parser.start Lexer.read lexbuf

      let input = if argv.Length >= 1 then System.IO.File.ReadAllText argv.[0] else System.IO.File.ReadAllText "../examples/semantic.pcl"

      try
        match parse input with
        | Some result -> Semantic.Analyze result
        | None -> printfn "No input given"
      with
        | e -> printfn "%s" e.Message

      (* LLVM *)
      // let modu = LLVM.ModuleCreateWithName "LLVMSharpIntro"

      // LLVM.PrintModuleToFile (modu, "test.txt", ref null) |> ignore

      0