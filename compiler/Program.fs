module Program

open FSharp.Text.Lexing

open LLVMSharp

let rec evalExpr expr =
  match expr with
  | Tokens.RExpression e -> 
    match e with
    | Tokens.Binop (e1, op, e2) -> 
      match op with
      | Tokens.Add  -> evalExpr e1 + evalExpr e2
      | Tokens.Sub  -> evalExpr e1 - evalExpr e2
      | Tokens.Mult -> evalExpr e1 * evalExpr e2
      | _ -> 0
    | Tokens.IntConst i -> i
    | _ -> 0
  | _ -> 0


let evalStatement stmt =
  match stmt with
  | Tokens.Assign (lval, expr) -> printfn "%A" <| evalExpr expr
  | _ -> ()

let eval program =
  let name, body = program
  let decls, stmts = body
  for s in stmts do
    evalStatement s
  ()


[<EntryPoint>]
let main argv =
    let readLexemes str =
      let lexbuf = LexBuffer<_>.FromString str
      let rec aux lexbuf =
        let x = Lexer.read lexbuf
        if x = Parser.EOF then []
        else x :: aux lexbuf
      aux lexbuf
     
    let parse input =
      let lexbuf = LexBuffer<_>.FromString input
      Parser.start Lexer.read lexbuf

    // try
    //   let l = parse input
    //   match l with
    //   | Some stmts ->
    //     for stmt in stmts do
    //       printfn "%A" stmt
    //   | None -> printfn "No input"
      
    // with
    //   | e -> printfn "Error occured: \n%s" (e.Message)

    let input = if argv.Length >= 1 then System.IO.File.ReadAllText argv.[0] else System.IO.File.ReadAllText "../examples/rvalues.pcl"

    try
      match parse input with
      | Some result -> printfn "%A" result
      | None -> printfn "No input given"
    with
      | e -> printfn "%s" e.Message

    (* LLVM *)
    // let modu = LLVM.ModuleCreateWithName "LLVMSharpIntro"

    // LLVM.PrintModuleToFile (modu, "test.txt", ref null) |> ignore

    0