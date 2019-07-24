module Program

open FSharp.Text.Lexing
open Tokens

open LLVMSharp

[<EntryPoint>]
let main argv =
    // let readLexemes str =
    //   let lexbuf = LexBuffer<_>.FromString str
    //   let rec aux lexbuf =
    //     let x = Lexer.read lexbuf
    //     if x = EOF then []
    //     else x :: aux lexbuf
    //   aux lexbuf
     
    let parse input =
      let lexbuf = LexBuffer<_>.FromString input
      let res = Parser.start Lexer.read lexbuf
      res

    let input = System.IO.File.ReadAllText "../examples/hello.mba"

    try
      let l = parse input
      match l with
      | Some stmts ->
        for stmt in stmts do
          printfn "%A" stmt
      | None -> printfn "No input"
      
    with
      | e -> printfn "Error occured: \n%s" (e.Message)

    let modu = LLVM.ModuleCreateWithName "LLVMSharpIntro"

    LLVM.PrintModuleToFile (modu, "test.txt", ref null) |> ignore

    // try
    //   let lexemes = readLexemes input
    //   for lexeme in lexemes do
    //     printfn "%s" <| MinibasicToken.ToString lexeme
    // with
    //   | e -> printfn "Error occured: \n%s" (e.Message)
    0