module Program

open FSharp.Text.Lexing
open Parser

open LLVMSharp

[<EntryPoint>]
let main argv =
    let readLexemes str =
      let lexbuf = LexBuffer<_>.FromString str
      let rec aux lexbuf =
        let x = Lexer.read lexbuf
        if x = EOF then []
        else x :: aux lexbuf
      aux lexbuf
     
    // let parse input =
    //   let lexbuf = LexBuffer<_>.FromString input
    //   let res = Parser.start Lexer.read lexbuf
    //   res

    // let input = System.IO.File.ReadAllText "../examples/hello.mba"

    // try
    //   let l = parse input
    //   match l with
    //   | Some stmts ->
    //     for stmt in stmts do
    //       printfn "%A" stmt
    //   | None -> printfn "No input"
      
    // with
    //   | e -> printfn "Error occured: \n%s" (e.Message)

    let input = System.IO.File.ReadAllText "../examples/hello.mba"
    let lexemes = readLexemes input

    for lexeme in lexemes do
      printfn "%A" lexeme

    (* LLVM *)
    // let modu = LLVM.ModuleCreateWithName "LLVMSharpIntro"

    // LLVM.PrintModuleToFile (modu, "test.txt", ref null) |> ignore

    0