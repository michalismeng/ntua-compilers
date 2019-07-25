module Program

open FSharp.Text.Lexing

open LLVMSharp

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

    let input = if argv.Length >= 1 then System.IO.File.ReadAllText argv.[0] else System.IO.File.ReadAllText "../examples/hello.pcl"

    try
      parse input
    with
      | e -> printfn "%s" e.Message

    (* LLVM *)
    // let modu = LLVM.ModuleCreateWithName "LLVMSharpIntro"

    // LLVM.PrintModuleToFile (modu, "test.txt", ref null) |> ignore

    0