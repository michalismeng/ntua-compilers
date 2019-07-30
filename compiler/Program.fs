namespace Compiler

open FSharp.Text.Lexing
open LLVMSharp

module Program =

  [<EntryPoint>]
  let main argv =
      let parse input =
        let lexbuf = LexBuffer<_>.FromString input
        Parser.start Lexer.read lexbuf

      let filename = if argv.Length >= 1 then argv.[0] else "../examples/semantic.pcl"
      Helpers.Error.FileName <- System.IO.Path.GetFullPath filename
      let input = System.IO.File.ReadAllText filename
      
      try
        match parse input with
        | Some result -> printfn "errors:\n%A" Helpers.Error.Parser.errorList ; Semantic.Analyze result
        | None -> printfn "No input given"
      with
        | Helpers.Error.Lexer.LexerException s -> printfn "Lex Exception -> %s" s
        | e -> printfn "%A" e

      (* LLVM *)
      // let modu = LLVM.ModuleCreateWithName "LLVMSharpIntro"

      // LLVM.PrintModuleToFile (modu, "test.txt", ref null) |> ignore

      0