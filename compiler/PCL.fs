namespace Compiler

open FSharp.Text.Lexing
open LLVMSharp

module PCL =

  [<EntryPoint>]
  let main argv =
      let parse input =
        let lexbuf = LexBuffer<_>.FromString input
        Parser.start Lexer.read lexbuf

      let filename = if argv.Length >= 1 then argv.[0] else "../examples/declarations2.pcl"
      Helpers.Error.FileName <- System.IO.Path.GetFullPath filename
      let input = System.IO.File.ReadAllText filename

      try
        match parse input with
        | Some result -> printfn "errors:\n%A" Helpers.Error.Parser.errorList ; Engine.Analyze result |> ignore
        | None -> printfn "errors:\n%A\n\nNo input given" Helpers.Error.Parser.errorList
      with
        | Helpers.Error.Lexer.LexerException e -> printfn "Lex Exception -> %s" <| Helpers.Error.StringifyError e
        | Helpers.Error.Parser.ParserException e -> printfn "Parse Exception -> %s" <| Helpers.Error.StringifyError e
        | e -> printfn "%A" e

      (* LLVM *)
      // let modu = LLVM.ModuleCreateWithName "LLVMSharpIntro"

      // LLVM.X86FP80Type

      // LLVM.PrintModuleToFile (modu, "test.txt", ref null) |> ignore

      0