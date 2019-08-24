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

      let symTable = SymbolTable.CreateSymbolTable()
      printfn "%A" symTable

      let scope, symTable = SymbolTable.OpenScope symTable "p"
      printfn "%A %A\n" scope symTable

      let s1, s2 = SymbolTable.Variable ("s1", PCL.Real), SymbolTable.Variable ("s2", PCL.Real)
      let i1, i2 = SymbolTable.Variable ("i1", PCL.Integer), SymbolTable.Variable ("i2", PCL.Integer)

      let symTable = SymbolTable.AddDeclarationToTable symTable s1
      let symTable = SymbolTable.AddDeclarationToTable symTable s2
      let symTable = SymbolTable.AddDeclarationToTable symTable i1

      printfn "%A" <| List.head symTable

      let scope, symTable = SymbolTable.OpenScope symTable "pr"
      printfn "%A" <| symTable

      let _, symTable = SymbolTable.CloseScope symTable
      printfn "%A" symTable
      printfn "---------------------"

      try
        let fpr = SymbolTable.Forward ("pr", [PCL.ProcessParam ("p1", PCL.Integer, PCL.ByValue); PCL.ProcessParam ("p2", PCL.Integer, PCL.ByValue)], PCL.Unit)
        let pr = SymbolTable.Process ("pr", [PCL.ProcessParam ("p1", PCL.Integer, PCL.ByValue); PCL.ProcessParam ("p2", PCL.Integer, PCL.ByValue)], PCL.Unit)
        let p1 = SymbolTable.Variable ("p1", PCL.Integer)
        let p2 = SymbolTable.Variable ("p2", PCL.Integer)

        let symTable = SymbolTable.AddDeclarationToTable symTable fpr
        let symTable = SymbolTable.AddDeclarationToTable symTable pr
        let symTable = SymbolTable.AddDeclarationToTable symTable i2

        let _, symTable = SymbolTable.OpenScope symTable "pr"

        let symTable = SymbolTable.AddDeclarationToTable symTable p1
        let symTable = SymbolTable.AddDeclarationToTable symTable p2

        printfn "%A" symTable
      with
      | e -> printfn "%A" e


      // try
      //   match parse input with
      //   | Some result -> printfn "errors:\n%A" Helpers.Error.Parser.errorList ; Semantic.Analyze result
      //   | None -> printfn "No input given"
      // with
      //   | Helpers.Error.Lexer.LexerException s -> printfn "Lex Exception -> %s" s
      //   | e -> printfn "%A" e

      (* LLVM *)
      // let modu = LLVM.ModuleCreateWithName "LLVMSharpIntro"

      // LLVM.PrintModuleToFile (modu, "test.txt", ref null) |> ignore

      0