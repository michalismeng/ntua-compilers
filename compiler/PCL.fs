namespace Compiler

open FSharp.Text.Lexing
open LLVMSharp

module PCL =

  [<EntryPoint>]
  let main argv =
    (* Get the filename that is to be processed and store it for future reference *)
    let filename = if argv.Length >= 1 then argv.[0] else "../examples/declarations2.pcl"
    Helpers.Error.FileName <- System.IO.Path.GetFullPath filename

    (* Setup the input text *)
    let input = System.IO.File.ReadAllText filename

    (* Parse and perform semantic analysis *)
    try
      let parse input =
        let lexbuf = LexBuffer<_>.FromString input
        Parser.start Lexer.read lexbuf

      match parse input with
      | Some result -> printfn "errors:\n%A" Helpers.Error.Parser.errorList ; Engine.Analyze result |> ignore
      | None -> printfn "errors:\n%A\n\nNo input given" Helpers.Error.Parser.errorList
    with
      | Helpers.Error.Lexer.LexerException e -> printfn "Lex Exception -> %s" <| Helpers.Error.StringifyError e
      | Helpers.Error.Parser.ParserException e -> printfn "Parse Exception -> %s" <| Helpers.Error.StringifyError e
      | Helpers.Error.Semantic.SemanticException e -> printfn "Semantic Exception -> %s" <| Helpers.Error.StringifyError e
      | Helpers.Error.Symbolic.SymbolicException e -> printfn "Symbolic Exception -> %s" <| Helpers.Error.StringifyError e
      | e -> printfn "%A" e

      

      (* LLVM *)
      // let theModule = LLVM.ModuleCreateWithName "PCL Compiler"
      // let theBuilder = LLVM.CreateBuilder ()
      // let theFalseBool = LLVMBool 0
      // let theNull() = LLVMValueRef (nativeint 0)

      // let lhs = LLVM.ConstInt (LLVM.Int32Type (), 1UL, theFalseBool)
      // let rhs = LLVM.ConstInt (LLVM.Int32Type (), 2UL, theFalseBool)

      // // define a global array
      // let theVars = LLVM.AddGlobal (theModule, LLVM.ArrayType (LLVM.Int32Type(), 32u), "vars")   // ??
      // LLVM.SetLinkage (theVars, LLVMLinkage.LLVMLinkerPrivateLinkage)
      // LLVM.SetInitializer (theVars, LLVM.ConstArray (LLVM.Int32Type (), LLVM.ConstInt (LLVM.Int32Type (), 0UL, theFalseBool) |> List.replicate 32 |> Array.ofList))
      // LLVM.SetAlignment (theVars, 16u)

      // // define a global pointer to int32
      // let thePtr = LLVM.AddGlobal (theModule, LLVM.ArrayType (LLVM.PointerType (LLVM.Int32Type (), 0u), 16u), "testPtr")

      // // define a 80-bit float (this is used in the exercise)
      // let theGloriousFloat = LLVM.AddGlobal (theModule, LLVM.X86FP80Type(), "testFloat")

      // // define the main function
      // let theFunction = LLVM.AddFunction (theModule, "main", LLVM.FunctionType (LLVM.Int32Type (), Array.ofList [], false))
      // LLVM.SetFunctionCallConv (theFunction, LLVMCallConv.LLVMX86FastcallCallConv |> uint32)

      // let theBasicBlock = LLVM.AppendBasicBlock (theFunction, "entry")
      // LLVM.PositionBuilderAtEnd (theBuilder, theBasicBlock)

      // let x = LLVM.BuildCall (theBuilder, LLVM.GetNamedFunction (theModule, "main"), Array.ofList [], "") // LLVM.BuildAdd (theBuilder, lhs, rhs, "addtmp")
      // let x = LLVM.BuildRet (theBuilder, LLVM.ConstInt (LLVM.Int32Type (), 0UL, theFalseBool))

      // let functionType = LLVM.FunctionType (LLVM.VoidType (), [LLVM.Int32Type ()] |> Array.ofList, false)
      // let testFunction = LLVM.AddFunction (theModule, "testFunction", functionType)
      
      // // check that the module is ok and emit code to console
      // if LLVM.VerifyModule (theModule, LLVMVerifierFailureAction.LLVMPrintMessageAction, ref null) <> theFalseBool then
      //   printfn "Erroneuous module"
      // LLVM.DumpModule theModule

      // LLVM.PrintModuleToFile (modu, "test.txt", ref null) |> ignore

    0