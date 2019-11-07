namespace Compiler

open FSharp.Text.Lexing
open CodeGenerator
open LLVMSharp

module PCL =
  let private verifyAndDump _module =
    // let mutable func = LLVM.GetFirstFunction _module
    // while func.Pointer <> System.IntPtr.Zero do
    //   printfn "Checking %s" <| func.GetValueName ()
    //   LLVM.VerifyFunction (func, LLVMVerifierFailureAction.LLVMPrintMessageAction) |> ignore
    //   func <- LLVM.GetNextFunction func
    // done

    if LLVM.VerifyModule (_module, LLVMVerifierFailureAction.LLVMPrintMessageAction, ref null) <> LLVMBool 0 then
      printfn "Erroneuous module\n"
      // LLVM.DumpModule _module
    else
      LLVM.DumpModule _module
      LLVM.PrintModuleToFile (_module, "test.txt", ref null) |> ignore

  let private combined program = async {
    let! semantic = Async.StartChild <| async { return Engine.Analyze program }
    let! arBuilder = Async.StartChild <| async { return GenerateARTypes program }

    let! res1 = semantic
    let! res2 = arBuilder

    return (res1, res2)
  }

  [<EntryPoint>]
  let main argv =
    (* Get the filename that is to be processed and store it for future reference *)
    let filename = if argv.Length >= 1 then argv.[0] else "../examples/semStrings.pcl"
    Helpers.Error.FileName <- System.IO.Path.GetFullPath filename

    (* Setup the input text *)
    let input = System.IO.File.ReadAllText filename

    (* Parse and perform semantic analysis *)
    try
      let parse input =
        let lexbuf = LexBuffer<_>.FromString input
        Parser.start Lexer.read lexbuf

      match parse input with
      | Some program -> printfn "errors:\n%A" Helpers.Error.Parser.errorList

                        let semanticAnalysis, semanticInstruction = Engine.Analyze program
                        let arTypes, globalInstructions = GenerateARTypes program
                        let labelNames = GenerateLabelledNames program

                        if not(semanticAnalysis) then
                          printfn "Semantic Analysis failed. Goodbye..."
                          exit 1

                        let topLevelFunction = 
                          match semanticInstruction with
                          | Base.SemDeclFunction (n, t, il) -> (n, t, il)
                          | _                      -> raise <| Helpers.Error.InternalException "Top Level Instruction must be a function"

                        let normalizedHierarchy = Engine.NormalizeInstructionHierarchy topLevelFunction |> Map.toList |> List.map snd

                        // printfn "normalized %A" normalizedHierarchy

                        // Can run in parallel with a few adjustments in AR type generation
                        // let semanticAnalysis, arTypes =
                        //   combined program |> 
                        //   Async.RunSynchronously

                        let externalFunctions = 
                          Helpers.ExternalFunctions.ExternalIO
                          |> List.map (fun (n, l, ret) -> (n, (List.map (fun (x,y,z) -> y,z) l), ret, [LLVMLinkage.LLVMExternalLinkage]))

                        CodeGenerator.GenerateLLVMModule ()

                        let theFPM = LLVM.CreateFunctionPassManagerForModule CodeModule.theModule
                        LLVM.AddBasicAliasAnalysisPass theFPM
                        LLVM.AddPromoteMemoryToRegisterPass theFPM
                        LLVM.AddInstructionCombiningPass theFPM
                        LLVM.AddReassociatePass theFPM
                        LLVM.AddGVNPass theFPM
                        LLVM.AddCFGSimplificationPass theFPM
                        LLVM.InitializeFunctionPassManager theFPM |> ignore

                        // generate global symbols (global variables and function declarations, external and private)
                        globalInstructions |> List.iter (fun gd -> gd ||> GenerateGlobalVariable)
                        List.iter (fun (x, y, z, w) -> CodeGenerator.GenerateFunctionRogue x y z w |> ignore) externalFunctions
                        List.iter (fun f -> CodeGenerator.GenerateFunctionPrototype arTypes f Base.Unit [] |> ignore) normalizedHierarchy

                        // Generate main function which calls the program's entry function
                        CodeGenerator.GenerateMain (fst normalizedHierarchy.Head) |> ignore

                        // Generate all program functions
                        List.iter (fun func -> CodeGenerator.GenerateFunctionCode arTypes labelNames func |> ignore) normalizedHierarchy

                        // * 'Custom Optimization Pass' which will transform all allocas to bitcasts of one big alloca 
                        // let theFunctionToOptimize = LLVM.GetNamedFunction (CodeModule.theModule, "factorial.calc")
                        // let mutable fInstr = ((theFunctionToOptimize.GetBasicBlocks ()).[0]).GetFirstInstruction ()
                        // let mutable shouldRun = true
                        // while fInstr.Pointer <> System.IntPtr.Zero do
                        //   if shouldRun && (fInstr.IsAAllocaInst ()).Pointer <> System.IntPtr.Zero then
                        //     shouldRun <- false
                        //     let newInstr = GenerateLocal <| Base.Integer.ToLLVM ()
                        //     fInstr.ReplaceAllUsesWith (newInstr)
                        //   fInstr <- fInstr.GetNextInstruction ()
                        // done

                        // LLVM.RunFunctionPassManager (theFPM, theFunctionToOptimize) |> ignore
                        // LLVM.RunPassManager (theFPM, CodeModule.theModule) |> ignore

                        verifyAndDump CodeModule.theModule
                       
      | None -> printfn "errors:\n%A\n\nNo input given" Helpers.Error.Parser.errorList
    with
      | Helpers.Error.Lexer.LexerException e -> printfn "Lex Exception -> %s" <| Helpers.Error.StringifyError e
      | Helpers.Error.Parser.ParserException e -> printfn "Parse Exception -> %s" <| Helpers.Error.StringifyError e
      | Helpers.Error.Semantic.SemanticException e -> printfn "Semantic Exception -> %s" <| Helpers.Error.StringifyError e
      | Helpers.Error.Symbolic.SymbolicException e -> printfn "Symbolic Exception -> %s" <| Helpers.Error.StringifyError e
      | e -> printfn "%A" e

    0