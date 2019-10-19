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
      LLVM.DumpModule _module
    else
      LLVM.DumpModule _module
      // LLVM.PrintModuleToFile (_module, "test.txt", ref null) |> ignore

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
    let filename = if argv.Length >= 1 then argv.[0] else "../examples/semInstructions.pcl"
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

                        if not(semanticAnalysis) then
                          printfn "Semantic Analysis failed. Goodbye..."
                          exit 1

                        let topLevelFunction = 
                          match semanticInstruction with
                          | Base.SemDeclFunction (n, t, il) -> (n, t, il)
                          | _                      -> raise <| Helpers.Error.InternalException "Top Level Instruction must be a function"

                        let normalizedHierarchy = Engine.NormalizeInstructionHierarchy topLevelFunction |> Map.toList |> List.map snd

                        // Can run in parallel with a few adjustments in AR type generation
                        // let semanticAnalysis, arTypes =
                        //   combined program |> 
                        //   Async.RunSynchronously

                        let externalFunctions = 
                          Helpers.ExternalFunctions.ExternalIO
                          |> List.map (fun (n, l, ret) -> (n, (List.map (fun (x,y,z) -> y) l), ret, [LLVMLinkage.LLVMExternalLinkage]))

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
                        List.iter (fun func -> CodeGenerator.GenerateFunctionCode arTypes func |> ignore) normalizedHierarchy

                        // Spuriously generate call to writeInteger
                        let m = LLVM.GetNamedFunction (CodeModule.theModule, "main")
                        let glo = LLVM.GetNamedGlobal (CodeModule.theModule, "x")
                        LLVM.PositionBuilderBefore (CodeModule.theBuilder, LLVM.GetLastInstruction (m.GetFirstBasicBlock()))
                        let glo = LowLevel.GenerateLoad glo 
                        GenerateFunctionCall "writeInteger" [glo] |> ignore


                        // * 'Custom Optimization Pass' which will transform all allocas to bitcasts of one big alloca 
                        // let theFunctionToOptimize = LLVM.GetNamedFunction (CodeModule.theModule, "hello")
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

    // (* LLVM *)

    // GenerateLLVMModule ()
    // let theModule, theBuilder = CodeModule.theModule, CodeModule.theBuilder
    // GenerateGlobalVariable "x" <| Base.Integer
    // GenerateFunctionRogue "writeInteger" [Base.Integer] Base.Unit [LLVMLinkage.LLVMExternalLinkage] |> ignore

    // let theMain = GenerateFunctionRogue "main" [] Base.Integer [LLVMLinkage.LLVMExternalLinkage]
    // let theBasicBlock = GenerateBasicBlock theMain "entry"
    // LLVM.PositionBuilderAtEnd (theBuilder, theBasicBlock)

    // LLVM.BuildStore (theBuilder, LLVM.ConstInt(LLVM.Int16Type(), 2UL, LLVMBool 0), LLVM.GetNamedGlobal (theModule, "x")) |> ignore

    // let comp = LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntNE, LLVM.GetNamedGlobal (theModule, "x"), LLVM.ConstInt(LLVM.Int16Type(), 2UL, LLVMBool 0), "teq")

    // let bbif = LLVM.AppendBasicBlock (theMain, "ifpart")
    // let bbelse = LLVM.AppendBasicBlock (theMain, "elsepart")
    // let bbendif = LLVM.AppendBasicBlock (theMain, "endifpart")

    // LLVM.BuildCondBr (theBuilder, comp, bbif, bbelse) |> ignore

    // LLVM.PositionBuilderAtEnd (theBuilder, bbif)
    // LLVM.BuildStore (theBuilder, LLVM.ConstInt(LLVM.Int16Type(), 3UL, LLVMBool 0), LLVM.GetNamedGlobal (theModule, "x")) |> ignore
    // LLVM.BuildBr (theBuilder, bbendif) |> ignore

    // LLVM.PositionBuilderAtEnd (theBuilder, bbelse)
    // LLVM.BuildStore (theBuilder, LLVM.ConstInt(LLVM.Int16Type(), 4UL, LLVMBool 0), LLVM.GetNamedGlobal (theModule, "x")) |> ignore
    // LLVM.BuildBr (theBuilder, bbendif) |> ignore

    // LLVM.PositionBuilderAtEnd (theBuilder, bbendif)

    // let glo = LLVM.BuildLoad (theBuilder, LLVM.GetNamedGlobal(theModule, "x"), "glo")
    // GenerateFunctionCall "writeInteger" [glo] |> ignore

    // LLVM.BuildRet (theBuilder, LLVM.ConstInt(LLVM.Int16Type(), 2UL, LLVMBool 0)) |> ignore

    // GenerateLLVMModule ()
    // let theModule, theBuilder = CodeModule.theModule, CodeModule.theBuilder

    // let theFPM = LLVM.CreateFunctionPassManagerForModule CodeModule.theModule
    // LLVM.AddBasicAliasAnalysisPass theFPM
    // LLVM.AddPromoteMemoryToRegisterPass theFPM
    // LLVM.AddInstructionCombiningPass theFPM
    // LLVM.AddReassociatePass theFPM
    // LLVM.AddGVNPass theFPM
    // LLVM.AddCFGSimplificationPass theFPM

    // let arTypes = [("hello", GenerateStructType (Base.Integer.ToLLVM ()) []);
    //                ("hello.add", GenerateStructType (Base.Integer.ToLLVM ()) [Base.Real; Base.Integer])] |> Map.ofList

    // GenerateGlobalVariable "theInteger" <| Base.Integer

    // GenerateFunctionPrototype arTypes ("hello", []) Base.Unit [] |> ignore
    // GenerateFunctionPrototype arTypes ("hello.add", []) Base.Integer [] |> ignore
    // GenerateFunctionRogue "writeInteger" [Base.Integer] Base.Unit [LLVMLinkage.LLVMExternalLinkage] |> ignore

    // // hello.add Function

    // let helloFunction = LLVM.GetNamedFunction (theModule, "hello.add")
    // let theBasicBlock = GenerateBasicBlock helloFunction "entry"
    // LLVM.PositionBuilderAtEnd (theBuilder, theBasicBlock)
    // LLVM.BuildRet (theBuilder, LLVM.ConstInt(Base.Integer.ToLLVM (), 15UL, LLVMBool 0)) |> ignore

    // // hello Function

    // let helloFunction = LLVM.GetNamedFunction (theModule, "hello")
    // let theBasicBlock = GenerateBasicBlock helloFunction "entry"
    // LLVM.PositionBuilderAtEnd (theBuilder, theBasicBlock)

    // let locals = GenerateLocal (LLVM.ArrayType (Base.Integer.ToLLVM (), 10u))

    // let p = LLVM.BuildBitCast (theBuilder, locals, LLVM.PointerType(Map.find "hello.add" arTypes, 0u), "tempcast")

    // GenerateStructStore p 1 (LLVM.ConstReal (Base.Real.ToLLVM(), 15.0)) |> ignore
    // GenerateStructStore p 2 (LLVM.ConstInt (Base.Integer.ToLLVM(), 10UL, LLVMBool 0)) |> ignore

    // GenerateFunctionCall "hello.add" [p] |> ignore

    // let p = LLVM.BuildBitCast (theBuilder, locals, LLVM.PointerType(Map.find "hello.add" arTypes, 0u), "tempcast")

    // GenerateStructStore p 1 (LLVM.ConstReal (Base.Real.ToLLVM(), 15.0)) |> ignore
    // GenerateStructStore p 2 (LLVM.ConstInt (Base.Integer.ToLLVM(), 10UL, LLVMBool 0)) |> ignore

    // GenerateFunctionCall "hello.add" [p] |> ignore

    // LLVM.BuildRetVoid (theBuilder) |> ignore

    // // the main

    // let theMain = GenerateFunctionRogue "main" [] Base.Integer [LLVMLinkage.LLVMExternalLinkage]
    // let theBasicBlock = GenerateBasicBlock theMain "entry"
    // LLVM.PositionBuilderAtEnd (theBuilder, theBasicBlock)
    // GenerateFunctionCall "hello" [LLVM.ConstPointerNull (LLVM.PointerType(Map.find "hello" arTypes, 0u))] |> ignore
    // LLVM.BuildRet (theBuilder, LLVM.ConstInt(Base.Integer.ToLLVM(), 1UL, LLVMBool 1)) |> ignore

    // LLVM.RunFunctionPassManager (theFPM, LLVM.GetNamedFunction (theModule, "hello")) |> ignore

    // if LLVM.VerifyModule (theModule, LLVMVerifierFailureAction.LLVMPrintMessageAction, ref null) <> LLVMBool 0 then
    //   printfn "Erroneuous module"
    // LLVM.DumpModule theModule

    // LLVM.PrintModuleToFile (theModule, "test.txt", ref null) |> ignore

    0