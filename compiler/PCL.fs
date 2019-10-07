namespace Compiler

open FSharp.Text.Lexing
open CodeGenerator
open LLVMSharp

module PCL =

  let private verifyAndDump _module =
    if LLVM.VerifyModule (_module, LLVMVerifierFailureAction.LLVMPrintMessageAction, ref null) <> LLVMBool 0 then
      printfn "Erroneuous module"
    else
      LLVM.DumpModule _module
      // LLVM.PrintModuleToFile (_module, "test.txt", ref null) |> ignore

  [<EntryPoint>]
  let main argv =
    // (* Get the filename that is to be processed and store it for future reference *)
    // let filename = if argv.Length >= 1 then argv.[0] else "../examples/adv_declarations.pcl"
    // Helpers.Error.FileName <- System.IO.Path.GetFullPath filename

    // (* Setup the input text *)
    // let input = System.IO.File.ReadAllText filename

    // (* Parse and perform semantic analysis *)
    // try
    //   let parse input =
    //     let lexbuf = LexBuffer<_>.FromString input
    //     Parser.start Lexer.read lexbuf

    //   match parse input with
    //   | Some program -> printfn "errors:\n%A" Helpers.Error.Parser.errorList
    //                     Engine.Analyze program |> ignore    // TODO: do not ignore result
    //                     let theModule = Module.theModule
    //                     verifyAndDump theModule
                       
    //   | None -> printfn "errors:\n%A\n\nNo input given" Helpers.Error.Parser.errorList
    // with
    //   | Helpers.Error.Lexer.LexerException e -> printfn "Lex Exception -> %s" <| Helpers.Error.StringifyError e
    //   | Helpers.Error.Parser.ParserException e -> printfn "Parse Exception -> %s" <| Helpers.Error.StringifyError e
    //   | Helpers.Error.Semantic.SemanticException e -> printfn "Semantic Exception -> %s" <| Helpers.Error.StringifyError e
    //   | Helpers.Error.Symbolic.SymbolicException e -> printfn "Symbolic Exception -> %s" <| Helpers.Error.StringifyError e
    //   | e -> printfn "%A" e

    (* LLVM *)
    let theModule, theBuilder = CodeGenerator.GenerateMain ()

    let ctx = LLVM.ContextCreate ()
    
    // CodeGenerator.GenerateGlobalVariable "theInteger" <| Base.Integer
    // CodeGenerator.GenerateGlobalVariable "theBoolean" <| Base.Boolean
    // CodeGenerator.GenerateGlobalVariable "theCharacter" <| Base.Character
    // CodeGenerator.GenerateGlobalVariable "theReal" <| Base.Real
    // CodeGenerator.GenerateGlobalVariable "theArray" <| Base.Array (Base.Integer, 2)
    // CodeGenerator.GenerateGlobalVariable "the2DArray" <| Base.Array (Base.Array (Base.Integer, 2), 4)
    // CodeGenerator.GenerateGlobalVariable "thePointer" <| Base.Ptr Base.Integer

    let addContextStruct = LLVM.StructType (Array.ofList [Base.Integer.ToLLVM (); Base.Real.ToLLVM (); Base.Integer.ToLLVM ()], true)

    GenerateFunction "addFunction" [Base.Integer; Base.Integer] <| Base.Integer
    // GenerateFunction "addFunction.Multiply" [Base.Integer] <| Base.Integer

    let parameters = [Base.Integer] |>
                     List.map (fun p -> p.ToLLVM ())
    let parameters = parameters @ [LLVM.PointerType (addContextStruct, 0u)] |> Array.ofList
    let theFunction = LLVM.AddFunction (theModule, "addFunction.Multiply", LLVM.FunctionType (Base.Integer.ToLLVM (), parameters, false))
    LLVM.SetLinkage (theFunction, LLVMLinkage.LLVMPrivateLinkage)

    // let myStruct = LLVM.StructCreateNamed (ctx, "myStruct")
    // LLVM.StructSetBody (myStruct, Array.ofList [Base.Integer.ToLLVM ()], false)


    // Multiply Function

    let multiplyFunction = LLVM.GetNamedFunction (theModule, "addFunction.Multiply")
    let theBasicBlock = LLVM.AppendBasicBlock (multiplyFunction, "entry")
    LLVM.PositionBuilderAtEnd (theBuilder, theBasicBlock)

    let lhs = LLVM.GetParam (multiplyFunction, 0u)
    let rhs = LLVM.GetParam (multiplyFunction, 1u)
    let trg = LLVM.BuildGEP (theBuilder, rhs, [LLVM.ConstInt (LLVM.Int32Type (), 0UL, LLVMBool 1); LLVM.ConstInt (LLVM.Int32Type (), 3UL, LLVMBool 1);] |> Array.ofList, "x")
    let ld = LLVM.BuildLoad (theBuilder, trg, "tempload")
    let mul = LLVM.BuildMul (theBuilder, lhs, ld, "tempmul")
    LLVM.BuildRet (theBuilder, mul) |> ignore

    // Add Function

    let addFunction = LLVM.GetNamedFunction (theModule, "addFunction")
    let theBasicBlock = LLVM.AppendBasicBlock (addFunction, "entry")
    LLVM.PositionBuilderAtEnd (theBuilder, theBasicBlock)

    let lhs = LLVM.GetParam (addFunction, 1u)

    let allocd = LLVM.BuildAlloca (theBuilder, addContextStruct, "tempstruct")
    // let trg = LLVM.BuildGEP (theBuilder, allocd, [LLVM.ConstInt (LLVM.Int32Type (), 0UL, LLVMBool 0)] |> Array.ofList, "x")
    // let cst = LLVM.BuildBitCast (theBuilder, trg, (Base.Ptr Base.Character).ToLLVM (), "tempcast")
    let rhs = GenerateFunctionCall "addFunction.Multiply" (Array.ofList [LLVM.GetParam (addFunction, 0u); allocd])
    let add = LLVM.BuildAdd (theBuilder, lhs, rhs, "tempadd")
    LLVM.BuildRet (theBuilder, add) |> ignore

    // Main Function

    let theMain = LLVM.GetNamedFunction (theModule, "main")
    let theBasicBlock = LLVM.GetEntryBasicBlock theMain
    let theRet = LLVM.GetFirstInstruction theBasicBlock
    LLVM.PositionBuilderBefore (theBuilder, theRet)
    let theParams = [0; 5] |> List.map (fun x -> LLVM.ConstInt (LLVM.Int32Type(), uint64(x), LLVMBool 1)) |> Array.ofList
    CodeGenerator.GenerateFunctionCall "addFunction" theParams |> ignore

    if LLVM.VerifyModule (theModule, LLVMVerifierFailureAction.LLVMPrintMessageAction, ref null) <> LLVMBool 0 then
      printfn "Erroneuous module"
    LLVM.DumpModule theModule

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

    // let theFloatVars = LLVM.AddGlobal (theModule, LLVM.ArrayType (LLVM.X86FP80Type (), 64u), "floatVars")
    // LLVM.SetLinkage (theFloatVars, LLVMLinkage.LLVMLinkerPrivateLinkage)
    // LLVM.SetInitializer (theFloatVars, LLVM.ConstArray (LLVM.X86FP80Type (), LLVM.ConstReal (LLVM.X86FP80Type (), 0.0) |> List.replicate 64 |> Array.ofList))
    // LLVM.SetAlignment (theFloatVars, 16u)

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
    // let gep = LLVM.BuildGEP (theBuilder, theFloatVars, [LLVM.ConstInt (LLVM.Int32Type (), 1UL, theFalseBool); LLVM.ConstInt (LLVM.Int32Type (), 1UL, theFalseBool)] |> Array.ofList, "x")
    // let x = LLVM.BuildLoad (theBuilder, gep, "x")

    // let nil = LLVM.ConstNull (LLVM.GetElementType (gep.TypeOf ()))      // This is how nil gets the correct type

    // LLVM.BuildStore (theBuilder, nil, gep) |> ignore


    // let x = LLVM.BuildRet (theBuilder, LLVM.ConstInt (LLVM.Int32Type (), 0UL, theFalseBool))

    // let functionType = LLVM.FunctionType (LLVM.VoidType (), [LLVM.Int32Type ()] |> Array.ofList, false)
    // let testFunction = LLVM.AddFunction (theModule, "testFunction", functionType)
    
    // // check that the module is ok and emit code to console
    // if LLVM.VerifyModule (theModule, LLVMVerifierFailureAction.LLVMPrintMessageAction, ref null) <> theFalseBool then
    //   printfn "Erroneuous module"
    // LLVM.DumpModule theModule

    // LLVM.PrintModuleToFile (theModule, "test.txt", ref null) |> ignore

    0