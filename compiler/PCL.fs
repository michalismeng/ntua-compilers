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

    let addStruct = GenerateStructType ((Base.Ptr Base.Character).ToLLVM ()) [Base.Integer; Base.Integer; Base.Integer]
    let multStruct = GenerateStructType (LLVM.PointerType (addStruct, 0u)) [Base.Integer; Base.Integer]

    GenerateFunction "addFunction" addStruct Base.Integer [] |> ignore
    GenerateFunction "addFunction.Multiply" multStruct Base.Integer [] |> ignore
    GenerateFunctionRogue "writeInteger" [Base.Integer] Base.Unit [LLVMLinkage.LLVMExternalLinkage] |> ignore

    // Multiply Function

    let multiplyFunction = LLVM.GetNamedFunction (theModule, "addFunction.Multiply")
    GenerateBasicBlock multiplyFunction "entry" |> ignore

    let p = multiplyFunction.GetFirstParam ()

    let myD = GenerateStructLoad p 1

    let alink = GenerateStructLoad p 0
    let parentA = GenerateStructLoad alink 1
    let parentC = GenerateStructLoad alink 3

    let mul = generateBinop Base.Mult parentA myD false
    let add = generateBinop Base.Add mul parentC false

    LLVM.BuildRet (theBuilder, add) |> ignore

    // Add Function

    let addFunction = LLVM.GetNamedFunction (theModule, "addFunction")
    GenerateBasicBlock addFunction "entry" |> ignore

    let p = addFunction.GetFirstParam ()

    // c = 10
    GenerateStructStore p 3 (LLVM.ConstInt(LLVM.Int32Type (), 10UL, LLVMBool 1)) |> ignore

    // Fill activation record to call Multiply 5
    let ar = GenerateLocal multStruct

    GenerateStructStore ar 0 p |> ignore
    GenerateStructStore ar 1 (LLVM.ConstInt(LLVM.Int32Type (), 5UL, LLVMBool 1)) |> ignore

    let call = GenerateFunctionCall "addFunction.Multiply" [|ar|]
    let loadB = GenerateStructLoad p 2
    let add = LLVM.BuildAdd (theBuilder, loadB, call, "tempadd")

    LLVM.BuildRet (theBuilder, add) |> ignore

    // Main Function

    let theMain = LLVM.GetNamedFunction (theModule, "main")
    let theBasicBlock = LLVM.GetEntryBasicBlock theMain
    let theRet = LLVM.GetFirstInstruction theBasicBlock
    LLVM.PositionBuilderBefore (theBuilder, theRet)

    // Fill activation record to call AddFunction 3 5
    let ar = GenerateLocal addStruct
    GenerateStructStore ar 0 (LLVM.ConstNull((Base.Ptr Base.Character).ToLLVM ())) |> ignore
    GenerateStructStore ar 1 (LLVM.ConstInt(LLVM.Int32Type (), 3UL, LLVMBool 1)) |> ignore
    GenerateStructStore ar 2 (LLVM.ConstInt(LLVM.Int32Type (), 5UL, LLVMBool 1)) |> ignore

    let res = GenerateFunctionCall "addFunction" [|ar|]
    GenerateFunctionCall "writeInteger" [|res|] |> ignore

    // let theNull() = LLVMValueRef (nativeint 0)

    // let nil = LLVM.ConstNull (LLVM.GetElementType (gep.TypeOf ()))      // This is how nil gets the correct type

    if LLVM.VerifyModule (theModule, LLVMVerifierFailureAction.LLVMPrintMessageAction, ref null) <> LLVMBool 0 then
      printfn "Erroneuous module"
    LLVM.DumpModule theModule

    // LLVM.PrintModuleToFile (theModule, "test.txt", ref null) |> ignore

    0