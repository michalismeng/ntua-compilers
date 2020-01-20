namespace Compiler

open FSharp.Text.Lexing
open LLVMSharp
open System.Runtime.InteropServices

module PCL =

  let private verifyAndDump _module =
    if LLVM.VerifyModule (_module, LLVMVerifierFailureAction.LLVMPrintMessageAction, ref null) <> LLVMBool 0 then
      printfn "Erroneuous module\n"
    else
      if Helpers.Environment.CLI.InterimCodeToStdout then
        LLVM.DumpModule _module
      else
        let path = System.IO.Path.ChangeExtension (Helpers.Environment.CLI.FileName, null)
        LLVM.PrintModuleToFile (_module, path + ".imm", ref null) |> ignore

  let private generateX86Assembly () =
    LLVM.InitializeX86TargetInfo()
    LLVM.InitializeX86Target()
    LLVM.InitializeX86TargetMC()

    LLVM.InitializeX86AsmPrinter()

    let defaultTriple = Marshal.PtrToStringAnsi <| LLVM.GetDefaultTargetTriple ()
    let mutable target = Unchecked.defaultof<LLVMTargetRef>
    let mutable err = Unchecked.defaultof<string>

    let mutable buffer = Unchecked.defaultof<LLVMMemoryBufferRef>

    if LLVM.GetTargetFromTriple (defaultTriple, &target, &err) <> LLVMBool 0 then
      raise <| Helpers.Error.InternalException (sprintf "Could not get target from triple %A" err)

    let targetMachine = LLVM.CreateTargetMachine (target, defaultTriple, "generic", "",
                          LLVMCodeGenOptLevel.LLVMCodeGenLevelAggressive, LLVMRelocMode.LLVMRelocStatic, LLVMCodeModel.LLVMCodeModelSmall)

    if LLVM.TargetMachineEmitToMemoryBuffer (targetMachine, CodeModule.theModule, LLVMCodeGenFileType.LLVMAssemblyFile, &err, &buffer) <> LLVMBool 0 then
      raise <| Helpers.Error.InternalException (sprintf "Could not emit assembly code") 

    Marshal.PtrToStringAuto (LLVM.GetBufferStart buffer)

  [<EntryPoint>]
  let main argv =

    if not(Helpers.Environment.CLI.parseCLIArguments argv) then
      exit 1

    (* Setup the input text *)
    let input = System.IO.File.ReadAllText Helpers.Environment.CLI.FileName

    (* Parse input program and perform semantic analysis *)
    let normalizedHierarchy, globalInstructions, arTypes, labelNames, externalFunctions =   
      try
        let parse input =
          let lexbuf = LexBuffer<_>.FromString input
          Parser.start Lexer.read lexbuf

        match parse input with
        | Some program -> Engine.RunSemanticAnalysis program
        | None -> printfn "errors:\n%A\n\nNo input given" Helpers.Error.Parser.errorList ; exit 1
      with
        | Helpers.Error.Lexer.LexerException e -> printfn "Lex Exception -> %s" <| Helpers.Error.StringifyError e             ; exit 1             
        | Helpers.Error.Parser.ParserException e -> printfn "Parse Exception -> %s" <| Helpers.Error.StringifyError e         ; exit 1
        | Helpers.Error.Semantic.SemanticException e -> printfn "Semantic Exception -> %s" <| Helpers.Error.StringifyError e  ; exit 1
        | Helpers.Error.Symbolic.SymbolicException e -> printfn "Symbolic Exception -> %s" <| Helpers.Error.StringifyError e  ; exit 1
        | Helpers.Error.InternalException s -> printfn "Fatal error -> %s" s ; exit 1
        | e -> printfn "%s" e.Message ; exit 1

    try
      Engine.GenerateCode normalizedHierarchy globalInstructions arTypes labelNames externalFunctions
      verifyAndDump CodeModule.theModule

      let assemblyString = generateX86Assembly ()

      if Helpers.Environment.CLI.FinalCodeToStdout then
        printfn "%s" assemblyString
      else
        let path = System.IO.Path.ChangeExtension (Helpers.Environment.CLI.FileName, null)
        System.IO.File.WriteAllText(path + ".asm", assemblyString)
    with
      | Helpers.Error.InternalException s -> printfn "Fatal error -> %s" s ; exit 1
      | e -> printfn "Unexpected error: %s" e.Message ; exit 1
    0