namespace Compiler

open FSharp.Text.Lexing
open CodeGenerator
open LLVMSharp

open System.Text.RegularExpressions
open System.Diagnostics
open System.Runtime.InteropServices

module PCL =

  let private verifyAndDump _module =
    if LLVM.VerifyModule (_module, LLVMVerifierFailureAction.LLVMPrintMessageAction, ref null) <> LLVMBool 0 then
      printfn "Erroneuous module\n"
      LLVM.PrintModuleToFile (_module, "test.txt", ref null) |> ignore
    else
      if Helpers.Environment.CLI.InterimCodeToStdout then
        LLVM.DumpModule _module
      else
        LLVM.PrintModuleToFile (_module, "test.txt", ref null) |> ignore

  let private generateX86Assembly () =
    LLVM.InitializeX86TargetInfo()
    LLVM.InitializeX86Target()
    LLVM.InitializeX86TargetMC()

    LLVM.InitializeX86AsmPrinter()

    let defaultTriple = Marshal.PtrToStringAnsi <| LLVM.GetDefaultTargetTriple ()
    let mutable target = Unchecked.defaultof<LLVMTargetRef>
    let mutable err = Unchecked.defaultof<string>

    let mutable buffer = Unchecked.defaultof<LLVMMemoryBufferRef>

    printfn "Default triple: %A" defaultTriple
    if LLVM.GetTargetFromTriple (defaultTriple, &target, &err) <> LLVMBool 0 then
      printfn "Could not get target from triple %A" err

    let targetMachine = LLVM.CreateTargetMachine (target, defaultTriple, "generic", "",
                          LLVMCodeGenOptLevel.LLVMCodeGenLevelAggressive, LLVMRelocMode.LLVMRelocStatic, LLVMCodeModel.LLVMCodeModelSmall)

    if LLVM.TargetMachineEmitToMemoryBuffer (targetMachine, CodeModule.theModule, LLVMCodeGenFileType.LLVMAssemblyFile, &err, &buffer) <> LLVMBool 0 then
      printfn "Could not emit assembly code"

    Marshal.PtrToStringAuto (LLVM.GetBufferStart buffer)

  [<EntryPoint>]
  let main argv =

    if not(Helpers.Environment.CLI.parseCLIArguments argv) then
      exit 1

    (* Setup the input text *)
    let input = System.IO.File.ReadAllText Helpers.Environment.CLI.FileName

    (* Parse and perform semantic analysis *)
    try
      let parse input =
        let lexbuf = LexBuffer<_>.FromString input
        Parser.start Lexer.read lexbuf

      match parse input with
      | Some program -> printfn "errors:\n%A" Helpers.Error.Parser.errorList

                        // let mallocARtype = LowLevel.GenerateStructType { { i16**, i16 }*, i16, i16, i16, i16, i16 }*
                        

                        let normalizedHierarchy, globalInstructions, arTypes, labelNames, externalFunctions = Engine.RunSemanticAnalysis program
                        Engine.GenerateCode normalizedHierarchy globalInstructions arTypes labelNames externalFunctions
                        verifyAndDump CodeModule.theModule
                       
      | None -> printfn "errors:\n%A\n\nNo input given" Helpers.Error.Parser.errorList
    with
      | Helpers.Error.Lexer.LexerException e -> printfn "Lex Exception -> %s" <| Helpers.Error.StringifyError e             ; exit 1             
      | Helpers.Error.Parser.ParserException e -> printfn "Parse Exception -> %s" <| Helpers.Error.StringifyError e         ; exit 1
      | Helpers.Error.Semantic.SemanticException e -> printfn "Semantic Exception -> %s" <| Helpers.Error.StringifyError e  ; exit 1
      | Helpers.Error.Symbolic.SymbolicException e -> printfn "Symbolic Exception -> %s" <| Helpers.Error.StringifyError e  ; exit 1
      | e -> printfn "%A" e ; exit 1

    let assemblyString = generateX86Assembly ()

    if Helpers.Environment.CLI.FinalCodeToStdout then
      printfn "%s" assemblyString
    else
      System.IO.File.WriteAllText("test.asm", assemblyString)
    0