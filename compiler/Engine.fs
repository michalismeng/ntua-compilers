namespace Compiler

module Engine =

  let rec private parseNamedBody symTable body name extraParams retType =
    let declarations, statements = body
    let _, symTable = SymbolTable.OpenScope symTable name retType
    let scope = List.head symTable

    printfn "Scope.%i: %s" scope.NestingLevel scope.Name

    let result = List.fold (fun acc b -> Semantic.AnalyzeDeclaration b :: acc) [] declarations

    printfn "Declaration analysis: %A" <| List.forall id result
    if not (List.forall id result) then
      printfn "Terminating compilation"
      exit 1

    let processParams = List.map (fun (n, t, _) -> Base.Variable (n, t)) extraParams
    let symTable = List.fold parseDeclaration symTable processParams
    let symTable = List.fold parseDeclaration symTable declarations


    let result = List.fold (fun acc b -> Semantic.AnalyzeStatement symTable b :: acc) [] statements

    printfn "Statement analysis: %A\n" <| List.forall id result
    symTable

  and private parseDeclaration symTable decl =
    let symbol = SymbolTable.Symbol.FromDeclaration decl
    let symTable = SymbolTable.AddDeclarationToTable symTable symbol
    match decl with
    | Base.Process ((name, paramList, ret), body) -> 
        let symTable = parseNamedBody symTable body name paramList ret
        snd <| SymbolTable.CloseScope symTable
    | _                                           -> symTable

  
  let private generateNamedBody symTable theModule theBuilder body name extraParams retType =
    let declarations, statements = body
    let _, symTable = SymbolTable.OpenScope symTable name retType

    // TODO: Continue here

    let theModule, theBuilder = List.fold (fun (tmod, tbuil) s -> CodeGenerator.GenerateStatement symTable tmod tbuil s) (theModule, theBuilder) statements
    
    theModule, theBuilder

  let Generate program =
    let name, body = program
    printfn "Generating code for '%s'" name

    let theModule = LLVMSharp.LLVM.ModuleCreateWithName "PCL Compiler"
    let theBuilder = LLVMSharp.LLVM.CreateBuilder ()

    // TODO: Finish LLVM preparation
    let theModule, theBuilder = CodeGenerator.GenerateMain theModule theBuilder

    LLVMSharp.LLVM.BuildRet (theBuilder, LLVMSharp.LLVM.ConstInt (LLVMSharp.LLVM.Int32Type (), 0UL, LLVMSharp.LLVMBool 0)) |> ignore

    let symTable = SymbolTable.CreateSymbolTable ()
    let theModule' = generateNamedBody symTable theModule theBuilder body name [] Base.Unit
    theModule'


  let Analyze program = 
    let name, body = program
    printfn "Performing semantic analysis on '%s'" name

    // initialize symbol table and open the global scope that corresponds to the program
    //TODO: Add global functions here
    let symTable = SymbolTable.CreateSymbolTable()
    parseNamedBody symTable body name [] Base.Unit |> ignore

    let analysisResult = Compiler.Helpers.Error.Parser.errorList.Count > 0

    printfn "Semantic analysis on '%s' %s" name (if analysisResult then "failed" else "succeeded")
    analysisResult

