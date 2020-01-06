namespace Compiler
open LLVMSharp

module Engine =

  let private onScopeEnter symTable (curScope: SymbolTable.Scope) =
    printfn "Scope.%i: %s" curScope.NestingLevel curScope.Name

  let private onStatementsEnter () =
    // assert all forward processes have definition 
    ()

  let private onStatementsExit symTable =
    // assert all used labels are defined
    let (curScope: SymbolTable.Scope) = List.head symTable
    let labelDif = Set.difference curScope.UsedLabels curScope.DefinedLabels
    if not(Set.isEmpty labelDif) then Helpers.Error.Semantic.RaiseSemanticError "Not all used labels are defined" None
    ()

  let rec private parseNamedBody symTable body name extraParams retType =
    let declarations, statements = body
    let scope, symTable = SymbolTable.OpenScope symTable name retType

    onScopeEnter symTable scope

    let result = List.fold (fun acc b -> Semantic.AnalyzeDeclaration b :: acc) [] declarations

    if not (List.forall id result) then
      printfn "Declaration analysis failed. Terminating compilation..."
      exit 1

    let processParams = List.map (fun (n, t, s) -> Base.Parameter (n, t, s)) extraParams
    let processDeclarations = processParams @ declarations
    let (*) (_, semAcc) (tbl2, sem) = (tbl2, semAcc @ sem)
    let symTable, innerInstrs = List.fold (fun (table, accInst) d -> (table, accInst) * parseDeclaration table d) (symTable, []) processDeclarations

    let (*) (res1, _, semAcc) (res2, tbl2, sem) = (res1 && res2, tbl2, semAcc @ sem)
    let result, symTable, semInstrs = List.fold (fun (res, tbl, sem) s -> (res, tbl, sem) * Semantic.AnalyzeStatement tbl s) (true, symTable, []) statements

    if not(result) then 
      printfn "Statement analysis failed. Terminating compilation..."
      exit 1

    onStatementsExit symTable

    let qName = SymbolTable.GetQualifiedName symTable
    let fInst = Base.SemDeclFunction (qName, retType, innerInstrs @ semInstrs) 
    (symTable, fInst)

  and private parseDeclaration symTable decl =
    let symbol = SymbolTable.Symbol.FromDeclaration decl
    let symTable = SymbolTable.AddDeclarationToTable symTable symbol
    match decl with
    | Base.Process ((name, paramList, ret), body, _) -> 
        let symTable, semInstr = parseNamedBody symTable body name paramList ret
        (snd <| SymbolTable.CloseScope symTable, [semInstr])
    | _                                           -> (symTable, [])

  let private insertGlobalFunctions symTable =
    let externalFunction = List.map SymbolTable.Forward Helpers.ExternalFunctions.ExternalIO

    //TODO: Add more external functions
    let symTable = List.fold SymbolTable.AddDeclarationToTable symTable externalFunction
    symTable

  let Analyze program = 
    let name, body = program
    printfn "Performing semantic analysis on '%s'" name

    // initialize symbol table and open the global scope that corresponds to the program
    let symTable = SymbolTable.CreateSymbolTable()
    // let _, symTable = SymbolTable.OpenScope symTable Helpers.Environment.ExternalsScopeName Base.Unit
    let symTable = insertGlobalFunctions symTable
    let result, semInstrs = parseNamedBody symTable body name [] Base.Unit

    let isFaulty = Compiler.Helpers.Error.Parser.errorList.Count > 0

    printfn "Semantic analysis on '%s' %s" name (if isFaulty then "failed" else "succeeded")
    not(isFaulty), semInstrs

  let NormalizeInstructionHierarchy topLevelFunction =
    let name, _, instructions = topLevelFunction
    let rec normalizeInstruction parentName name instructions hashhMap = 
      let isFunction inst = 
        match inst with
        | Base.SemDeclFunction (x, y, z) -> Some (x, y, z) 
        | _                              -> None
      let isStatement inst =
        match inst with
        | Base.SemDeclFunction _ -> None
        | _                      -> Some inst

      let functions = List.choose isFunction instructions
      let statements = List.choose isStatement instructions

      let hashMap = List.fold (fun acc (n, _, instrs) -> normalizeInstruction name n instrs acc) hashhMap functions
      Map.add name (Base.SemanticFunction (name, statements)) hashMap

    normalizeInstruction "" name instructions Map.empty

  let RunSemanticAnalysis program = 
    let semanticAnalysis, semanticInstruction = Analyze program

    if not(semanticAnalysis) then
      printfn "Semantic Analysis failed. Goodbye..."
      exit 1

    let arTypes, globalInstructions = CodeGenerator.GenerateARTypes program
    let labelNames = CodeGenerator.GenerateLabelledNames program

    let topLevelFunction = 
      match semanticInstruction with
      | Base.SemDeclFunction (n, t, il) -> (n, t, il)
      | _                      -> raise <| Helpers.Error.InternalException "Top Level Instruction must be a function"

    let normalizedHierarchy = NormalizeInstructionHierarchy topLevelFunction |> Map.toList |> List.map snd

    let externalFunctions = 
      Helpers.ExternalFunctions.ExternalIO
      |> List.map (fun (n, l, ret) -> (n, (List.map (fun (x,y,z) -> y,z) l), ret, [LLVMLinkage.LLVMExternalLinkage]))

    (normalizedHierarchy, globalInstructions, arTypes, labelNames, externalFunctions)

  let GenerateCode normalizedHierarchy globalInstructions arTypes labelNames externalFunctions =
    CodeGenerator.GenerateLLVMModule ()
    CodeGenerator.InitializeFPM ()

    // generate global symbols (global variables and function declarations, external and private)
    globalInstructions |> List.iter (fun gd -> gd ||> CodeGenerator.GenerateGlobalVariable)
    List.iter (fun (x, y, z, w) -> CodeGenerator.GenerateFunctionRogue x y z w |> ignore) externalFunctions
    List.iter (fun f -> CodeGenerator.GenerateFunctionPrototype arTypes f Base.Unit [] |> ignore) normalizedHierarchy

    // generate mymalloc and myfree function prototypes
    CodeGenerator.GenerateAllocatorPrototypes ()

    // Generate main function which calls the program's entry function
    if not(Helpers.Environment.CLI.IsLibrary) then
      CodeGenerator.GenerateMain (fst normalizedHierarchy.Head) |> ignore

    // Generate all program functions
    List.iter (fun func -> CodeGenerator.GenerateFunctionCode arTypes labelNames func |> ignore) normalizedHierarchy

    if Helpers.Environment.CLI.ShouldOptimize then
      LLVM.RunPassManager (CodeModule.theFPM, CodeModule.theModule) |> ignore
