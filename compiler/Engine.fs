namespace Compiler

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

    let processParams = List.map (fun (n, t, _) -> Base.Variable (n, t)) extraParams
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
    | Base.Process ((name, paramList, ret), body) -> 
        let symTable, semInstr = parseNamedBody symTable body name paramList ret
        (snd <| SymbolTable.CloseScope symTable, [semInstr])
    | _                                           -> (symTable, [])

  let private insertGlobalFunctions symTable =
    let stringType = Base.IArray Base.Character

    let globalsIO = [
      SymbolTable.Forward ("writeInteger", [("n", Base.Integer, Base.ProcessParamSpecies.ByValue)], Base.Unit);
      SymbolTable.Forward ("writeBoolean", [("b", Base.Boolean, Base.ProcessParamSpecies.ByValue)], Base.Unit);
      SymbolTable.Forward ("writeChar", [("c", Base.Character, Base.ProcessParamSpecies.ByValue)], Base.Unit);
      SymbolTable.Forward ("writeReal", [("r", Base.Real, Base.ProcessParamSpecies.ByValue)], Base.Unit);
      SymbolTable.Forward ("writeString", [("s", stringType, Base.ProcessParamSpecies.ByRef)], Base.Unit);

      SymbolTable.Forward ("readInteger", [], Base.Integer);
      SymbolTable.Forward ("readBoolean", [], Base.Boolean);
      SymbolTable.Forward ("readChar",    [], Base.Character);
      SymbolTable.Forward ("readReal",    [], Base.Real);
      SymbolTable.Forward ("readString",  [("size", Base.Integer, Base.ProcessParamSpecies.ByValue); ("s", stringType, Base.ProcessParamSpecies.ByRef)], Base.Unit)
    ]

    let symTable = List.fold SymbolTable.AddDeclarationToTable symTable globalsIO
    //TODO: Add global functions here
    symTable

  let Analyze program = 
    let name, body = program
    printfn "Performing semantic analysis on '%s'" name

    // initialize symbol table and open the global scope that corresponds to the program
    let symTable = SymbolTable.CreateSymbolTable()
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
      Map.add name (parentName, statements) hashMap

    normalizeInstruction "" name instructions Map.empty

  let Generate activationRecords functions =
    let theMain = CodeGenerator.GenerateMain ()
    
    let theBasicBlock = LLVMSharp.LLVM.GetEntryBasicBlock theMain
    let theRet = LLVMSharp.LLVM.GetFirstInstruction theBasicBlock
    LLVMSharp.LLVM.PositionBuilderBefore (Module.theBuilder, theRet)

    // let curARType = CodeGenerator.GenerateStructType (LLVMSharp.LLVM.PointerType (LLVMSharp.LLVM.Int32Type(), 0u)) [Base.Integer; Base.Integer; Base.Integer; Base.Integer; Base.Real]
    // let curAR = CodeGenerator.GenerateLocal curARType

    let (n, (pn: string), insts) = List.head functions
    CodeGenerator.GenerateFunctionCode activationRecords n pn Base.Integer insts

    // List.iter (fun (n, pn, insts) -> (CodeGenerator.GenerateFunctionCode activationRecords n pn Base.Integer insts) |> ignore) functions
    (Module.theModule, Module.theBuilder)
