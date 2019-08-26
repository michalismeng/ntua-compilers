namespace Compiler

module Engine =

  let rec private parseNamedBody symTable body name extraParams retType =
    let declarations, statements = body
    let _, symTable = SymbolTable.OpenScope symTable name retType
    let processParams = List.map (fun (n, t, _) -> Base.Variable (n, t)) extraParams
    let symTable = List.fold parseDeclaration symTable processParams
    let symTable = List.fold parseDeclaration symTable declarations
    let scope = List.head symTable

    printfn "Scope.%i: %s %A\n" scope.NestingLevel scope.Name scope.Symbols

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

  let Analyze program = 
    let name, body = program
    printfn "Performing semantic analysis on '%s'" name

    // initialize symbol table and open the global scope that corresponds to the program
    let symTable = SymbolTable.CreateSymbolTable()
    parseNamedBody symTable body name [] Base.Unit |> ignore
    ()

