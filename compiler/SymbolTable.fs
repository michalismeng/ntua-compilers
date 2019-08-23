namespace Compiler

module SymbolTable =

  type ScopeEntry = { Symbol: PCL.Declaration; Position: int }
  type Scope = { Name: string; Symbols: Map<string, ScopeEntry>; NestingLevel: int }
  type SymbolTable = Scope list

  // Current Scope
  let private (~%) symTable = List.head symTable

  let private (~~) symTable = List.tail symTable

  let OpenScope symTable name =
    let curScope = %symTable
    let scope = { Name = name; Symbols = Map.empty; NestingLevel = curScope.NestingLevel + 1 }
    (scope, scope :: symTable)

  let CloseScope symTable =
    if List.isEmpty symTable || (%symTable).Name = "Guard" then raise Helpers.Error.Semantic.SemanticException
    (%symTable, ~~symTable)

  // Add the given declaration to the current scope
  let AddDeclarationToScope scope decl = 
    let id = "ID"
    { scope with Symbols = scope.Symbols.Add(id, decl) } 

  let AddDeclarationToTable symTable decl =
    let id = "ID"
    let newCurrentScope = AddDeclarationToScope (%symTable) decl
    newCurrentScope :: ~~symTable

  let CreateSymbolTable () =
    [{ Name = "Guard"; Symbols = Map.empty; NestingLevel = -1; }]
    