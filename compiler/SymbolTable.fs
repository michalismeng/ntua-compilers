namespace Compiler

open Compiler.Helpers.Error

module SymbolTable =

  // Current Scope
  let private (~%) symTable = List.head symTable

  // All other Scopes
  let private (~~) symTable = List.tail symTable

  // Transforms process name to forward declaration name
  let private (~+) procName = sprintf "+%s" procName

  type Symbol =
  | Variable of string * Base.Type
  | Label of string
  | Process  of Base.ProcessHeader
  | Forward  of Base.ProcessHeader
  with
    member this.Size =
      match this with
      | Variable (s, t) -> t.Size
      | _               -> 0

    member this.Name =
      match this with
      | Variable (s, _)   -> s
      | Label s           -> s
      | Process (s, _, _) -> s
      | Forward (s, _, _) -> +s

    static member FromDeclaration decl =
      match decl with
      | Base.Variable (s, t)  -> Variable (s, t)
      | Base.Label s          -> Label s
      | Base.Process (hdr, _) -> Process hdr
      | Base.Forward hdr      -> Forward hdr
      | Base.DeclError _      -> raise <| InternalException "Cannot process declaration error in symbol table"

  type ScopeEntry = { Symbol: Symbol; Position: int }
  type Scope = { Name: string; Symbols: Map<string, ScopeEntry>; NestingLevel: int; ReturnType: Base.Type; UsedLabels: Set<string> }
  type SymbolTable = Scope list

  let private assertUniqueName scope name =
    if scope.Symbols.ContainsKey name then Symbolic.RaiseSymbolicError (sprintf "Identifier %s already exists" name) None

  let private assertFwdImpliesProc scope proc = 
    let procName, _, _ = proc
    if scope.Symbols.ContainsKey +procName then
      let fwdProc = scope.Symbols.[+procName].Symbol
      match fwdProc with
      | Forward fhdr -> if proc <> fhdr then Symbolic.RaiseSymbolicError (sprintf "Function definition of '%s' doesn't match previous declaration" procName) None
      | _            -> raise <| InternalException "Cannot compare function against non-callable declaration"

  let private (^!@) name scope = assertUniqueName scope name
  let private (^=>) proc scope = assertFwdImpliesProc scope proc

  let private getCurrentMaxPosition scope = 
    match scope.Symbols.IsEmpty with
    | true -> 0
    | false -> scope.Symbols |> Seq.maxBy (fun kv -> kv.Value.Position) 
                             |> fun kv -> kv.Value.Position + 1 // kv.Value.Symbol.Size -- LLVM auto-fixes size, we only need to provide  the index

  let OpenScope symTable name returnType =
    let curScope = %symTable
    let scope = { Name = name; Symbols = Map.empty; NestingLevel = curScope.NestingLevel + 1; ReturnType = returnType; UsedLabels = Set.empty }
    (scope, scope :: symTable)

  let CloseScope symTable =
    if (%symTable).Name = "Guard" then raise <| InternalException "Cannot close guard scope"
    (%symTable, ~~symTable)

  // Add the given symbol to the current scope
  let AddSymbolToScope scope symbol = 
    let maxPosition = getCurrentMaxPosition scope         
    let scopeEntry = { Symbol = symbol; Position = -1 }

    // Check that it is possible to add symbol to the current scope
    let scopeEntry = 
      match symbol with
      | Variable (name, _)          -> name ^!@ scope ; +name ^!@ scope               ; { scopeEntry with Position = maxPosition }
      | Label name                  -> name ^!@ scope ; +name ^!@ scope               ; scopeEntry
      | Process (name, paras, ret)  -> name ^!@ scope ; (name, paras, ret) ^=> scope  ; scopeEntry
      | Forward (name, _, _)        -> name ^!@ scope ; +name ^!@ scope               ; scopeEntry
      
    { scope with Symbols = scope.Symbols.Add(symbol.Name, scopeEntry) }

  let AddDeclarationToTable symTable decl =
    let newCurrentScope = AddSymbolToScope (%symTable) decl
    newCurrentScope :: ~~symTable

  let UseLabelInCurrentScope symTable name =
    let currentScope = %symTable
    let newCurrentScope = { currentScope with UsedLabels = currentScope.UsedLabels.Add(name) }
    newCurrentScope :: ~~symTable

  let LookupInCurrentScope symTable name =
    let scope = %symTable
    if scope.Symbols.ContainsKey name then
      Some (scope, scope.Symbols.[name].Symbol)
    elif scope.Symbols.ContainsKey +name then
      Some (scope, scope.Symbols.[+name].Symbol)
    else
      None

  let LookupInCurrentScopeSafe symTable name =
    let result = LookupInCurrentScope symTable name
    match result with
    | Some ss -> ss
    | None -> Symbolic.RaiseSymbolicError (sprintf "Symbol %s could not be found in scope %s" name (%symTable).Name) None

  let LookupScopeEntry symTable name =
    // we do two searches. One for the name as is reasonable. The second tries to find a forward declaration using the '+' convention.
    let scope = List.tryFind (fun s -> s.Symbols.ContainsKey name) symTable
    match scope with
    | Some s -> Some (s, s.Symbols.[name])
    | None   -> let scope = List.tryFind (fun s -> s.Symbols.ContainsKey +name) symTable
                match scope with
                | Some s -> Some (s, s.Symbols.[+name])
                | None   -> None

  let LookupScopeEntrySafe symTable name =
    let result = LookupScopeEntry symTable name
    match result with
    | Some (s, se) -> (s, se)
    | None -> Symbolic.RaiseSymbolicError (sprintf "Symbol %s could not be found" name) None

  let Lookup symTable name =
    let result = LookupScopeEntry symTable name
    match result with
    | Some (s, se) -> Some (s, se.Symbol)
    | None -> None

  let LookupSafe symTable name =
    let result = Lookup symTable name
    match result with
    | Some ss -> ss
    | None -> Symbolic.RaiseSymbolicError (sprintf "Symbol %s could not be found" name) None

  let CreateSymbolTable () =
    [{ Name = "Guard"; Symbols = Map.empty; NestingLevel = -1; ReturnType = Base.Unit; UsedLabels = Set.empty }]
    