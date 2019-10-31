namespace Compiler

open Compiler.Base
open Compiler.Helpers.Error

module rec Semantic =

  let private getIdentifierType symTable name =
    let scope, symbol = SymbolTable.LookupSafe symTable name
    match symbol with
    | SymbolTable.Variable (s, t) -> t
    | _                           -> Semantic.RaiseSemanticError "Callable given for l-value" None

  let private getProcessHeader symTable name =
    let scope, symbol = SymbolTable.LookupSafe symTable name
    let isExternal = scope.Name = Helpers.Environment.ExternalsScopeName
    let qualifiedName = (SymbolTable.GetQualifiedNameScoped symTable scope) + "." + name
    let nestingLevelDifference = (List.head symTable).NestingLevel - scope.NestingLevel - 1
    let name, paramList, ptype =
      match symbol with
      | SymbolTable.Forward phdr | SymbolTable.Process phdr -> phdr
      | _           -> Semantic.RaiseSemanticError (sprintf "Cannot call %s" name) None
    (paramList, ptype, qualifiedName, nestingLevelDifference, isExternal)


  let private checkLabelExists symTable name = 
    // We only check the current scope for a label symbol
    let scope, symbol = SymbolTable.LookupInCurrentScopeSafe symTable name
    match symbol with
    | SymbolTable.Label s -> true
    | _                   -> Semantic.RaiseSemanticError (sprintf "Cannot goto %s" name) None

  let private checkLabelNotDefined (symTable: SymbolTable.Scope list) name =
    let scope = List.head symTable
    not (Set.contains name scope.DefinedLabels)

  let private assertCallCompatibility symTable procName callParamList hdrParamList =
    let callParamTypes, callParamInstructions = callParamList 
                                                |> List.map (getExpressionType symTable) 
                                                |> List.unzip

    let callParamInstructionsCast = hdrParamList
                                    |> List.map (fun (_,y,_) -> y)                      // isolate formal parameter types (target types)
                                    |> List.zip3 callParamTypes callParamInstructions   // zip with real parameter types and instructions (sources)
                                    |> List.map (fun (cpt, cpi, hpl) -> handleRealCast hpl cpt cpi )
    let compatible (exprType, param) =
      let _, paramType, paramSpecies = param
      match paramSpecies with
      | ByValue -> paramType =~ exprType
      | ByRef   -> Ptr paramType =~ Ptr exprType

    if (not (List.length callParamList = List.length hdrParamList && List.forall compatible (List.zip callParamTypes hdrParamList))) then
      Semantic.RaiseSemanticError (sprintf "Incompatible call %s" procName) None

    callParamInstructionsCast

  let private checkIsNotBoolean symTable value =
    (fst <| getExpressionType symTable value) <> Boolean

  let private getBinopType lhs op rhs =
    match lhs, rhs with
    | (Integer, Integer)                    -> match op with
                                               | Add | Sub | Mult | Divi | Modi -> Integer
                                               | Div -> Real
                                               | Less | LessEquals | Greater | GreaterEquals | Equals | NotEquals -> Boolean
                                               | _ -> Semantic.RaiseSemanticError "Bad binary operands" None
    | (Integer, Real) | (Real, Integer) 
                      | (Real, Real)        -> match op with
                                               | Add | Sub | Mult | Div -> Real
                                               | Less | LessEquals | Greater | GreaterEquals | Equals | NotEquals -> Boolean
                                               | _ -> Semantic.RaiseSemanticError "Bad binary operands" None
    | (Boolean, Boolean)                    -> match op with
                                               | And | Or | Equals | NotEquals -> Boolean
                                               | _        -> Semantic.RaiseSemanticError "Bad binary operands" None
    | (t1, t2)                              -> match t1, t2 with
                                               | Array _, _ -> Semantic.RaiseSemanticError "Bad binary operands" None
                                               | IArray _, _ -> Semantic.RaiseSemanticError "Bad binary operands" None
                                               | Ptr x, Ptr y when x =~ y -> match op with 
                                                                             | Equals | NotEquals -> Boolean
                                                                             | _                  -> Semantic.RaiseSemanticError "Bad binary operands" None
                                               | _ -> Semantic.RaiseSemanticError "Bad binary operands" None
  
  let private getBinopKind lhs op rhs =
    match (lhs, rhs) with
    | (Integer, Integer)                  -> match op with
                                             | Div -> Real
                                             | _   -> Integer
    | (Integer, Real) | (Real, Integer) 
                      | (Real, Real)      -> Real
    | _                                   -> Integer

  let private getUnopType op t =
    match op, t with
    | (Not, Boolean)                                    -> Boolean
    | (Positive, x) | (Negative, x) when x.IsArithmetic -> t
    | _                                                 -> Semantic.RaiseSemanticError "Bad unary operand" None

  let private getLValueType symTable lval =
    let getIdentifierIndexPath symTable name =
      let curScope = List.head symTable
      let scope, entry = SymbolTable.LookupScopeEntrySafe symTable name
      let interARDifference = curScope.NestingLevel - scope.NestingLevel
      let intraARDifference = entry.Position
      (scope.NestingLevel, interARDifference, intraARDifference)

    match lval with
    | StringConst s   -> (Array (Character, s.Length), SemString s)
    | LParens l       -> getLValueType symTable l
    | Identifier s    -> let abs, u, i = getIdentifierIndexPath symTable s
                         let semInstruction = if abs = Helpers.Environment.GlobalScopeNesting then SemGlobalIdentifier s else SemIdentifier (u, i) 
                         (getIdentifierType symTable s, semInstruction)
    | Result          -> let scope = (List.head symTable) ; 
                         if scope.ReturnType = Unit then Semantic.RaiseSemanticError "Keyword 'result' cannot be used in non-function environment" None
                                                    else (scope.ReturnType, SemResult)
    | Brackets (l,e)  -> match getExpressionType symTable e with
                         | Integer, iInst -> match getLValueType symTable l with
                                             | (Array (t, _), lInst) | (IArray t, lInst) -> (t, SemDerefArray (lInst, iInst))
                                             | _            -> Semantic.RaiseSemanticError "Cannot index a non-array object" None
                         | _        -> Semantic.RaiseSemanticError "Array index must have integer type" None
    | Dereference e   -> match getExpressionType symTable e with
                         | Ptr x, i   -> (x, SemDeref i)
                         | NilType, _ -> Semantic.RaiseSemanticError "Cannot dereference the Nil pointer" None
                         | _          -> Semantic.RaiseSemanticError "Cannot dereference a non-ptr value" None  

  let private handleRealCast targetType sourceType sourceInst =
    match targetType, sourceType with
    | Real, Integer -> SemToFloat sourceInst
    | _             -> sourceInst

  let private getRValueType symTable rval =
    match rval with
    | IntConst n            -> (Integer, SemInt n)
    | RealConst r           -> (Real, SemReal r)
    | CharConst c           -> (Character, SemChar c)
    | BoolConst b           -> (Boolean, SemBool b)
    | Nil                   -> (NilType, SemNil)
    | RParens r             -> getRValueType symTable r
    | AddressOf e           -> match e with
                               | LExpression l -> let semantic, semInstr = getLValueType symTable l
                                                  (Ptr semantic, SemAddress semInstr)
                               | RExpression _ -> Semantic.RaiseSemanticError "Cannot get address of r-value object" None
    | Call (n, p)           -> let procHdr, procType, qualifiedName, nestingLevelDifference, isExternal = getProcessHeader symTable n
                               let instructions = assertCallCompatibility symTable n p procHdr
                               (procType, SemFunctionCall (isExternal, qualifiedName, nestingLevelDifference, instructions)) 
    | Binop (e1, op, e2)    -> let lhsType, lhsInst = getExpressionType symTable e1
                               let rhsType, rhsInst = getExpressionType symTable e2
                               let binopTypr = getBinopType lhsType op rhsType
                               let binopKind = getBinopKind lhsType op rhsType

                               let lhsInstCast = handleRealCast binopKind lhsType lhsInst 
                               let rhsInstCast = handleRealCast binopKind rhsType rhsInst 

                               (binopTypr, SemBinop (lhsInstCast, rhsInstCast, op, binopKind))
    | Unop (op, e)          -> let pType, pInst = getExpressionType symTable e
                               let unopType = getUnopType op pType
                               (unopType, SemUnop (pInst, op, unopType))

  let getExpressionType symTable expr =
    match expr with
    | LExpression l -> getLValueType symTable l
    | RExpression r -> getRValueType symTable r

  let private setStatementPosition statement = 
    match statement with
      | Goto (_, pos)
      | While (_, _, pos)
      | If (_, _, _, pos)
      | SCall (_, _, pos)
      | Assign (_, _, pos)
      | LabeledStatement (_, _, pos)  -> SetLastErrorPosition pos
      | _                             -> ()

  let AnalyzeStatement symTable statement =
    setStatementPosition statement
    let result = 
      match statement with
      | Empty                         -> (true, symTable, [])
      | Return                        -> (true, symTable, [SemReturn])
      | Error (x, pos)                -> printfn "<Erroneous Statement>\t-> false @ %d" pos.NextLine.Line ; (false, symTable, [])
      | Goto (target, pos)            -> let table = SymbolTable.UseLabelInCurrentScope symTable target
                                         (checkLabelExists symTable target, table, [SemGoto target])
      | While (e, stmt, pos)          -> let conditionType, conditionInstruction = getExpressionType symTable e
                                         if conditionType <> Boolean then Semantic.RaiseSemanticError "'While' construct condition must be boolean" None
                                         else 
                                          let res, table, bodyInstructions = AnalyzeStatement symTable stmt
                                          (res, table, [SemWhile (conditionInstruction, bodyInstructions)])
      | If (e, istmt, estmt, pos)     -> let conditionType, conditionInstruction = getExpressionType symTable e
                                         if conditionType <> Boolean then Semantic.RaiseSemanticError "'If' construct condition must be boolean" None
                                         else 
                                           let (res1, table1, ifpart) = AnalyzeStatement symTable istmt 
                                           let (res2, table2, elsepart) = AnalyzeStatement table1 estmt
                                           (res1 && res2, table2, [SemIf (conditionInstruction, ifpart, elsepart)])
      | SCall (n, p, pos)             -> let procHdr, _, qualifiedName, nestingLevelDifference, isExternal = getProcessHeader symTable n
                                         let instructions = assertCallCompatibility symTable n p procHdr
                                         (true, symTable, [SemFunctionCall (isExternal, qualifiedName, nestingLevelDifference, instructions)])
      | Assign (lval, expr, pos)      -> let lvalType, lhsInst = getExpressionType symTable (LExpression lval)
                                         let exprType, rhsInst = getExpressionType symTable expr
                                         let assignmentPossible = lvalType =~ exprType
                                         let rhsInstCast = handleRealCast lvalType exprType rhsInst
                                         printfn "Assign <%A> := <%A>\t-> %b @ %d" lvalType exprType assignmentPossible pos.NextLine.Line
                                         (assignmentPossible, symTable, [SemAssign (lhsInst, rhsInstCast)])
      | LabeledStatement (l, s, pos)   -> //! Caution short-circuit happens here and AnalyzeStatement never executes
                                          let res = checkLabelExists symTable l && checkLabelNotDefined symTable l 
                                          let (res2, table, semInstructions) = AnalyzeStatement symTable s
                                          let table = SymbolTable.DefineLabelInCurrentScope table l
                                          if not (res && res2) then Semantic.RaiseSemanticError (sprintf "Label '%s' already defined" l) None
                                          (res && res2, table, [SemLblStmt (l, semInstructions)])
      | New _ | NewArray _ | Dispose _ | DisposeArray _ -> raise <| InternalException "Dynamic memory allocation semantics not implemented"
      | Block stmts                    -> let (*) (res1, _, semAcc) (res2, tbl2, sem) = (res1 && res2, tbl2, semAcc @ sem)
                                          List.fold (fun (res, tbl, sem) s -> (res, tbl, sem) * AnalyzeStatement tbl s) (true, symTable, []) stmts

    result

  // Declaration Analysis

  let AnalyzeType typ =
    match typ with
    | Array (t, sz)     -> sz > 0 && AnalyzeType t && t.IsComplete
    | IArray t | Ptr t  -> AnalyzeType t
    | Proc              -> raise <| InternalException "Cannot analyze the type of a process here"
    | _                 -> true

  let AnalyzeProcessParamType ptype =
    let _, typ, species = ptype
    let procQuirk =
      match typ, species with
      | Array _, ByValue | IArray _, ByValue -> false
      | _                                    -> true
    AnalyzeType typ && procQuirk

  let AnalyzeProcessReturnType retType =
    match retType with
    | Array _ | IArray _ -> false
    | _                  -> true

  let AnalyzeProcessHeader (hdr: ProcessHeader) =
    let _, paramList, retType = hdr
    let paramResult = paramList |> List.map AnalyzeProcessParamType 
                                |> List.forall id
    let retResult = AnalyzeProcessReturnType retType
    paramResult && retResult

  let AnalyzeDeclaration decl =
    match decl with
    | Variable (_, t)                 -> AnalyzeType t
    | Label _                         -> true
    | Process (hdr, _) | Forward hdr  -> AnalyzeProcessHeader hdr
    | DeclError (_, pos)              -> printfn "<Erroneous Declaration\t-> false @ %d" pos.NextLine.Line; false