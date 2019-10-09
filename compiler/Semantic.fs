namespace Compiler

open Compiler.Base
open Compiler.Helpers.Error

module rec Semantic =

  // Statement Analysis

  let private getIdentifierType symTable name =
    let scope, symbol = SymbolTable.LookupSafe symTable name
    match symbol with
    | SymbolTable.Variable (s, t) -> t
    | _                           -> Semantic.RaiseSemanticError "Callable given for l-value" None

  let private getProcessHeader symTable name =
    let scope, symbol = SymbolTable.LookupSafe symTable name
    let name, paramList, ptype =
      match symbol with
      | SymbolTable.Forward phdr | SymbolTable.Process phdr -> phdr
      | _           -> Semantic.RaiseSemanticError (sprintf "Cannot call %s" name) None
    (paramList, ptype)

  let private checkLabelExists symTable name = 
    // We only check the current scope for a label symbol
    let scope, symbol = SymbolTable.LookupInCurrentScopeSafe symTable name
    match symbol with
    | SymbolTable.Label s -> true
    | _                   -> Semantic.RaiseSemanticError (sprintf "Cannot goto %s" name) None

  let private checkLabelNotUsed (symTable: SymbolTable.Scope list) name =
    let scope = List.head symTable
    not (Set.contains name scope.UsedLabels)


  let private assertCallCompatibility symTable procName callParamList hdrParamList =
    let compatible (expr, param) =
      let exprType = getExpressionType symTable expr
      let _, paramType, paramSpecies = param
      match paramSpecies with
      | ByValue -> paramType =~ exprType
      | ByRef   -> Ptr paramType =~ Ptr exprType

    if (not (List.length callParamList = List.length hdrParamList && List.forall compatible (List.zip callParamList hdrParamList))) then
      Semantic.RaiseSemanticError (sprintf "Incompatible call %s" procName) None

  let private checkIsNotBoolean symTable value =
    getExpressionType symTable value <> Boolean

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
                                               | Array _, Array _ -> Semantic.RaiseSemanticError "Bad binary operands" None
                                               | IArray _, IArray _ -> Semantic.RaiseSemanticError "Bad binary operands" None
                                               | _ -> match op with 
                                                      | Equals | NotEquals -> Boolean
                                                      | _                  -> Semantic.RaiseSemanticError "Bad binary operands" None

  let private getUnopType op t =
    match op, t with
    | (Not, Boolean)                                    -> Boolean
    | (Positive, x) | (Negative, x) when x.IsArithmetic -> t
    | _                                                 -> Semantic.RaiseSemanticError "Bad unary operand" None

  let private getLValueType symTable lval =
    match lval with
    | StringConst s   -> Array (Character, s.Length)
    | LParens l       -> getLValueType symTable l
    | Identifier s    -> getIdentifierType symTable s
    | Result          -> let scope = (List.head symTable) ; 
                         if scope.ReturnType = Unit then Semantic.RaiseSemanticError "Keyword 'result' cannot be used in non-function environment" None
                                                    else scope.ReturnType
    | Brackets (l,e)  -> match getExpressionType symTable e with            // TODO: Perhaps number of brackets must equal array level - do not allow assignment to whole array
                         | Integer  -> match getLValueType symTable l with
                                       | Array (t, _) | IArray t -> t
                                       | _            -> Semantic.RaiseSemanticError "Cannot index a non-array object" None
                         | _        -> Semantic.RaiseSemanticError "Array index must have integer type" None
    | Dereference e   -> match getExpressionType symTable e with
                         | Ptr x   -> x
                         | NilType -> Semantic.RaiseSemanticError "Cannot dereference the Nil pointer" None
                         | _       -> Semantic.RaiseSemanticError "Cannot dereference a non-ptr value" None

  let private getRValueType symTable rval =
    match rval with
    | IntConst _            -> Integer
    | RealConst _           -> Real
    | CharConst _           -> Character
    | BoolConst _           -> Boolean
    | Nil                   -> NilType
    | RParens r             -> getRValueType symTable r
    | AddressOf e           -> match e with
                               | LExpression l -> Ptr <| getLValueType symTable l
                               | RExpression _ -> Semantic.RaiseSemanticError "Cannot get address of r-value object" None
    | Call (n, p)           -> let procHdr, procType = getProcessHeader symTable n
                               assertCallCompatibility symTable n p procHdr
                               procType
    | Binop (e1, op, e2)    -> getBinopType (getExpressionType symTable e1) op (getExpressionType symTable e2)
    | Unop (op, e)          -> getUnopType op <| getExpressionType symTable e

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
      | Empty                         -> (true, symTable)
      | Return                        -> (true, symTable)
      | Error (x, pos)                -> printfn "<Erroneous Statement>\t-> false @ %d" pos.NextLine.Line ; (false, symTable)
      | Goto (target, pos)            -> (checkLabelExists symTable target, symTable) // TODO: Assert that label is actually defined in code block 
      | While (e, stmt, pos)          -> if checkIsNotBoolean symTable e then Semantic.RaiseSemanticError "'While' construct condidition must be boolean" None
                                         else AnalyzeStatement symTable stmt
      | If (e, istmt, estmt, pos)     -> if checkIsNotBoolean symTable e then Semantic.RaiseSemanticError "'If' construct condidition must be boolean" None
                                         else 
                                           let (res1, table1) = AnalyzeStatement symTable istmt 
                                           let (res2, table2) = AnalyzeStatement table1   estmt
                                           (res1 && res2, table2)
      | SCall (n, p, pos)             -> let procHdr, _ = getProcessHeader symTable n
                                         assertCallCompatibility symTable n p procHdr
                                         (true, symTable)
      | Assign (lval, expr, pos)      -> let lvalType = getExpressionType symTable <| LExpression lval
                                         let exprType = getExpressionType symTable expr
                                         let assignmentPossible = lvalType =~ exprType
                                         printfn "Assign <%A> := <%A>\t-> %b @ %d" lvalType exprType assignmentPossible pos.NextLine.Line
                                         (assignmentPossible, symTable)
      | LabeledStatement (l, s, pos)   -> //! Caution short-circuit happens here and AnalyzeStatement never executes
                                          let res = checkLabelExists symTable l && checkLabelNotUsed symTable l 
                                          let (res2, table) = AnalyzeStatement symTable s
                                          let table = SymbolTable.UseLabelInCurrentScope table l
                                          if not (res && res2) then Semantic.RaiseSemanticError (sprintf "Label '%s' already used" l) None
                                          (res && res2, table)
      | New _ | NewArray _ | Dispose _ | DisposeArray _ -> raise <| InternalException "Dynamic memory allocation semantics not implemented"
      | Block stmts                    -> let (*) (res1, _) (res2, tbl2) = (res1 && res2, tbl2)
                                          List.fold (fun (res, tbl) s -> (res, tbl) * AnalyzeStatement tbl s) (true, symTable) stmts

    result

  // Semantic Instruction Generation 

  let GenerateSemanticRVal symTable rval =
    match rval with
    | IntConst n            -> SemInt n
    | RealConst r           -> SemReal r
    | CharConst c           -> SemChar c
    | BoolConst b           -> SemBool b
    | Nil                   -> SemNil
    | RParens r             -> GenerateSemanticRVal symTable r
    | AddressOf e           -> let p = GenerateSemanticExpression symTable e 
                               SemAddress p
    // | Call (n, p)           -> let procHdr, procType = getProcessHeader symTable n
    //                            assertCallCompatibility symTable n p procHdr
    //                            procType
    | Binop (e1, op, e2)    -> let lhs = GenerateSemanticExpression symTable e1
                               let rhs = GenerateSemanticExpression symTable e2
                               SemBinop (lhs, rhs, op, getBinopType (getExpressionType symTable e1) op (getExpressionType symTable e2))
    | Unop (op, e)          -> let p =  GenerateSemanticExpression symTable e 
                               SemUnop (p, op, (getExpressionType symTable e))
    | _ -> raise <| InternalException "askjdkjd"

  

  let GenerateSemanticLVal symTable lval =
    let getIdentifierIndexPath symTable name =
      let curScope = List.head symTable
      let scope, entry = SymbolTable.LookupScopeEntrySafe symTable name
      let interARDifference = curScope.NestingLevel - scope.NestingLevel
      let intraARDifference = entry.Position
      (scope.NestingLevel, interARDifference, intraARDifference)

    match lval with
    | StringConst s   -> SemString s
    | LParens l       -> GenerateSemanticLVal symTable l
    | Identifier s    -> let abs, u, i = getIdentifierIndexPath symTable s
                         if abs = 1 then SemGlobalIdentifier s else SemIdentifier (u, i)      //TODO: absolute scope level = 1 => global variable
    | _ -> raise <| InternalException "kdflgdfkg"

  let GenerateSemanticExpression symTable expr =
    match expr with
    | LExpression l -> GenerateSemanticLVal symTable l
    | RExpression r -> GenerateSemanticRVal symTable r

  let GenerateSemanticStatement symTable statement =
    match statement with
    | Assign (lval, expr, pos)      -> let lhs = GenerateSemanticLVal symTable lval
                                       let rhs = GenerateSemanticExpression symTable expr
                                       SemAssign (lhs, rhs) 
    // | Goto (target, pos)            -> (checkLabelExists symTable target, symTable)
    | _                             -> raise <| InternalException "skjfksfdk"

  // let GenerateSemanticProgram symTable program = 

  

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