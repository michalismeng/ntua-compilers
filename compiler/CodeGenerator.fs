namespace Compiler

open Compiler.Base
open LLVMSharp

module rec CodeGenerator =

  let private theTrue  = LLVMBool 1
  let private theFalse = LLVMBool 0

  let private generateBinop theBuilder op lhs rhs isFloat =
    match op with
    | Add           -> if isFloat then LLVM.BuildFAdd (theBuilder, lhs, rhs, "tfadd") else LLVM.BuildAdd (theBuilder, lhs, rhs, "tadd") 
    | Sub           -> if isFloat then LLVM.BuildFSub (theBuilder, lhs, rhs, "tfsub") else LLVM.BuildSub (theBuilder, lhs, rhs, "tsub")
    | Mult          -> if isFloat then LLVM.BuildFMul (theBuilder, lhs, rhs, "tfmult") else LLVM.BuildMul (theBuilder, lhs, rhs, "tmult")
    | Div           -> if isFloat then LLVM.BuildFDiv (theBuilder, lhs, rhs, "tfdiv") else raise <| Helpers.Error.InternalException "Operator Div requires float"
    | Divi          -> if not(isFloat) then LLVM.BuildSDiv (theBuilder, lhs, rhs, "tdivi") else raise <| Helpers.Error.InternalException "Operatir Divi requires integer"
    | Modi          -> if not(isFloat) then LLVM.BuildSRem (theBuilder, lhs, rhs, "tmodi") else raise <| Helpers.Error.InternalException "Operatir Modi requires integer"
    | Equals        -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOEQ, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntEQ, lhs, rhs, "teq")
    | NotEquals     -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealONE, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntNE, lhs, rhs, "teq")
    | Less          -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOLT, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSLE, lhs, rhs, "teq")
    | LessEquals    -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOLE, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSLT, lhs, rhs, "teq")
    | Greater       -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOGT, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSGT, lhs, rhs, "teq")
    | GreaterEquals -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOGE, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSGE, lhs, rhs, "teq")
    | Or            -> LLVM.BuildOr  (theBuilder, lhs, rhs, "tor")
    | And           -> LLVM.BuildAnd (theBuilder, lhs, rhs, "tand")

  let private generateLValue symTable theModule theBuilder lval =
    match lval with
    // | StringConst s   -> Array (Character, s.Length)
    | LParens l       -> generateLValue symTable theModule theBuilder l
    // | Identifier s    -> getIdentifierType symTable s
    // | Result          -> let scope = (List.head symTable) ; 
    //                      if scope.ReturnType = Unit then Semantic.RaiseSemanticError "Keyword 'result' cannot be used in non-function environment" None
    //                                                 else scope.ReturnType
    // | Brackets (l,e)  -> match getExpressionType symTable e with
    //                      | Integer  -> match getLValueType symTable l with
    //                                    | Array (t, _) | IArray t -> t
    //                                    | _            -> Semantic.RaiseSemanticError "Cannot index a non-array object" None
    //                      | _        -> Semantic.RaiseSemanticError "Array index must have integer type" None
    // | Dereference e   -> match getExpressionType symTable e with
    //                      | Ptr x   -> x
    //                      | NilType -> Semantic.RaiseSemanticError "Cannot dereference the Nil pointer" None
    //                      | _       -> Semantic.RaiseSemanticError "Cannot dereference a non-ptr value" None

  let private generateRValue theModule theBuilder rval =
      match rval with
      | IntConst i            -> LLVM.ConstInt (LLVM.Int32Type (), uint64(i), theTrue)
      | RealConst r           -> LLVM.ConstInt (LLVM.Int32Type (), uint64(1), theTrue)
      | CharConst c           -> LLVM.ConstInt (LLVM.Int32Type (), uint64(2), theTrue)
      | BoolConst b           -> LLVM.ConstInt (LLVM.Int32Type (), uint64(3), theTrue)
      | Nil                   -> LLVM.ConstInt (LLVM.Int32Type (), uint64(4), theTrue)
      | RParens r             -> generateRValue theModule theBuilder r
      | AddressOf e           -> LLVM.ConstInt (LLVM.Int32Type (), uint64(5), theTrue)
      | Call (n, p)           -> LLVM.ConstInt (LLVM.Int32Type (), uint64(6), theTrue)
      | Binop (e1, op, e2)    -> LLVM.ConstInt (LLVM.Int32Type (), uint64(7), theTrue)
      | Unop (op, e)          -> LLVM.ConstInt (LLVM.Int32Type (), uint64(8), theTrue)

  let GenerateExpression symTable theModule theBuilder expression =
    match expression with
    | LExpression l -> generateLValue symTable theModule theBuilder l
    | RExpression r -> generateRValue theModule theBuilder r

  let GenerateStatement symTable (theModule: LLVMModuleRef) (theBuilder: LLVMBuilderRef) (statement: Statement) =
    match statement with
    | Empty               -> (theModule, theBuilder)
    | Error _             -> (theModule, theBuilder)
    | Assign (lval, expr, _) -> let lhs = GenerateExpression symTable theModule theBuilder (LExpression lval)
                                let rhs = GenerateExpression symTable theModule theBuilder expr
                                LLVM.BuildStore (theBuilder, lhs, rhs) |> ignore
                                (theModule, theBuilder)
    | Block stmts         -> List.fold (fun (tmod, tbuil) s -> GenerateStatement symTable tmod tbuil s) (theModule, theBuilder) stmts


  let GenerateMain theModule theBuilder =
    let theMain = LLVM.AddFunction (theModule, "main", LLVM.FunctionType (LLVM.Int32Type (), Array.ofList [], false))
    let theBasicBlock = LLVM.AppendBasicBlock (theMain, "entry")
    LLVM.PositionBuilderAtEnd (theBuilder, theBasicBlock)
    (theModule, theBuilder)