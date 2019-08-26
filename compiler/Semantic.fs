namespace Compiler

open Compiler.Base
open Compiler.Helpers.Error 

module rec Semantic =

  exception ArrayIndexTypeException of string * Expression
  exception DereferenceTypeException of string * Expression
  exception InvalidAddressException of string * Expression
  exception InvalidBinaryOperandsException of string * Type * BinaryOperator * Type
  exception InvalidUnaryOperandsException of string * UnaryOperator * Type

  let private canAssign lhs rhs =
      match lhs, rhs with
      | Real, Integer                         -> true
      | Ptr (IArray t1), Ptr (Array (t2, _))  -> t1 =~ t2
      | Unit, _                               -> false
      | _                                     -> lhs =~ rhs
      // TODO: More cases

  let isArithmetic t =
      match t with
      | Integer | Real -> true
      | _              -> false

  let private getIdentifierType symTable name =
    let scope, symbol = SymbolTable.LookupSafe symTable name
    match symbol with
    | SymbolTable.Variable (s, t) -> t
    | _                           -> raise <| Semantic.SemanticException "Callable given for l-value"

  let private getProcessHeader symTable name =
    let scope, symbol = SymbolTable.LookupSafe symTable name
    let name, paramList, ptype =
      match symbol with
      | SymbolTable.Forward phdr | SymbolTable.Process phdr -> phdr
      | _           -> raise <| Semantic.SemanticException (sprintf "Cannot call %s" name)
    (paramList, ptype)

  let private assertCallCompatibility symTable procName callParamList hdrParamList =
    let compatible (expr, param) =
      let exprType = getExpressionType symTable expr
      let _, paramType, _ = param
      canAssign paramType exprType

    if (not (List.length callParamList = List.length hdrParamList && List.forall compatible (List.zip callParamList hdrParamList))) then
      raise <| Semantic.SemanticException (sprintf "Incompatible call %s" procName)

  let private getBinopType lhs op rhs =
    match lhs, rhs with
    | (Integer, Integer)                    -> match op with
                                               | Add | Sub | Mult | Divi | Modi -> Integer
                                               | Div -> Real
                                               | Less | LessEquals | Greater | GreaterEquals | Equals | NotEquals -> Boolean
                                               | _ -> raise <| InvalidBinaryOperandsException ("Bad binary operands", lhs, op, rhs)
    | (Integer, Real) | (Real, Integer) 
                      | (Real, Real)        -> match op with
                                               | Add | Sub | Mult | Div -> Real
                                               | Less | LessEquals | Greater | GreaterEquals | Equals | NotEquals -> Boolean
                                               | _ -> raise <| InvalidBinaryOperandsException ("Bad binary operands", lhs, op, rhs)
    | (Boolean, Boolean)                    -> match op with
                                               | And | Or | Equals | NotEquals -> Boolean
                                               | _        -> raise <| InvalidBinaryOperandsException ("Bad binary operands", lhs, op, rhs)
    | (t1, t2)                              -> match t1, t2 with
                                               | Array _, Array _ -> raise <| InvalidBinaryOperandsException ("Bad binary operands", lhs, op, rhs)
                                               | IArray _, IArray _ -> raise <| InvalidBinaryOperandsException ("Bad binary operands", lhs, op, rhs)
                                               | _ -> match op with 
                                                      | Equals | NotEquals -> Boolean
                                                      | _                  -> raise <| InvalidBinaryOperandsException ("Bad binary operands", lhs, op, rhs)

  let private getUnopType op t =
    match op, t with
    | (Not, Boolean)                                    -> Boolean
    | (Positive, x) | (Negative, x) when isArithmetic x -> t
    | _                                                 -> raise <| InvalidUnaryOperandsException ("Bad unary operand", op, t)

  let private getLValueType symTable lval =
    match lval with
    | StringConst s   -> Array (Character, s.Length)
    | LParens l       -> getLValueType symTable l
    | Identifier s    -> getIdentifierType symTable s
    | Result          -> (List.head symTable).ReturnType          //! Allow usage only in functions (scope type must )
    | Brackets (l,e)  -> match getExpressionType symTable e with
                         | Integer  -> match getLValueType symTable l with
                                       | Array (t, s) -> t
                                       | _            -> raise <| DereferenceTypeException ("Non array object given", e)
                         | _        -> raise <| ArrayIndexTypeException ("Array index must have integer type", e)
    | Dereference e   -> match getExpressionType symTable e with
                         | Ptr x -> x
                         | _     -> raise <| DereferenceTypeException ("Cannot dereference a non-ptr value", e)

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
                               | RExpression _ -> raise <| InvalidAddressException ("Cannot get address of r-value object", e)
    | Call (n, p)           -> let procHdr, procType = getProcessHeader symTable n
                               assertCallCompatibility symTable n p procHdr
                               procType
    | Binop (e1, op, e2)    -> getBinopType (getExpressionType symTable e1) op (getExpressionType symTable e2)
    | Unop (op, e)          -> getUnopType op <| getExpressionType symTable e

  let getExpressionType symTable expr =
    match expr with
    | LExpression l -> getLValueType symTable l
    | RExpression r -> getRValueType symTable r

  let AnalyzeStatement symTable statement =
    match statement with
    | Empty               -> true
    | Error (_, pos)      -> printfn "<Erroneous Statement>\t-> false @ %d" pos.NextLine.Line; false
    | Assign (lval, expr, pos) -> let lvalType = getExpressionType symTable <| LExpression lval
                                  let exprType = getExpressionType symTable expr
                                  let assignmentPossible = canAssign lvalType exprType
                                  printfn "Assign <%A> := <%A>\t-> %b @ %d" lvalType exprType assignmentPossible pos.NextLine.Line
                                  assignmentPossible
    | Block stmts         -> List.forall (AnalyzeStatement symTable) stmts