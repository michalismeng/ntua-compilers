namespace Compiler

open Compiler.PCL

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

  let private getLValueType (lval: LValue) =
    match lval with
    | StringConst s   -> Array (Character, s.Length)
    | LParens l       -> getLValueType l
    | Identifier s    -> if s.[0] = 'i' then Integer elif s.[0] = 'r' then Real elif s.[0] = 'c' then Character else Boolean //! Requires symbol table information. TODO: Fix
    | Result          -> Unit                 //! Requires function definition (return type) from symbol table
    | Brackets (l,e)  -> match getExpressionType e with
                         | Integer  -> match getLValueType l with
                                       | Array (t, s) -> t
                                       | _            -> raise <| DereferenceTypeException ("Non array object given", e)
                         | _        -> raise <| ArrayIndexTypeException ("Array index must have integer type", e)
    | Dereference e   -> match getExpressionType e with
                         | Ptr x -> x
                         | _     -> raise <| DereferenceTypeException ("Cannot dereference a non-ptr value", e)

  let private getRValueType (rval: RValue) =
    match rval with
    | IntConst _            -> Integer
    | RealConst _           -> Real
    | CharConst _           -> Character
    | BoolConst _           -> Boolean
    | Nil                   -> NilType
    | RParens r             -> getRValueType r
    | AddressOf e           -> match e with
                               | LExpression l -> Ptr <| getLValueType l
                               | RExpression _ -> raise <| InvalidAddressException ("Cannot get address of r-value object", e)
    | Call _                -> Unit       //! Requires function definition (return type) from symbol table
    | Binop (e1, op, e2)    -> getBinopType (getExpressionType e1) op (getExpressionType e2)
    | Unop (op, e)          -> getUnopType op <| getExpressionType e

  let getExpressionType (expr: Expression) =
    match expr with
    | LExpression l -> getLValueType l
    | RExpression r -> getRValueType r

  let private analyzeStatement statement =
    match statement with
    | Empty               -> true
    | Error (_, pos)      -> printfn "<Erroneous Statement>\t-> false @ %d" pos.NextLine.Line; false
    | Assign (lval, expr, pos) -> let lvalType = getExpressionType <| LExpression lval
                                  let exprType = getExpressionType expr
                                  let assignmentPossible = canAssign lvalType exprType
                                  printfn "Assign <%A> := <%A>\t-> %b @ %d" lvalType exprType assignmentPossible pos.NextLine.Line
                                  assignmentPossible
    | Block stmts         -> List.forall analyzeStatement stmts

  let Analyze program = 
    let name, body = program
    let (declarations: PCL.Declaration list), (statements: PCL.Statement list) = body
    printfn "Performing semantic analysis on '%s'" name
    // TODO: do semantic analysis on declarations and build symbol table
    try
      let result = List.fold (fun acc b -> analyzeStatement b :: acc) [] statements
      printfn "\nAnalysis result: %A" <| List.forall id result
    with
    | e -> printfn "%A" e