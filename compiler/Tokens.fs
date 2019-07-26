module Tokens

type PCLType = 
    | Unit                          (* This type is used only for the return type of procedures *)
    | Integer
    | Boolean
    | Character
    | Real
    | Array of PCLType * int
    | IArray of PCLType
    | Ptr of PCLType
    | Proc

    with
        member this.Size =
            match this with
            | Integer       -> 2
            | Boolean       -> 1
            | Character     -> 1
            | Real          -> 10
            | Array (t, s)  -> s * t.Size
            | Ptr _         -> 2
            | _             -> 0

type PCLParamType =
    | ByValue
    | ByRef

type PCLProcessParam = string * PCLType * PCLParamType

type PCLProcessHeader = string * PCLProcessParam list * PCLType

type UnaryOperator = 
    | Not
    | Positive
    | Negative

type BinaryOperator =
    | Add | Sub | Mult | Div | Divi | Modi 
    | Equals | NotEquals | Less | LessEquals | Greater | GreaterEquals
    | Or | And 

type RValue =
    | IntConst of int
    | BoolConst of bool
    | RealConst of float
    | CharConst of string
    | RParens of RValue
    | Nil 
    | AddressOf of LValue
    | Unop of UnaryOperator * PCLExpression
    | Binop of PCLExpression * BinaryOperator * PCLExpression
    | Call of string * PCLExpression list
and LValue = 
    | Identifier of string
    | StringConst of string
    | Brackets of LValue * PCLExpression
    | Dereference of PCLExpression
    | Parens of LValue
    | Result
and PCLExpression = LExpression of LValue | RExpression of RValue

type PCLStatement =
    | Empty
    | Print of string
    | Block of PCLStatement list
    | Assign of LValue * PCLExpression

type PCLDeclaration =
    | Variable of string * PCLType
    | Process  of PCLProcessHeader * PCLBody
    | Forward  of PCLProcessHeader
and PCLBody = PCLDeclaration list * PCLStatement list

type PCLProgram = string * PCLBody