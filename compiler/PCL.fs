namespace Compiler.PCL
open System

type Type = 
    | Unit                          (* This type is used only for the return type of procedures *)
    | NilType
    | Integer
    | Boolean
    | Character
    | Real
    | Array of Type * int
    | IArray of Type
    | Ptr of Type
    | Proc

    with
        member this.Size =
            match this with
            | Integer       -> 2
            | Boolean       -> 1
            | Character     -> 1
            | Real          -> 10
            | Array (t, s)  -> s * t.Size
            | IArray _      -> 2
            | Ptr _         -> 2
            | _             -> 0

        member private a.TypeEquals b =
            match a, b with
            | Array (t1, _), Array(t2, _) -> t1 = t2
            | _                           -> a = b

        static member (=~) (a: Type, b) =
            a.TypeEquals b

type UnaryOperator = 
    | Not
    | Positive
    | Negative

type BinaryOperator =
    | Add | Sub | Mult | Div | Divi | Modi 
    | Equals | NotEquals | Less | LessEquals | Greater | GreaterEquals
    | Or | And 

type ProcessParamSpecies =
    | ByValue
    | ByRef

type ProcessParam = string * Type * ProcessParamSpecies

type ProcessHeader = string * ProcessParam list * Type

type Expression = LExpression of LValue | RExpression of RValue
and RValue =
    | IntConst of int
    | BoolConst of bool
    | RealConst of string
    | CharConst of string
    | RParens of RValue
    | Nil 
    | AddressOf of Expression     (* This should be LValue but the parser generates reduce-reduce conflict. TODO: Check that it is actually LExpression *)
    | Unop of UnaryOperator * Expression
    | Binop of Expression * BinaryOperator * Expression
    | Call of string * Expression list
and LValue = 
    | Identifier of string
    | StringConst of string
    | Brackets of LValue * Expression
    | Dereference of Expression
    | LParens of LValue
    | Result

type Statement =
    | Empty
    | Block of Statement list
    | Assign of LValue * Expression

type Declaration =
    | Variable of string * Type
    | Process  of ProcessHeader * Body
    | Forward  of ProcessHeader
and Body = Declaration list * Statement list

type Program = string * Body