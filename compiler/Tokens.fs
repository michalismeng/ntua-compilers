module Tokens

// type ConstType = 
//     | Integer of int

type MinibasicExpression =
    | Expr of string
    | Int of int

type MinibasicStatement =
    | Print of MinibasicExpression

    static member ToString x = 
        match x with
        | Print s -> sprintf "Print(%A)" s
