module Tokens

// type ConstType = 
//     | Integer of int

type MinibasicExpression =
    | Expr of string
    | Int of int

// type MinibasicToken =
//     | Command of string
//     | Identifier of string
//     | Constant of int
//     | Operator of char
//     | EOF2

//     static member ToString x =
//         match x with
//         | Command c -> sprintf "Command(%s)" c
//         | Identifier i -> sprintf "Identifier(%s)" i
//         | Constant c -> sprintf "Const(%i)" c
//         | Operator o -> sprintf "Operator(%c)" o
//         | _ -> ""

type MinibasicStatement =
    | Print of MinibasicExpression

    static member ToString x = 
        match x with
        | Print s -> sprintf "Print(%A)" s
