module Tokens

// type ConstType = 
//     | Integer of int

type MinibasicToken =
    | Command of string
    | Identifier of string
    | Constant of int
    | Operator of char
    | EOF

    static member ToString x =
        match x with
        | Command c -> sprintf "Command(%s)" c
        | Identifier i -> sprintf "Identifier(%s)" i
        | Constant c -> sprintf "Const(%i)" c
        | Operator o -> sprintf "Operator(%c)" o
        | _ -> ""