module Tokens

type PCLType = 
    | Integer
    | Boolean
    | Character
    | Real
    | Array of PCLType * int
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


type PCLDeclaration =
    | Variable of string * PCLType

type PCLStatements =
    | Print of string