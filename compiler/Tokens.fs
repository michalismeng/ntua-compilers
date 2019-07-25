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

type PCLStatement =
    | Print of string

type PCLDeclaration =
    | Variable of string * PCLType
    | Process  of PCLProcessHeader * PCLBody
    | Forward  of PCLProcessHeader

and PCLBody = PCLDeclaration list * PCLStatement list

type PCLProgram = string * PCLBody