namespace Compiler
open FSharp.Text.Lexing

module Base =

  type ErrorType =
    | LexerError
    | ParserError
    | SemanticError
    | SymbolicError

  type PCLError =
    { errorType: ErrorType
      position: Position;
      lexeme: string;
      text: string; }

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
        | Integer       -> Helpers.Environment.VariableSize.IntegerSize
        | Boolean       -> Helpers.Environment.VariableSize.BooleanSize
        | Character     -> Helpers.Environment.VariableSize.CharacterSize
        | Real          -> Helpers.Environment.VariableSize.RealSize
        | Array (t, s)  -> s * t.Size
        | IArray _      -> Helpers.Environment.VariableSize.PointerSize
        | Ptr _         -> Helpers.Environment.VariableSize.PointerSize
        | _             -> Helpers.Environment.VariableSize.ErroneousSize

      member this.IsArithmetic =
        match this with
        | Integer | Real -> true
        | _              -> false

      member this.IsComplete =
        match this with
        | IArray _      -> false
        | _             -> true

      static member (=~) (lhs: Type, rhs: Type) =
        match lhs, rhs with
        | Real, Integer  -> true
        | Unit, _        -> false
        | Ptr _, NilType -> true      //* pointer type must be valid, but i don't know if we can generate invalid type without throwing a semantic exception
        | Ptr (IArray t1), Ptr (Array (t2, _))   -> t1 = t2
        | x, y when x.IsComplete && y.IsComplete -> x = y
        | _ -> false

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

  type Expression = 
    | LExpression of LValue 
    | RExpression of RValue
  and RValue =
    | IntConst of int
    | BoolConst of bool
    | RealConst of string
    | CharConst of string
    | RParens of RValue
    | Nil 
    | AddressOf of Expression
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
    | Assign of LValue * Expression * Position
    | Goto of string * Position
    | New of LValue
    | NewArray of LValue * Expression
    | Dispose of LValue
    | DisposeArray of LValue
    | Return
    | SCall of string * Expression list * Position
    | While of Expression * Statement * Position
    | If of Expression * Statement * Statement * Position      // Full if-then-else. On if-then the second statement must be Empty
    | LabeledStatement of string * Statement * Position
    | Error of string * Position

  type Declaration =
    | Variable of string * Type
    | Parameter of string * Type * ProcessParamSpecies
    | Label of string
    | Process  of ProcessHeader * Body
    | Forward  of ProcessHeader
    | DeclError of string * Position
  and Body = Declaration list * Statement list

  type Program = string * Body

  type SemanticInstruction = 
    | SemInt of int
    | SemBool of bool
    | SemReal of string
    | SemChar of string
    | SemString of string
    | SemNil of Type
    | SemIdentifier of int * int        // inter * intra index in Activation Record Hierarchy
    | SemGlobalIdentifier of string     // special case of identifier that is global and not found in the Activation Record Hierarchy
    | SemBinop of SemanticInstruction * SemanticInstruction * BinaryOperator * Type
    | SemUnop of SemanticInstruction * UnaryOperator * Type
    | SemAddress of SemanticInstruction
    | SemAssign of SemanticInstruction * SemanticInstruction
    | SemGoto of string
    | SemIf of SemanticInstruction * SemanticInstruction list * SemanticInstruction list // condition - if part - else part
    | SemWhile of SemanticInstruction * SemanticInstruction list                         // condition - loop instructions
    | SemReturn
    | SemResult
    | SemToFloat of SemanticInstruction      // Cast to float
    | SemToZeroArray of SemanticInstruction * Type
    | SemDeref of SemanticInstruction
    | SemDerefArray of SemanticInstruction * SemanticInstruction
    | SemDeclFunction of string * Type * SemanticInstruction list
    | SemFunctionCall of bool * string * int * (SemanticInstruction * bool) list // is external (uses AR mechanism or not) * function qualified name * nesting level difference * (funtion parameters * isByRef)
    | SemLblStmt of string * SemanticInstruction list
    | SemNew of SemanticInstruction
    | SemNewArray of SemanticInstruction * SemanticInstruction // length * lvalue
    | SemDispose of SemanticInstruction
    | SemDisposeArray of SemanticInstruction

  type SemanticFunction = string * SemanticInstruction list  // qualified name, instruction list
  type SemanticGlobalVariable = string * Type

  