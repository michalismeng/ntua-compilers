namespace Compiler.Helpers

open FSharp.Text.Parsing
open FSharp.Text.Lexing
open System.Collections.Generic

open Compiler.Base

module Error =

  exception InternalException of string

  // Name of the file being processed by the compiler
  let mutable FileName = ""

  // position of the last error. Note that not all PCL elements generate a position on error.
  let mutable lastErrorPosition = Unchecked.defaultof<Position>

  let SetLastErrorPosition pos =
    lastErrorPosition <- pos

  let GetLastErrorPosition () =
    lastErrorPosition

  let FormatPrologue (position: Position) =
    sprintf "%s(%d,%d)" FileName (position.Line + 1) (position.Column + 1)

  let StringifyError (error: PCLError) =
    let prologue = sprintf "%s: " <| FormatPrologue error.position
    let body = sprintf "%s." error.text
    let epilogue = match error.errorType with
                   | ParserError | LexerError -> sprintf " Error near token '%s'." error.lexeme
                   | _ -> ""
    prologue + body + epilogue

  module Parser =
    exception ParserException of PCLError

    let errorList = new List<string>()

    let RaiseParseError msg pos lexeme =
      let error = 
        { errorType = ParserError;
          position = pos;
          lexeme = lexeme
          text = msg; }
      raise <| ParserException error

  module Lexer = 
    exception LexerException of PCLError

    let CheckCharacter (str: string) =
      match str.Length with
      | 1 -> true
      | 2 -> str.[0] = '\\'
      | _ -> false

    let private beautifyErroneousToken (tok: string) =
      match tok with
      | "\n" -> "<EOL>"
      | "\t" -> "<TAB>"
      | _    -> tok

    let RaiseLexError msg (lexbuf: LexBuffer<_>) = 
      let error = 
        { errorType = LexerError;
          position = lexbuf.StartPos;
          lexeme = beautifyErroneousToken <| LexBuffer<_>.LexemeString lexbuf;
          text = msg }
      raise <| LexerException error

  module Semantic =
    exception SemanticException of PCLError

    let RaiseSemanticError msg pos =
      let error =
        { errorType = SemanticError;
          position = Option.defaultValue (GetLastErrorPosition ()) pos ;
          lexeme = "";
          text = msg;
        }
      raise <| SemanticException error

  module Symbolic =
    exception SymbolicException of PCLError

    let RaiseSymbolicError msg pos =
      let error =
        { errorType = SemanticError;
          position = Option.defaultValue (GetLastErrorPosition ()) pos ;
          lexeme = "";
          text = msg;
        }
      raise <| SymbolicException error