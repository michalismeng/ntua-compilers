namespace Compiler.Helpers

open FSharp.Text.Parsing
open FSharp.Text.Lexing
open System.Collections.Generic
open System.Reflection

module Error =
  // Name of the file being processed by the compiler
  let mutable FileName = ""

  let FormatPrologue (position: Position) =
    sprintf "%s(%d,%d):" FileName (position.Line + 1) (position.Column + 1)

  module Parser =
    let errorList = new List<string>()

    let private (?) x prop =
      let flags = BindingFlags.GetProperty ||| BindingFlags.InvokeMethod
      x.GetType().InvokeMember(prop, flags, null, x, [||])

    let erroneousLexeme (o: ParseErrorContext<'tok>) =
      sprintf "%s" <| new string ((o.ParseState.ParserLocalStore.["LexBuffer"]?Lexeme) :?> char[])

  module Lexer = 
    exception LexerException of string

    let checkCharacter (str: string) =
      match str.Length with
      | 1 -> true
      | 2 -> str.[0] = '\\'
      | _ -> false