module Program

open FSharp.Text.Lexing
open Tokens

[<EntryPoint>]
let main argv =
    let readLexemes str =
      let lexbuf = LexBuffer<_>.FromString str
      let rec aux lexbuf =
        let x = Lexer.read lexbuf
        if x = EOF then []
        else x :: aux lexbuf

      aux lexbuf

    let input = System.IO.File.ReadAllText "../examples/lexerror.mba"

    try
      let lexemes = readLexemes input
      for lexeme in lexemes do
        printfn "%s" <| MinibasicToken.ToString lexeme
    with
      | e -> printfn "Error occured: \n%s" (e.Message)
    0