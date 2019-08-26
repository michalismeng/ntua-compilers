namespace Compiler.Tests

open NUnit.Framework
open Compiler.Base
open FSharp.Text.Lexing
open Parser

[<TestFixture>]
type LexerTest () =

  let basicLexer = """
    asdv
    Asdv
    A456sDv456_klsdjf
    0 42 1284 00200
    42.0 4.2e1 0.420e+2 42000.0e-3
    "abc" "Route 66" "Hello World!\n" "Name:\t\"Douglas Adams\"\nValue:\t42\n"
    (* this is a comment *)
    (* And this is a multiline comment \n\n\t\\\?
        with many lines
    *)
    these are identifiers "Hello"
    'a' '1' '\n' '\''
  """

  let badIdUnderscore = """
    _asd
  """

  // TODO: Check that all keywords are supported

  let readLexemes str =
    let lexbuf = LexBuffer<_>.FromString str
    let rec aux lexbuf =
      let x = Lexer.read lexbuf
      if x = EOF then []
      else x :: aux lexbuf

    aux lexbuf

  let rep = List.replicate
    
  [<Test>]
  member this.LexesStrings () =
    let expected = [T_id "asdv"; T_id "Asdv"; T_id "A456sDv456_klsdjf"; 
                    T_int 0; T_int 42; T_int 1284; T_int 200;
                    T_real "42.0"; T_real "4.2e1"; T_real "0.420e+2"; T_real "42000.0e-3";
                    T_str "abc\x00"; T_str "Route 66\x00"; T_str "Hello World!\\n\x00"; T_str "Name:\\t\\\"Douglas Adams\\\"\\nValue:\\t42\\n\x00";
                    T_id "these"; T_id "are"; T_id "identifiers"; T_str "Hello\x00";
                    T_char "a"; T_char "1"; T_char "\\n"; T_char "\\'" ]
    let testLexemes = readLexemes basicLexer     

    printfn "%A" testLexemes

    Assert.AreEqual (expected, testLexemes)

  [<Test>]
  member this.``Crashes when id starts with bad character`` () =
    let input = ["_asd"; "!asd"]
    let assertion input = Assert.Throws<Compiler.Helpers.Error.Lexer.LexerException> (fun () -> printfn "%A" <| readLexemes input) |> ignore
    List.iter assertion input
    
