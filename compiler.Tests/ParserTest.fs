namespace Compiler.Tests

open NUnit.Framework
open FSharp.Text.Lexing

[<TestFixture>]
type ParserTest () =

  let readLexemes str =
      let lexbuf = LexBuffer<_>.FromString str
      let rec aux lexbuf =
        let x = Lexer.read lexbuf
        if x = Parser.EOF then []
        else x :: aux lexbuf
      aux lexbuf

  let parse input =
    let lexbuf = LexBuffer<_>.FromString input
    Parser.start Lexer.read lexbuf

  let expressions = """
    program expressions;

    begin
        x := (1 + 2 - 2 * 3) / 3;        (* -1 *)
        y := @x * 2;                     (* 2440 = (1000 + 100 + 120) * 2 *)
        z := x[10] / 2 + 1;              (* 256 *)
        w := @x[10];                     (* 1510 *)
        a := @x^;                        (* 4880 *)
        u := @a[10]^;
        z := @a^[10];                    (* ?? *)
        o := x[1 + a^];
    end.
  """

  [<SetUp>]
  member this.Setup () =
    ()

  [<Test>]
  member this.AlwaysTrue () =
    Assert.True(true)

  [<Test>]
  member this.DoesntThrowException () =
    Assert.DoesNotThrow (fun () -> parse expressions |> ignore)