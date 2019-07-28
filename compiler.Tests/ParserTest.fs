namespace Compiler.Tests

open System
open NUnit.Framework
open FSharp.Text.Lexing

[<TestFixture>]
type TestClass () =

    let parse input =
      let lexbuf = LexBuffer<_>.FromString input
      Parser.start Lexer.read lexbuf

    let input = "program hello; begin end."

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.AlwaysTrue () =
        Assert.True(true)

    [<Test>]
    member this.DoesntThrowException () =
      Assert.DoesNotThrow (fun () -> parse input |> ignore)