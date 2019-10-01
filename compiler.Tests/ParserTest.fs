namespace Compiler.Tests

open NUnit.Framework
open Compiler.Base
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

  let isVariable d = 
    match d with 
    | Variable _ -> true 
    | _          -> false 
   
  let isForwardProc d = 
    match d with
    | Forward _ -> true
    | _         -> false

  let parseToSingleDeclaration decl input =
    let program = (parse input).Value
    let _, body = program
    let declarations, _ = body
    List.filter decl declarations

  let expressions = """
    program expressions;

    begin
        x := (1 + 2 - 2 * 3) / 3;
        y := @x * 2;
        z := x[10] / 2 + 1;
        w := @x[10];
        a := @x^;
        u := @a[10]^;
        z := @a^[10];
        o := x[1 + a^];
    end.
  """

  let variables = """
    program hello;
      var a : integer;
      var b   :   real;
      var c,  d,e : boolean ; f,g,h : char;

      var str  : array [10] of char;
          str2 : array of char;
          bitVector : array of boolean;
          image : array [24] of array of real;

      var ptr : ^^array [24] of array of ^integer;
    begin
    end.
  """

  let procs = """
    program procs;
      forward procedure p1 ();
      forward procedure p2 (n : integer);
      forward procedure p3 (n : integer; z : real);
      forward procedure p4 (a,b:integer; b:boolean; var c,d: real);

      forward function f1 () : real;
      forward function f2 (x: real) : real;
      forward function f3 (var s : array of char) : integer;
      forward function f4 (n : integer; var x : real) : ^array of array [10] of real;
    begin
    end.
  """

  [<Test>]
  member this.ParsesVariables () =
    let names = (['a' .. 'h'] |> List.map string) @ ["str"; "str2"; "bitVector"; "image"; "ptr"]
    let types = [Integer; Real] @ List.replicate 3 Boolean @ List.replicate 3 Character @
                   [Array (Character, 10); IArray Character; IArray Boolean; Array (IArray Real, 24); Ptr (Ptr (Array (IArray (Ptr Integer), 24)))]

    let expectedVariables = List.zip names types |> List.map Variable
    let testVariables = parseToSingleDeclaration isVariable variables
    let errors = Compiler.Helpers.Error.Parser.errorList
    printfn "%A" errors
    printfn "%A" testVariables
    Assert.AreEqual(errors, [])
    Assert.AreEqual(testVariables, expectedVariables)

  [<Test>]
  member this.ParsesForwardDeclarations () =
    let names = ["p1"; "p2"; "p3"; "p4"] @ ["f1"; "f2"; "f3"; "f4"]
    let parameters = [[]; [("n", Integer, ByValue)]; [("n", Integer, ByValue); ("z", Real, ByValue)]; 
                      [("a", Integer, ByValue); ("b", Integer, ByValue); ("b", Boolean, ByValue); ("c", Real, ByRef); ("d", Real, ByRef)]] @
                     [[]; [("x", Real, ByValue)]; [("s", IArray Character, ByRef)]; [("n", Integer, ByValue); ("x", Real, ByRef)]]
    let results = List.replicate 4 Unit @ [Real; Real; Integer; Ptr (IArray (Array (Real, 10)))]

    let expectedForwardDeclarations = List.zip3 names parameters results |> List.map Forward
    let testDeclarations = parseToSingleDeclaration isForwardProc procs
    let errors = Compiler.Helpers.Error.Parser.errorList
    printfn "%A" errors
    printfn "%A" testDeclarations
    Assert.AreEqual(errors, [])
    Assert.AreEqual(testDeclarations, expectedForwardDeclarations)


  [<Test>]
  member this.DoesntThrowException () =
    parse expressions |> ignore
    let errors = Compiler.Helpers.Error.Parser.errorList
    printfn "%A" errors
    Assert.AreEqual(errors, []) 