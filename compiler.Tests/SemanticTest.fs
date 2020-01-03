namespace Compiler.Tests

open NUnit.Framework
open FSharp.Text.Lexing

open Compiler.Base
open Compiler.Semantic
open Compiler.SymbolTable

[<TestFixture>]
type SemanticTest () =

  let rep = List.replicate
  let map = List.map

  let parse input =
    let lexbuf = LexBuffer<_>.FromString input
    Parser.start Lexer.read lexbuf

  let createTestSymbolTable () =
    let symTable = CreateSymbolTable ()
    let symTable = AddDeclarationToTable symTable (Variable ("i", Integer))
    let symTable = AddDeclarationToTable symTable (Variable ("r", Real))
    let symTable = AddDeclarationToTable symTable (Variable ("b", Boolean))
    let symTable = AddDeclarationToTable symTable (Variable ("c", Character))
    symTable

  let parseAssignStatementsToType scope input = 
    let program = (parse input).Value
    let _, body = program
    let _, statements = body
    let mapStmt s = match s with
                    | Assign (_, expr, _) -> fst <| getExpressionType scope expr
                    | _                   -> Unit
    map mapStmt statements

  let parseAssignStatementsScopedToType = 
    let symTable = createTestSymbolTable ()
    parseAssignStatementsToType symTable

  let basicDataTypes = """
    program basic;
    begin
      a := 0; a := 10; a := 00200; a := 15896;
      a := true; a := false;
      a := 'a'; a := 'v'; a := '0'; a := '9'; a := '!'; a := '$'; a := '~'; a := '+'; a := ','; a := '>'; a := ' '; a := '\n';
      a := '\t'; a := '\r'; a := '\0'; a := '\\'; a := '\''; a := '\"'; a := 42.0;
      a := 4.2e1; a := 0.420e+2; a := 42000.0e-3
    end.
  """

  let compositeDataTypes = """
    program composite;
    begin
      b := ""; b := "1234!~@^&>"; b := "abc\n\t\r\0\\\'\"def";
      b := @"normal"
    end.
  """

  let simpleArithmeticTypes = """
    program simpleExpr;
    begin
      c := +1; c := -1;
      c := +1.0; c := -1.0;
      c := 1 * 2; c := 1.0 * 2.0; c := 1 * 2.0; c := 1.0 * 2;
      c := 1 / 2; c := 1.0 / 2.0; c := 1 / 2.0; c := 1 / 2.0;
      c := 1 div 2; c := 1 mod 2; c := 1 + 1; c := 1 - 1  
    end.
  """

  let simpleBooleanTypes = """
    program simpleExpr;
    begin
      c := not true;
      c := true and true; c := false or false;
      c := 1 < 2; c := 1.0 < 2.0; c := 1 < 2.0; c := 1.0 < 2;
      c := 1 <= 2; c := 1.0 <= 2.0; c := 1 <= 2.0; c := 1.0 <= 2;
      c := 1 > 2; c := 1.0 > 2.0; c := 1 > 2.0; c := 1.0 > 2;
      c := 1 >= 2; c := 1.0 >= 2.0; c := 1 >= 2.0; c := 1.0 >= 2;
      c := 1 = 2; c := 1.0 = 2.0; c := 1 = 2.0; c := 1.0 = 2;
      c := 1 <> 2; c := 1.0 <> 2.0; c := 1 <> 2.0; c := 1.0 <> 2;
      c := true = true; c := 'a' = 'a'; c := @"str" = @"str"; 
      c := true <> true; c := 'a' <> 'a'; c := @"str" <> @"str"
    end.
  """

  // TODO: Test operator precedence - exceptions are thrown correctly

  [<Test>]
  member this.CalculatesBasicTypes () =
    let expectedExpressions = rep 4 Integer @ rep 2 Boolean @ rep 18 Character @ rep 4 Real
    let testExpressions = parseAssignStatementsScopedToType basicDataTypes
    printfn "testExpressions: %A" testExpressions
    Assert.AreEqual(testExpressions, expectedExpressions)

  [<Test>]
  member this.CalculatesCompositeTypes () =
    let expectedExpressions = map (fun l -> Array (Character, l)) [1; 11; 21] @ [Ptr <| Array (Character, 7)]
    let testExpressions = parseAssignStatementsScopedToType compositeDataTypes
    printfn "testExpressions: %A" testExpressions
    Assert.AreEqual(testExpressions, expectedExpressions)

  [<Test>]
  member this.CalculatesSimpleArithmeticTypes () =
    let expectedExpressions = rep 2 Integer @ rep 2 Real @ [Integer] @ rep 7 Real @ rep 4 Integer
    let testExpressions = parseAssignStatementsScopedToType simpleArithmeticTypes
    printfn "testExpressions: %A" testExpressions
    Assert.AreEqual(testExpressions, expectedExpressions)

  [<Test>]
  member this.``Calculates Simple Boolean Types`` () =
    let expectedExpressions = rep 33 Boolean
    let testExpressions = parseAssignStatementsScopedToType simpleBooleanTypes
    printfn "testExpressions: %A" testExpressions
    Assert.AreEqual(testExpressions, expectedExpressions)
