module FsLox.Test

open NUnit.Framework
open Lox
open System.Collections.Generic

let runTest
    (code: string)
    (expectedTokens: Token list option)
    (expectedEndState: Map<string, Value>)
    (expectedLines: string seq)
    =
    let tokens = code |> Seq.toList |> Tokeniser.tokenise

    match expectedTokens with
    | None -> ()
    | Some t -> Assert.That(tokens |> List<Token>, t |> List<Token> |> Is.EqualTo)

    let lines = List<string>()
    let output = Runner.run lines.Add tokens

    Assert.That(output |> Dictionary.ofMap, Is.EqualTo(expectedEndState |> Dictionary.ofMap))
    Assert.That(lines, Is.EqualTo(expectedLines |> List<string>))


[<Test>]
let TestBasic () =
    let code =
        "var hello = \"Hello, world!\";\
        print(hello);"

    let expected = [ "hello", Value.String "Hello, world!" ] |> Map.ofSeq
    let expectedLines = [ "Hello, world!" ]

    runTest code None expected expectedLines


[<Test>]
let TestFunctions () =

    let code =
        """
        fun add_exclamation(v) {
            var ret = v + "!";
            return ret;
        }

        fun hello(name) {
            var ret = "hello " + name;
            var ret2 = add_exclamation(ret);
            return ret2;
        }
        var name = "Guillaume";
        var hello = hello(name);
        print(hello);
        """

    let expected =
        [ "name", Value.String "Guillaume"; "hello", Value.String "hello Guillaume!" ]
        |> Map.ofSeq

    let expectedLines = [ "hello Guillaume!" ]
    runTest code None expected expectedLines

[<Test>]
let TestFunctions2 () =

    let code =
        """
        fun add_exclamation(v) {
            return v + "!";
        }

        fun hello(name) {
            var ret = "hello " + name;
            return add_exclamation(ret);
        }
        var name = "Guillaume";
        var hello = hello(name);
        print(hello);
        """

    let expected =
        [ "name", Value.String "Guillaume"; "hello", Value.String "hello Guillaume!" ]
        |> Map.ofSeq

    let expectedLines = [ "hello Guillaume!" ]
    runTest code None expected expectedLines

[<Test>]
let TestFunctionsArgOrderOk () =

    let code =
        """
        fun concat(a, b) {
            return a + b;
        }
        var value = concat("hello ", "world");
        """

    let expected = [ "value", Value.String "hello world" ] |> Map.ofSeq
    runTest code None expected []

[<Test>]
let TestMutation () =
    let code =
        """
        var a = 1;
        print(a);
        a <- a + 1;
        print(a);
        """

    let tokens =
        [ Var
          Identifier "a"
          Equals
          Number 1
          Semicolon
          Identifier "print"
          OpenParenthesis
          Identifier "a"
          CloseParenthesis
          Semicolon
          Identifier "a"
          BackArrow
          Identifier "a"
          Plus
          Number 1
          Semicolon
          Identifier "print"
          OpenParenthesis
          Identifier "a"
          CloseParenthesis
          Semicolon ]
        |> Some

    let expected = [ "a", Value.Number 2.0 ] |> Map.ofSeq
    runTest code tokens expected [ "1"; "2" ]

[<Test>]
let TestLoop () =
    let code =
        """
        var counter = 0;

        for i in 0 .. 10 {
            counter <- counter + 1;
        }

        print(counter);
        """

    let tokens =
        [ Var
          Identifier "counter"
          Equals
          Number 0
          Semicolon
          For
          Identifier "i"
          In
          Number 0
          DoubleDot
          Number 10
          OpenBracket
          Identifier "counter"
          BackArrow
          Identifier "counter"
          Plus
          Number 1
          Semicolon
          CloseBracket
          Identifier "print"
          OpenParenthesis
          Identifier "counter"
          CloseParenthesis
          Semicolon ]
        |> Some

    let expected = [ "counter", Value.Number 10.0 ] |> Map.ofSeq
    runTest code tokens expected [ "10" ]

[<Test>]
let TestControlFlow () =
    let code =
        """
        var bool = true;

        var value = if bool {
            return "it was true!";
        } else {
            return "it was false!";
        }

        print(value);
        """

    let expected =
        [ "bool", Value.Boolean true; "value", Value.String "it was true!" ]
        |> Map.ofSeq

    let expectedLines = [ "it was true!" ] |> List<string>
    runTest code None expected expectedLines

[<Test>]
let TestControlFlow2 () =
    let code =
        """
        var bool = true;

        if bool {
            print("It's true!");
        }
        """

    let expected = [ "bool", Value.Boolean true ] |> Map.ofSeq
    let expectedLines = [ "It's true!" ] |> List<string>
    runTest code None expected expectedLines

[<Test>]
let TestFibonacci () =
    let code =
        """
        var a = 0;
        var b = 1;

        var n = 10;

        for i in 0 .. n {
            var temp = a + b;
            a <- b;
            b <- temp;
        }

        print("fibo of 10 is:");
        print(a);
        """

    let expected =
        [ "a", Value.Number 55; "b", Value.Number 89; "n", Value.Number 10 ]
        |> Map.ofSeq

    let expectedLines = [ "fibo of 10 is:"; "55" ] |> List<string>
    runTest code None expected expectedLines

[<Test>]
let TestArithmetic () =
    let code =
        """
        fun add3(a, b, c) {
            return a + b + c;
        }

        var value = add3(1, 2, 3);
        """

    let expectedTokens =
        [ Fun
          Identifier "add3"
          OpenParenthesis
          Identifier "a"
          Comma
          Identifier "b"
          Comma
          Identifier "c"
          CloseParenthesis
          OpenBracket
          Return
          Identifier "a"
          Plus
          Identifier "b"
          Plus
          Identifier "c"
          Semicolon
          CloseBracket
          Var
          Identifier "value"
          Equals
          Identifier "add3"
          OpenParenthesis
          Number 1
          Comma
          Number 2
          Comma
          Number 3
          CloseParenthesis
          Semicolon ]
        |> Some

    let expected = [ "value", Value.Number 6. ] |> Map.ofSeq
    runTest code expectedTokens expected []

[<Test>]
let TestTokeniserFunction () =
    let code =
        """
        fun add_exclamation(v) {
                var ret = v + "!";
                return ret;
            }
        """

    let tokens = code |> Tokeniser.tokenise' |> Array.ofList

    let expected: Token array =
        [| Fun
           Identifier "add_exclamation"
           OpenParenthesis
           Identifier "v"
           CloseParenthesis
           OpenBracket
           Var
           Identifier "ret"
           Equals
           Identifier "v"
           Plus
           String "!"
           Semicolon
           Return
           Identifier "ret"
           Semicolon
           CloseBracket |]

    Assert.That(tokens, Is.EqualTo(expected))
