module FsLox.Test

open NUnit.Framework
open Lox
open System.Collections.Generic

[<Test>]
let TestBasic () =

    let code =
        "var hello = \"Hello, world!\";\
        print(hello);"

    let tokens = code |> Seq.toList |> Tokeniser.tokenise

    let mutable lines = List.empty

    let print: string -> unit =
        fun s ->
            lines <- s :: lines
            ()

    let output = Runner.run print tokens

    let expected = [ "hello", Value.String "Hello, world!" ] |> Map.ofSeq
    Assert.That(output, Is.EqualTo(expected))

    let expectedLines = [ "Hello, world!" ]
    Assert.That(lines, Is.EqualTo(expectedLines))


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

    let tokens =
        code.Split("\n")
        |> Seq.map (fun s -> s.TrimStart())
        |> String.concat "\n"
        |> Seq.toList
        |> Tokeniser.tokenise

    let mutable lines = List.empty

    let print: string -> unit =
        fun s ->
            lines <- s :: lines
            ()

    let output = Runner.run print tokens

    let expected =
        [ "name", Value.String "Guillaume"; "hello", Value.String "hello Guillaume!" ]
        |> Map.ofSeq

    Assert.That(output, Is.EqualTo(expected))

    let expectedLines = [ "hello Guillaume!" ]
    Assert.That(lines, Is.EqualTo(expectedLines))


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

    let tokens =
        code.Split("\n")
        |> Seq.map (fun s -> s.TrimStart())
        |> String.concat "\n"
        |> Seq.toList
        |> Tokeniser.tokenise

    let lines = List<string>()
    let output = Runner.run lines.Add tokens |> Map.toSeq |> dict

    let expected =
        [ "bool", Value.Boolean true; "value", Value.String "it was true!" ] |> dict

    Assert.That(output, Is.EqualTo(expected))

    let expectedLines = [ "it was true!" ] |> List<string>
    Assert.That(lines, Is.EqualTo(expectedLines))

[<Test>]
let TestControlFlow2 () =
    let code =
        """
        var bool = true;

        if bool {
            print("It's true!");
        }
        """

    let tokens =
        code.Split("\n")
        |> Seq.map (fun s -> s.TrimStart())
        |> String.concat "\n"
        |> Seq.toList
        |> Tokeniser.tokenise

    let lines = List<string>()

    let output = Runner.run lines.Add tokens |> Map.toSeq |> dict

    let expected = [ "bool", Value.Boolean true ] |> dict

    Assert.That(output, Is.EqualTo(expected))

    let expectedLines = [ "It's true!" ] |> List<string>
    Assert.That(lines, Is.EqualTo(expectedLines))

[<Test>]
let TestArithmetic () =
    let code =
        """
        fun add3(a, b, c) {
            return a + b + c;
        }

        var value = add3(1, 2, 3);
        """

    let tokens =
        code.Split("\n")
        |> Seq.map (fun s -> s.TrimStart())
        |> String.concat "\n"
        |> Seq.toList
        |> Tokeniser.tokenise
        |> List<Token>

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
        |> List<Token>

    Assert.That(tokens, Is.EqualTo(expectedTokens))

    let lines = List<string>()

    let output = tokens |> List.ofSeq |> Runner.run lines.Add |> Map.toSeq |> dict

    let expected = [ "value", Value.Number 6. ] |> dict

    Assert.That(output, Is.EqualTo(expected))

    Assert.That(lines, Is.Empty)


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
