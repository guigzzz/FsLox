module FsLox.Test

open NUnit.Framework
open Lox

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
