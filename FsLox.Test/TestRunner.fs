module FsLox.TestRunner

open NUnit.Framework
open Lox
open System.Collections.Generic


[<Test>]
let TestSplitByTokenPreserveParens () =

    let tokens = """concat("hello", " world"), " !" """ |> Tokeniser.tokenise'

    let splits =
        Runner.splitByTokenPreserveParens Comma tokens |> List.map List<_> |> List<_>

    let expected =
        [ [ Identifier "concat"
            OpenParenthesis
            String "hello"
            Comma
            String " world"
            CloseParenthesis ]
          |> List<_>
          [ String " !" ] |> List<_> ]
        |> List<_>

    Assert.That(splits, Is.EqualTo expected)
