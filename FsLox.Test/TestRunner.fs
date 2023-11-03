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

[<Test>]
let TestRemoveOuterParens_DontRemoveWhenNotNeeded () =

    let tokens = """(a() + b()) / b()""" |> Tokeniser.tokenise'

    let out = Runner.removeOuterParens tokens

    Assert.That(out, Is.EqualTo tokens)

[<Test>]
let TestRemoveOuterParens_RemoveWhenNeeded () =

    let tokens = """(1 + 2)""" |> Tokeniser.tokenise'

    let out = tokens |> Runner.removeOuterParens |> List<_>

    let expected = 
        [
            Number 1; Plus; Number 2
        ] 
        |> List<_>

    Assert.That(out, Is.EqualTo expected)
