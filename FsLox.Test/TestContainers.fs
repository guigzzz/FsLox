module FxLox.TestContainers

open NUnit.Framework

open FsLox.Test
open Lox

let arrayListOfNumbers (ns: double seq) : Value =
    ns |> Seq.map Value.Number |> List.ofSeq |> ListObject.make


[<Test>]
let TestList () : unit =

    let code =
        """
        var l = list();
        l <- l.append(1);
        print(l);
        """

    let expectedTokens =
        [ Var
          Identifier "l"
          Equals
          Identifier "list"
          OpenParenthesis
          CloseParenthesis
          Semicolon

          Identifier "l"
          BackArrow
          Identifier "l"
          Dot
          Identifier "append"
          OpenParenthesis
          Number 1
          CloseParenthesis
          Semicolon

          Identifier "print"
          OpenParenthesis
          Identifier "l"
          CloseParenthesis
          Semicolon

          ]
        |> Some

    let expectedOutput = [ "l", arrayListOfNumbers [ 1 ] ] |> Map.ofSeq
    let expectedLines = [ "List(internal_array: [1])" ]

    runTest code expectedTokens expectedOutput expectedLines
