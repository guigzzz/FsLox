module FxLox.TestClass

open NUnit.Framework
open FsLox.Test

open Lox

let makeDummyFunction name args =
    Function.make name args (fun _ -> "" |> Value.String)

[<Test>]
let TestBasic () : unit =

    let code =
        """
        class HelloWorldPrinter {
            call() {
                print("hello world");
            }
        }

        var c = HelloWorldPrinter();
        c.call();
        """

    let expectedTokens =
        [ Class
          Identifier "HelloWorldPrinter"
          OpenBracket

          Identifier "call"
          OpenParenthesis
          CloseParenthesis
          OpenBracket
          Identifier "print"
          OpenParenthesis
          String "hello world"
          CloseParenthesis
          Semicolon
          CloseBracket
          CloseBracket

          Var
          Identifier "c"
          Equals
          Identifier "HelloWorldPrinter"
          OpenParenthesis
          CloseParenthesis
          Semicolon
          Identifier "c"
          Dot
          Identifier "call"
          OpenParenthesis
          CloseParenthesis
          Semicolon ]
        |> Some

    let expectedEndState =
        [ "c",
          { Type = "HelloWorldPrinter"
            Variables = Map.empty
            Functions = [ "call", makeDummyFunction "call" List.empty ] |> Map.ofSeq }
          |> Object ]
        |> Map.ofSeq

    let expectedLines = [ "hello world" ]

    runTest code expectedTokens expectedEndState expectedLines
