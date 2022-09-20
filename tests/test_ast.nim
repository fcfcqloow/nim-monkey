import unittest
import options
import domain/monkey/ast
import domain/monkey/token

suite "ast.nim":
  test "to string":
      let program = ast.Program(statements: @[
        ast.Statement(
            ast.LetStatement(
                token: token.Token(typ: token.TokenType.LET,literal: "let"),
                name: ast.Identifier(
                    token: token.Token(typ: token.TokenType.IDENT, literal: "myVar"),
                    value: "myVar"
                ),
                value: option(ast.Expression(ast.Identifier(
                    token: token.Token(typ: token.TokenType.IDENT, literal: "anotherVar"),
                    value: "anotherVar"
                )))
            )
        ),
      ])
      check $program == "let myVar = anotherVar;"