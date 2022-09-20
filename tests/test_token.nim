import unittest
import std/strutils

from domain/monkey/token import TokenType

type Case[K, V] = object
  value: K
  want: V

suite "token.nim":
  test "token enum":
    type LCase = Case[TokenType, string]
    var cases = @[
      LCase(value: TokenType.ILLEGAL, want: "ILLEGAL"),
      LCase(value: TokenType.EOF, want: "EOF"),
      LCase(value: TokenType.IDENT, want: "IDENT"),
      LCase(value: TokenType.INT, want: "INT"),
      LCase(value: TokenType.STRING, want: "STRING"),
      LCase(value: TokenType.ASSIGN, want: "="),
      LCase(value: TokenType.PLUS, want: "+"),
      LCase(value: TokenType.MINUS, want: "-"),
      LCase(value: TokenType.BANG, want: "!"),
      LCase(value: TokenType.SLASH, want: "/"),
      LCase(value: TokenType.LT, want: "<"),
      LCase(value: TokenType.GT, want: ">"),
      LCase(value: TokenType.COMMA, want: ","),
      LCase(value: TokenType.COLON, want: ":"),
      LCase(value: TokenType.SEMICOLON, want: ";"),
      LCase(value: TokenType.LPAREN, want: "("),
      LCase(value: TokenType.RPAREN, want: ")"),
      LCase(value: TokenType.LBRACE, want: "{"),
      LCase(value: TokenType.RBRACE, want: "}"),
      LCase(value: TokenType.FUNCTION, want: "FUNCTION"),
      LCase(value: TokenType.LET, want: "LET"),
      LCase(value: TokenType.TRUE, want: "TRUE"),
      LCase(value: TokenType.FALSE, want: "FALSE"),
      LCase(value: TokenType.IF, want: "IF"),
      LCase(value: TokenType.ELSE, want: "ELSE"),
      LCase(value: TokenType.RETURN, want: "RETURN"),
      LCase(value: TokenType.EQ, want: "=="),
      LCase(value: TokenType.NOT_EQ, want: "!="),
      LCase(value: TokenType.LBRACKET, want: "["),
      LCase(value: TokenType.RBRACKET, want: "]"),
    ]

    for c in cases:
      check(c.value == parseEnum[TokenType](c.want))