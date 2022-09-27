import unittest
import std/tables

from domain/monkey/lexer import newLexer
from domain/monkey/token import TokenType

suite "lexer.nim":
  test "lexer next token":
    const cases = {
        "=+(){},;": @[
            (TokenType.ASSIGN, "="),
            (TokenType.PLUS, "+"),
            (TokenType.LPAREN, "("),
            (TokenType.RPAREN, ")"),
            (TokenType.LBRACE, "{"),
            (TokenType.RBRACE, "}"),
            (TokenType.COMMA, ","),
            (TokenType.SEMICOLON, ";"),
            (TokenType.EOF, $'\0'),
        ],
        """let five = 5;
        let ten = 10;
        
        let add = fn(x, y) {
          x + y;
        };
        
        let result = add(five, ten);
        """: @[
            (TokenType.LET, "let"),
            (TokenType.IDENT, "five"),
            (TokenType.ASSIGN, "="),
            (TokenType.INT, "5"),
            (TokenType.SEMICOLON, ";"),
            (TokenType.LET, "let"),
            (TokenType.IDENT, "ten"),
            (TokenType.ASSIGN, "="),
            (TokenType.INT, "10"),
            (TokenType.SEMICOLON, ";"),
            (TokenType.LET, "let"),
            (TokenType.IDENT, "add"),
            (TokenType.ASSIGN, "="),
            (TokenType.FUNCTION, "fn"),
            (TokenType.LPAREN, "("),
            (TokenType.IDENT, "x"),
            (TokenType.COMMA, ","),
            (TokenType.IDENT, "y"),
            (TokenType.RPAREN, ")"),
            (TokenType.LBRACE, "{"),
            (TokenType.IDENT, "x"),
            (TokenType.PLUS, "+"),
            (TokenType.IDENT, "y"),
            (TokenType.SEMICOLON, ";"),
            (TokenType.RBRACE, "}"),
            (TokenType.SEMICOLON, ";"),
            (TokenType.LET, "let"),
            (TokenType.IDENT, "result"),
            (TokenType.ASSIGN, "="),
            (TokenType.IDENT, "add"),
            (TokenType.LPAREN, "("),
            (TokenType.IDENT, "five"),
            (TokenType.COMMA, ","),
            (TokenType.IDENT, "ten"),
            (TokenType.RPAREN, ")"),
            (TokenType.SEMICOLON, ";"),
            (TokenType.EOF, $'\0'),
        ],
        """
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        """: @[
            (TokenType.BANG, $'!'),
            (TokenType.MINUS, $'-'),
            (TokenType.SLASH, $'/'),
            (TokenType.ASTERISC, $'*'),
            (TokenType.INT, "5"),
            (TokenType.SEMICOLON, $';'),
            (TokenType.INT, "5"),
            (TokenType.LT, "<"),
            (TokenType.INT, "10"),
            (TokenType.GT, ">"),
            (TokenType.INT, "5"),
            (TokenType.SEMICOLON, $';'),
            (TokenType.IF, "if"),
            (TokenType.LPAREN, "("),
            (TokenType.INT, "5"),
            (TokenType.LT, "<"),
            (TokenType.INT, "10"),
            (TokenType.RPAREN, ")"),
            (TokenType.LBRACE, "{"),
            (TokenType.RETURN, "return"),
            (TokenType.TRUE, "true"),
            (TokenType.SEMICOLON, $';'),
            (TokenType.RBRACE, "}"),
            (TokenType.ELSE, "else"),
            (TokenType.LBRACE, "{"),
            (TokenType.RETURN, "return"),
            (TokenType.FALSE, "false"),
            (TokenType.SEMICOLON, $';'),
            (TokenType.RBRACE, "}"),
            (TokenType.EOF, $'\0'),
        ],
        """
        10 == 10;
        10 != 9;
        """: @[
            (TokenType.INT, "10"),
            (TokenType.EQ, "=="),
            (TokenType.INT, "10"),
            (TokenType.SEMICOLON, ";"),
            (TokenType.INT, "10"),
            (TokenType.NOT_EQ, "!="),
            (TokenType.INT, "9"),
            (TokenType.SEMICOLON, ";"),
        ],
        """
        "foobar"
        "foo bar"
        """: @[
            (TokenType.STRING, "foobar"),
            (TokenType.STRING, "foo bar"),
        ],
        "[1, 2];": @[
            (TokenType.LBRACKET, "["),
            (TokenType.INT, "1"),
            (TokenType.COMMA, ","),
            (TokenType.INT, "2"),
            (TokenType.RBRACKET, "]"),
            (TokenType.SEMICOLON, ";"),
        ],
        "{\"foo\": \"bar\"}": @[
            (TokenType.LBRACE, "{"),
            (TokenType.STRING, "foo"),
            (TokenType.COLON, ":"),
            (TokenType.STRING, "bar"),
            (TokenType.RBRACE, "}"),
        ],
    }.toTable;
    for text, tests in cases:
        let lexer = newLexer(text)
        for want in tests:
            let tk = lexer.nextToken()
            assert want[0] == tk.typ, "expected: " & $want[0] & " , got: " & $tk.typ
            assert want[1] == tk.literal