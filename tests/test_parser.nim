import unittest
import options
import std/typeinfo
import strformat
import tables
import domain/monkey/lexer
import domain/monkey/parser
import domain/monkey/ast

var 
    zero    = 0
    one     = 1
    two     = 2
    three   = 3
    five    = 5
    eight   = 8
    teen    = 10
    fifteen = 15
    x       = "x"
    y       = "y"
    z       = "z"
let
    ZERO    = zero.toAny
    ONE     = one.toAny
    TWO     = two.toAny
    THREE   = three.toAny
    FIVE    = five.toAny
    EIGHT   = eight.toAny
    TEEN    = teen.toAny
    FIFTEEN = fifteen.toAny
    X       = x.toAny
    Y       = y.toAny
    Z       = z.toAny

proc testIdentifier(exp: ast.Expression, value: string) =
    let indetExp = ast.Identifier(exp)
    check:
        indetExp.value == value
        indetExp.tokenLiteral() == value
proc testIntegerLiteral(exp: ast.Expression, value: int) =
    let indetExp = ast.IntegerLiteral(exp)
    check:
        indetExp.value == value
        indetExp.tokenLiteral() == $value
proc testLiteralExpression(exp: ast.Expression, value: Any) =
    case value.kind
    of akInt:    testIntegerLiteral(exp, value.getInt())
    of akString: testIdentifier(exp, value.getString())
    else: raise
proc testInfixExpression(exp: ast.Expression, left: Any, operator: string, right: Any) =
    let infixExp = ast.InfixExpression(exp)
    check infixExp.operator == operator
    testLiteralExpression(infixExp.left.get(), left)
    testLiteralExpression(infixExp.right.get(), right)

suite "parser.nim statement":
    test "program  string":
        const cases = @[
            ("-a * b","((-a) * b)"),
            ("!-a","(!(-a))"),
            ("a + b + c","((a + b) + c)"),
            ("a + b - c","((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4","((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("true","true"),
            ("false","false"),
            ("3 > 5 == false","((3 > 5) == false)"),
            ("3 < 5 == true","((3 < 5) == true)"),
            ("1 + (2 + 3) + 4","((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2","((5 + 5) * 2)"),
            ("2 / (5 + 5)","(2 / (5 + 5))"),
            ("(5 + 5) * 2 * (5 + 5)","(((5 + 5) * 2) * (5 + 5))"),
            ("-(5 + 5)","(-(5 + 5))"),
            ("!(true == true)","(!(true == true))"),
            ("a + add(b * c) + d","((a + add((b * c))) + d)"),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))","add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            ("add(a + b + c * d / f + g)","add((((a + b) + ((c * d) / f)) + g))"),
            ("a + add(b * c) + d","((a + add((b * c))) + d)"),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))","add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
            ("[1, 2 * 3]", "[1, (2 * 3)]"),
            ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
            ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"),
        ]
        for values in cases:
            let l = newLexer(values[0])
            let p = parser.newParser(l)
            let program = p.parseProgram()
            assert values[1] == $program.get(), fmt"expected {values[1]} but got = {$program.get()}"

    const tests = {
        "parse let": (
            @["""
            let x = 5;
            let y = 10;
            let foobar = 838383;
            """], 
            3,
            proc(statements: seq[ast.Statement], inputIdx: int): void = 
                for idx, tt in @["x", "y", "foobar"]:
                    let smt = cast[ast.LetStatement](statements[idx])
                    check:
                        ast.Node(smt).tokenLiteral() == "let"
                        tt == smt.name.value
        ),
        "parse return": (
            @["""
            return 5;
            return 10;
            return 993322;
            """],
            3,
            proc(statements: seq[ast.Statement], inputIdx: int): void = return
        ),
        "parse identigier express": (
            @["foobar;"],
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void = 
                let expSmt = ast.ExpressionStatement(statements[0])
                let ident = ast.Identifier(expSmt.expression.get())

                check:
                    ident.value == "foobar"
                    ident.tokenLiteral() == "foobar"
        ),
        "parse integer literal expression": (
            @["5;"],
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void = 
                let expSmt = ast.ExpressionStatement(statements[0])
                let literal = ast.IntegerLiteral(expSmt.expression.get())
                check:
                    literal.value == 5
                    literal.tokenLiteral() == "5"
        ),
        "parse prefix expressions": (
            @["!5", "-15"],
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void =
                let expSmt = ast.ExpressionStatement(statements[0])
                let preExp = ast.PrefixExpression(expSmt.expression.get())
                let integ = ast.IntegerLiteral(preExp.right.get())
                let checks = proc(operator: string, value: int64, literal: string) = 
                    check:
                        preExp.operator == operator
                        integ.value == value
                        integ.tokenLiteral() == literal
                
                case inputIdx:
                of 0: checks("!", 5, "5")
                of 1: checks("-", 15, "15")
                else: return
        ),
        "parse infix expressions": (
            @[
                "5 + 5;",
                "5 - 5;",
                "5 * 5;",
                "5 / 5;",
                "5 > 5;",
                "5 < 5;",
                "5 == 5;",
                "5 != 5;",
                "true == true",
                "true != false",
                "false == false",
            ],
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void =
                let expSmt = ast.ExpressionStatement(statements[0])
                let inExp = ast.InfixExpression(expSmt.expression.get())
                
                proc checksInt(operator: string, leftv, rightv: auto, lefts, rights: string) = 
                    let left = ast.IntegerLiteral(inExp.left.get())
                    let right = ast.IntegerLiteral(inExp.right.get())
                    check:
                        inExp.operator == operator
                        left.value == leftv
                        left.tokenLiteral() == lefts
                        right.value == rightv
                        right.tokenLiteral() == rights

                proc checksBool(operator: string, leftv, rightv: auto, lefts, rights: string) = 
                    let left = ast.Boolean(inExp.left.get())
                    let right = ast.Boolean(inExp.right.get())
                    check:
                        inExp.operator == operator
                        left.value == leftv
                        left.tokenLiteral() == lefts
                        right.value == rightv
                        right.tokenLiteral() == rights
                
                case inputIdx:
                of 0: checksInt("+", 5, 5, "5", "5")
                of 1: checksInt("-", 5, 5, "5", "5")
                of 2: checksInt("*", 5, 5, "5", "5")
                of 3: checksInt("/", 5, 5, "5", "5")
                of 4: checksInt(">", 5, 5, "5", "5")
                of 5: checksInt("<", 5, 5, "5", "5")
                of 6: checksInt("==", 5, 5, "5", "5")
                of 7: checksInt("!=", 5, 5, "5", "5")
                of 8: checksBool("==", true, true, "true", "true")
                of 9: checksBool("!=", true, false, "true", "false")
                of 10: checksBool("==", false, false, "false", "false")
                else: return
        ),
        "parse bool expression": (
            @["true;", "false;"],
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void =
                let expSmt = ast.ExpressionStatement(statements[0])
                let boolExp = ast.Boolean(expSmt.expression.get())
                case inputIdx:
                of 0: check(boolExp.value == true)
                of 1: check(boolExp.value == false)
                else: discard
        ),
        "parse if expression": (
            @["true;", "false;"],
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void =
                let expSmt = ast.ExpressionStatement(statements[0])
                let boolExp = ast.Boolean(expSmt.expression.get())
                case inputIdx:
                of 0: check(boolExp.value == true)
                of 1: check(boolExp.value == false)
                else: discard
        ),
        "parse if expression2": (
            @["if (x < y) { x }"],
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void =
                let smt = ast.ExpressionStatement(statements[0])
                let ifExp = ast.IfExpression(smt.expression.get())
                testInfixExpression(ifExp.condition, X, "<", Y)
                check ifExp.consequence.get().statements.len == 1
                let expSmt = ast.ExpressionStatement(ifExp.consequence.get().statements[0])
                testIdentifier(expSmt.expression.get(), "x")
                check ifExp.alternative.isNone()
        ),
        "parse function literal" : (
            @["fn(x, y) { x + y; }"],
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void =
                let expStmt = ast.ExpressionStatement(statements[0])
                let function = ast.FunctionLiteral(expStmt.expression.get())
                check function.parameters.len == 2
                check function.body.get().statements.len == 1
                testLiteralExpression(function.parameters[0].get(), X)
                testLiteralExpression(function.parameters[1].get(), Y)

                let bodyStmt = ast.ExpressionStatement(function.body.get().statements[0])
                testInfixExpression(bodyStmt.expression.get(), X, "+", Y)
                return;
        ),
        "parse function params" : (
            @["fn() { }", "fn(x) { }", "fn(x, y, z) { x }"],
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void =
                let expStmt = ast.ExpressionStatement(statements[0])
                let function = ast.FunctionLiteral(expStmt.expression.get())
                case inputIdx:
                of 0:
                    check function.parameters.len == 0
                of 1:
                    check function.parameters.len == 1
                    testLiteralExpression(function.parameters[0].get(), X)
                of 2: 
                    check function.parameters.len == 3
                    testLiteralExpression(function.parameters[0].get(), X)
                    testLiteralExpression(function.parameters[1].get(), Y)
                    testLiteralExpression(function.parameters[2].get(), Z)
                else: discard
                return;
        ),
        "parse call": (
            @["add(1, 2 * 3, 4 + 5);"], 
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void = 
                let expStmt = ast.ExpressionStatement(statements[0])
                let callExp = ast.CallExpression(expStmt.expression.get())
                testIdentifier(callExp.function, "add")
                check callExp.arguments.len == 3
                testLiteralExpression(callExp.arguments[0], ONE)
        ),
        "parse array": (
            @["[1, 2 * 2, 3 + 3]"], 
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void = 
                let smt = ast.ExpressionStatement(statements[0])
                let arr = ast.ArrayLiteral(smt.expression.get())
                testIntegerLiteral(arr.elements[0], 1)
                testInfixExpression(arr.elements[1], TWO, "*", TWO)
                testInfixExpression(arr.elements[2], THREE, "+", THREE)
        ),
        "parse hashmap": (
            @[
                "{\"one\": 1, \"two\": 2, \"three\": 3}",
                "{}",
                "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}",
            ], 
            1,
            proc(statements: seq[ast.Statement], inputIdx: int): void =
                let smt = ast.ExpressionStatement(statements[0])
                let hash = ast.HashLiteral(smt.expression.get())
                
                case inputIdx:
                of 0:
                    for k, v in hash.pairs:
                        let literal = ast.StringLiteral(k)
                        testIntegerLiteral(v, { "one": 1, "two": 2, "three": 3 }.toTable[$literal])
                of 1: assert hash.pairs.len == 0
                of 2:
                    for k, v in hash.pairs:
                        let literal = ast.StringLiteral(k)
                        case $literal:
                        of "one": testInfixExpression(v, ZERO, "+", ONE)
                        of "two": testInfixExpression(v, TEEN, "-", EIGHT)
                        of "three": testInfixExpression(v, FIFTEEN, "/", FIVE)
                else: raise
        ),
    }
    for value in tests:
        let title = value[0]
        let values = value[1]
        test title:
            for idx, input in values[0]:
                let l = newLexer(input)
                let p = parser.newParser(l)
                let program = p.parseProgram()
                assert not program.isNone(), "expected not program.isNone()"
                assert program.get().statements.len == values[1], "expected length: " & $values[1] & ", but " & $program.get().statements.len

                let errs  = p.errors()
                for err in errs:
                    echo "error: " & $err
                    raise 
                values[2](program.get().statements, idx)
