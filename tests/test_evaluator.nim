import unittest
import options
import hashes
import tables

import domain/monkey/ast
import domain/monkey/obj
import domain/monkey/lexer
import domain/monkey/parser
import domain/monkey/evaluator

proc testEval(input: string): obj.Object =
    let l = lexer.newLexer(input)
    let p = parser.newParser(l)
    let program = p.parseProgram()

    return evaluator.eval(ast.Node(program.get()), newEnviroment())

func testInteger(o: obj.Object, exp: int64) = assert obj.Integer(o).value == exp, "expected: `" & $exp & "` but got: `" & $obj.Integer(o).value & "`"
func testBoolean(o: obj.Object, exp: bool) = assert obj.Boolean(o).value == exp, "expected: `" & $exp & "` but got: `" & $obj.Boolean(o).value & "`"
func testNull(o: obj.Object) = assert isSame(o, obj.Null()), "expected: null but got: `" & $o & "`"
func testReturn(o: Object, exp: Object) = assert isSame(o, exp), "expected: `" & $exp & "` but got: `" & $obj.Return(o).value & "`"
func testError(o: Object, exp: string) = assert obj.Error(o).message == exp, "expected: `" & $exp & "` but got: `" & obj.Error(o).message & "`"
func testFunction(o: Object, exp: string) = assert $obj.Function(o) == exp, "expected: `" & $exp & "` but got: `" & $obj.Function(o) & "`"
func testString(o: Object, exp: string) = assert obj.String(o).value == exp, "expected: `" & $exp & "` but got: `" & obj.String(o).value & "`"
func testArray(o: Object, exp: string) = assert $obj.Array(o) == exp, "expected: `" & $exp & "` but got: `" & $obj.Array(o) & "`"
func testHash(o: Object, exp: string) = assert $obj.Hash(o) == exp, "expected: `" & $exp & "` but got: `" & $obj.Hash(o) & "`"

suite "evaluator.nim statement":
    const tests = {
        "Eval INTEGER": (
            @[
                "5",
                "10",
                "-5",
                "-10",
                "5 + 5 + 5 + 5 - 10",
                "2 * 2 * 2 * 2 * 2",
                "-50 + 100 + -50",
                "5 * 2 + 10",
                "5 + 2 * 10",
                "20 + 2 * -10",
                "50 / 2 * 2 + 10",
                "2 * (5 + 10)",
                "3 * 3 * 3 + 10",
                "3 * (3 * 3) + 10",
                "(5 + 10 * 2 + 15 / 3) * 2 + -10"
            ],
            proc (o: obj.Object, idx: int) = 
                case idx:
                of 0: testInteger(o, 5)
                of 1: testInteger(o, 10)
                of 2: testInteger(o, -5)
                of 3: testInteger(o, -10)
                of 4: testInteger(o, 10)
                of 5: testInteger(o, 32)
                of 6: testInteger(o, 0)
                of 7: testInteger(o, 20)
                of 8: testInteger(o, 25)
                of 9: testInteger(o, 0)
                of 10: testInteger(o, 60)
                of 11: testInteger(o, 30)
                of 12: testInteger(o, 37)
                of 13: testInteger(o, 37)
                of 14: testInteger(o, 50)
                else: raise
        ),
        "Eval BOOLEAN": (
            @[
                "true", 
                "false",
                "true == true",
                "false == false",
                "true == false",
                "true != false",
                "false != true",
                "(1 < 2) == true",
                "(1 < 2) == false",
                "(1 > 2) == true",
                "(1 > 2) == false",
            ],
            proc (o: obj.Object, idx: int) =
                case idx:
                of 0: testBoolean(o, true)
                of 1: testBoolean(o, false)
                of 2: testBoolean(o, true)
                of 3: testBoolean(o, true)
                of 4: testBoolean(o, false)
                of 5: testBoolean(o, true)
                of 6: testBoolean(o, true)
                of 7: testBoolean(o, true)
                of 8: testBoolean(o, false)
                of 9: testBoolean(o, false)
                of 10: testBoolean(o, true)
                else: raise
        ),
        "Eval BANG BOOLEAN": (
            @["!true", "!false", "!5", "!!true", "!!false", "!!5"],
            proc (o: obj.Object, idx: int) =
                case idx:
                of 0: testBoolean(o, false)
                of 1: testBoolean(o, true)
                of 2: testBoolean(o, false)
                of 3: testBoolean(o, true)
                of 4: testBoolean(o, false)
                of 5: testBoolean(o, true)
                else: raise
        ),
        "Eval IF": (
            @[
                "if (true) { 10 }",
                "if (false) { 10 }",
                "if (1) { 10 }",
                "if (1 < 2) { 10 }",
                "if (1 > 2) { 10 }",
                "if (1 > 2) { 10 } else { 20 }",
                "if (1 < 2) { 10 } else { 10 }",
            ],
            proc (o: obj.Object, idx: int) {.noSideEffect, gcsafe, locks: 0.} =
                case idx:
                of 0: testInteger(o, 10)
                of 1: testNull(o)
                of 2: testInteger(o, 10)
                of 3: testInteger(o, 10)
                of 4: testNull(o)
                of 5: testInteger(o, 20)
                of 6: testInteger(o, 10)
                else: raise
        ),
        "Eval RETURN": (
            @[
                "return 10;",
                "return 10; 9;",
                "return 2 * 5; 9;",
                "9; return 2 * 5;",
                """
                if (10 > 1) {
                  if (10 > 1) {
                    return 10;
                  }
                }
                return 1;
                """,
            ],
            proc (o: obj.Object, idx: int) {.noSideEffect, gcsafe, locks: 0.} =
                case idx:
                of 0: testReturn(o, obj.Integer(value: 10))
                of 1: testReturn(o, obj.Integer(value: 10))
                of 2: testReturn(o, obj.Integer(value: 10))
                of 3: testReturn(o, obj.Integer(value: 10))
                of 4: testReturn(o, obj.Integer(value: 10))
                else: raise
        ),
        "Eval ERROR": (
            @[
                "5 + true",
                "5 + true; 5",
                "-true",
                "true + false",
                "5; true + false; 5",
                "if (10 > 1) { true + false; }",
                """
                if (10 > 1) {
                  if (10 > 1) {
                    return true + false;
                  }
                }
                return 1;
                """,
                "\"Hello\" - \"Hello\"",
                "{\"name\": \"Monkey\"}[fn(x) { x }]",
            ],
            proc (o: obj.Object, idx: int) {.noSideEffect, gcsafe, locks: 0.} =
                case idx:
                of 0: testError(o, "type mismatch: INTEGER + BOOLEAN")
                of 1: testError(o, "type mismatch: INTEGER + BOOLEAN")
                of 2: testError(o, "not integer: -BOOLEAN")
                of 3: testError(o, "unknown operator: BOOLEAN + BOOLEAN")
                of 4: testError(o, "unknown operator: BOOLEAN + BOOLEAN")
                of 5: testError(o, "unknown operator: BOOLEAN + BOOLEAN")
                of 6: testError(o, "unknown operator: BOOLEAN + BOOLEAN")
                of 7: testError(o, "unknown operator: STRING - STRING")
                of 8: testError(o, "unusable as hash key FUNCTION")
                else: raise
        ),
        "Eval FUNCTION": (
            @[
                "fn(x) { x + 2 };",
            ],
            proc (o: obj.Object, idx: int) =
                case idx:
                of 0: testFunction(o, "fn(x){ (x + 2) }")
                else: raise
        ),
        "Eval FUNCTION LITERAL": (
            @[
                "let identity = fn(x) { x; }; identity(5);",
                "let identity = fn(x) { return x; }; identity(5);",
                "let double = fn(x) { return x * 2; }; double(5);",
                "let add = fn(x, y) { return x + y; }; add(5, 5);",
                "let add = fn(x, y) { return x + y; }; add(5 + 5, add(5, 5));",
                "fn(x) { x }(5)",
            ],
            proc (o: obj.Object, idx: int) {.noSideEffect, gcsafe, locks: 0.} =
                case idx:
                of 0: testInteger(o, 5)
                of 1: testInteger(o, 5)
                of 2: testInteger(o, 10)
                of 3: testInteger(o, 10)
                of 4: testInteger(o, 20)
                of 5: testInteger(o, 5)
                else: raise
        ),
        "Eval STRING LITERAL": (
            @[
                "\"HELLO WORLD!\"",
            ],
            proc (o: obj.Object, idx: int) {.noSideEffect, gcsafe, locks: 0.} =
                case idx:
                of 0: testString(o, "HELLO WORLD!")
                else: raise
        ),
        "Eval BUILTIN FUNCTIONS": (
            @[
                "len(\"\")",
                "len(\"four\")",
                "len(\"hello world\")",
                "len(1)",
                "len(\"one\", \"two\")",
            ],
            proc (o: obj.Object, idx: int) {.noSideEffect, gcsafe, locks: 0.} =
                case idx:
                of 0: testInteger(o, 0)
                of 1: testInteger(o, 4)
                of 2: testInteger(o, 11)
                of 3: testError(o, "argument to `len` not supported, got INTEGER")
                of 4: testError(o, "wrong number of arguments, get=2, want=1")
                else: raise
        ),
        "Eval ARRAY LITERAL": (
            @[
                "[1, 2 * 2, 3 + 3]",
                "[1, 2, 3][0]",
                "[1, 2, 3][1]",
                "[1, 2, 3][2]",
                "let i = 0; [1][i]",
                "[1, 2, 3][1 + 1]",
                "let myArray = [1, 2, 3]; myArray[2];",
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]",
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                "[1, 2, 3][3]",
                "[1, 2, 3][-1]",
                """
                let map = fn(arr, f) {
                    let iter = fn(arr, accumulated) {
                        if (len(arr) == 0) {
                            accumulated
                        } else {
                            iter(rest(arr), push(accumulated, f(first(arr))));
                        }
                    };

                    iter(arr, []);
                };

                let a = [1, 2, 3, 4];
                let double = fn(x) { x * 2 };
                map(a, double);
                """,
                """
                let reduce = fn(arr, initial, f) {
                    let iter = fn(arr, result) {
                        if (len(arr) == 0) {
                            result
                        } else {
                            iter(rest(arr), f(result, first(arr)));
                        }
                    };

                    iter(arr, initial);
                };

                let sum = fn(arr) {
                    reduce(arr, 0, fn(initial, el) { initial + el });
                };

                sum([1, 2, 3, 4, 5]);
                """,
            ],
            proc (o: obj.Object, idx: int) {.noSideEffect, gcsafe, locks: 0.} =
                case idx:
                of 0: 
                    let arr = obj.Array(o)
                    testInteger(arr.elements[0], 1)
                    testInteger(arr.elements[1], 4)
                    testInteger(arr.elements[2], 6)
                of 1: testInteger(o, 1)
                of 2: testInteger(o, 2)
                of 3: testInteger(o, 3)
                of 4: testInteger(o, 1)
                of 5: testInteger(o, 3)
                of 6: testInteger(o, 3)
                of 7: testInteger(o, 6)
                of 8: testInteger(o, 2)
                of 9: testNull(o)
                of 10: testNull(o)
                of 11: testArray(o, "[2, 4, 6, 8]")
                of 12: testInteger(o, 15)
                else: raise
        ),
        "Eval HASH INDEX": (
            @[
                "{\"foo\": 5}[\"foo\"]",
                """
                let two = "two";
                {
                    "one": 10 - 9,
                    two: 1 + 1,
                    "thr" + "ee": 6 / 2,
                    4: 4,
                    true: 5,
                    false: 6,
                }
                """,
                "{\"foo\": 5}[\"foo\"]",
                "{\"foo\": 5}[\"bar\"]",
                "let key = \"foo\"; {\"foo\": 5}[key]",
                "{}[\"foo\"]",
                "{5: 5}[5]",
                "{true: 5}[true]",
                "{false: 5}[false]",
            ],
            proc (o: obj.Object, idx: int) =
                case idx:
                of 0: testInteger(o, 5)
                of 1: 
                    let expected = {
                        obj.String(value: "one").hashKey   : 1,
                        obj.String(value: "two").hashKey   : 2,
                        obj.String(value: "three").hashKey : 3,
                        obj.Integer(value: 4).hashKey      : 4,
                        obj.Boolean(value: true).hashKey   : 5,
                        obj.Boolean(value: false).hashKey  : 6,
                    }.toTable
                    for k, v in expected:
                        let pair = obj.Hash(o).pairs[k]
                        testInteger(pair.value, v)
                of 2: testInteger(o, 5)
                of 3: testNull(o)
                of 4: testInteger(o, 5)
                of 5: testNull(o)
                of 6: testInteger(o, 5)
                of 7: testInteger(o, 5)
                of 8: testInteger(o, 5)
                else: raise
        ),
    }
    for t in tests:
        let title = t[0]
        let values = t[1]
        test title:
            for idx, value in values[0]:
                let evaluated = testEval(value)
                values[1](evaluated, idx)