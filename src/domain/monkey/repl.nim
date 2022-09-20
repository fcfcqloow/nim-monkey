import options
import std/sequtils
import strformat

import ast
import evaluator
import obj

from lexer import newLexer
from parser import newParser

const PROMPT = ">> ";

proc printParserErrors(errors: seq[ast.AstException]): seq[string] = toSeq(errors).map(proc (err: ast.AstException): string = fmt"\t{err.msg}\n")
    
proc repl*(reader: File, writer: File) =
    let env = obj.newEnviroment()
    while true:
        echo ""
        writer.write(PROMPT)
        let scanned = reader.readLine()
        let lexer = newLexer(scanned)
        let parser = newParser(lexer)
        let program = parser.parseProgram().get()
        if parser.errors().len != 0:
            for err in printParserErrors(parser.errors()):
                writer.write $err
            continue
        let evaluated = evaluator.eval(ast.Node(program), env)
        writer.write evaluated.inspect()
        