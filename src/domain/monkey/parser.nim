import options
import strformat
import strutils
import std/tables

import aop/log
import lexer, token, ast

func toExpression(v: Option[ast.Expression]): ast.Expression = return if v.isSome(): v.get() else: ast.Expression()
proc trace(msg: string): string =
    debug(fmt"BEGIN {msg}", 1)
    return msg
proc untrace(msg: string) =
    debug(fmt"END {msg}", -1)

type Priority* {.pure.} = enum
    _
    LOWEST
    EQUALS
    LESSGREATER
    SUM
    PRODUCT
    PREFIX
    CALL
    INDEX

const precedences = {
    token.TokenType.LPAREN:   Priority.CALL,
    token.TokenType.EQ:       Priority.EQUALS,
    token.TokenType.NOT_EQ:   Priority.EQUALS,
    token.TokenType.LT:       Priority.LESSGREATER,
    token.TokenType.GT:       Priority.LESSGREATER,
    token.TokenType.PLUS:     Priority.SUM,
    token.TokenType.MINUS:    Priority.SUM,
    token.TokenType.SLASH:    Priority.PRODUCT,
    token.TokenType.ASTERISC: Priority.PRODUCT,
    token.TokenType.LBRACKET: Priority.INDEX,
}.toTable

type
    prefixParseFn = proc(): ast.Expression
    infixParseFn  = proc(exp: ast.Expression): ast.Expression
    IParser*      = tuple
        parseProgram : proc(): Option[ast.Program]
        errors:        proc(): seq[ast.AstException]
    Parser = ref object
        lexer:          lexer.ILexer
        curToken:       token.Token
        peekToken:      token.Token
        errs:           seq[ast.AstException]
        prefixParseFns: Table[token.TokenType, prefixParseFn]
        infixParseFns:  Table[token.TokenType, infixParseFn]

proc nextToken(self: Parser) 
proc expectPeek(self: Parser, t: token.TokenType): bool 
proc curPrecedence(self: Parser): Priority
proc peekPrecedence(self: Parser): Priority
proc peekError(self: Parser, t: token.TokenType)
proc noPrefixFnError(self: Parser, t: token.TokenType)
proc parseLetStatement(self: Parser): Option[ast.Statement]
proc parseReturnStatement(self: Parser): Option[ast.Statement]
proc parseExpressionStatement(self: Parser): Option[ast.Statement]
proc parseStatement(self: Parser): Option[ast.Statement] 
proc parseProgram(self: Parser): Option[ast.Program] 
proc parseExpression(self: Parser, priority: Priority): Option[ast.Expression]
proc parseGroupedExpression(self: Parser): Option[ast.Expression]
proc parseIdentifier(self: Parser): ast.Expression 
proc parseIntegerliteral(self: Parser): ast.Expression
proc parsePrefixExpression(self: Parser): ast.Expression
proc parseInfixExpression(self: Parser, left: Option[ast.Expression]): ast.Expression
proc parseBoolean(self: Parser): ast.Expression
proc parseIfExpression(self: Parser): Option[ast.Expression]
proc parseBlockStatement(self: Parser): Option[ast.BlockStatement]
proc parseFunctionParameters(self: Parser): seq[Option[ast.Identifier]]
proc parseFunctionLiteral(self: Parser): Option[ast.Expression]
proc parseCallExpression(self: Parser, fun: ast.Expression): ast.Expression
proc parseString(self: Parser): ast.Expression
proc parseArrayLiteral(self: Parser): ast.Expression
proc parseExpressionList(self: Parser, endToken: token.TokenType): seq[ast.Expression]
proc parseIndexExpression(self: Parser, left: ast.Expression): Option[ast.Expression]
proc parseHashLiteral(self: Parser):  Option[ast.Expression]

func curTokenIs(self: Parser, t: token.TokenType): bool  = self.curToken.typ == t
func peekTokenIs(self: Parser, t: token.TokenType): bool =  self.peekToken.typ == t

proc newParser*(lexer: lexer.ILexer): IParser = 
    let parser = Parser(
        lexer : lexer,
        errs  : @[],
    )

    parser.nextToken()
    parser.nextToken()

    proc registerPrefix(t: token.TokenType, fn: prefixParseFn) = parser.prefixParseFns[t] = fn
    registerPrefix(token.TokenType.IDENT,  proc(): ast.Expression = parser.parseIdentifier())
    registerPrefix(token.TokenType.INT, proc(): ast.Expression = parser.parseIntegerliteral())
    registerPrefix(token.TokenType.BANG, proc(): ast.Expression = parser.parsePrefixExpression())
    registerPrefix(token.TokenType.MINUS, proc(): ast.Expression = parser.parsePrefixExpression())
    registerPrefix(token.TokenType.TRUE, proc(): ast.Expression = parser.parseBoolean())
    registerPrefix(token.TokenType.FALSE, proc(): ast.Expression = parser.parseBoolean())
    registerPrefix(token.TokenType.LPAREN, proc(): ast.Expression = toExpression(parser.parseGroupedExpression()))
    registerPrefix(token.TokenType.IF, proc(): ast.Expression = toExpression(parser.parseIfExpression()))
    registerPrefix(token.TokenType.FUNCTION, proc(): ast.Expression = toExpression(parser.parseFunctionLiteral()))
    registerPrefix(token.TokenType.STRING, proc(): ast.Expression = parser.parseString())
    registerPrefix(token.TokenType.LBRACKET, proc(): ast.Expression = parser.parseArrayLiteral())
    registerPrefix(token.TokenType.LBRACE, proc(): ast.Expression = toExpression(parser.parseHashLiteral()))

    proc registerInfix(t: token.TokenType, fn: infixParseFn) = parser.infixParseFns[t] = fn
    registerInfix(token.TokenType.PLUS, proc(e: ast.Expression): ast.Expression = parser.parseInfixExpression(option(e)))
    registerInfix(token.TokenType.MINUS, proc(e: ast.Expression): ast.Expression = parser.parseInfixExpression(option(e)))
    registerInfix(token.TokenType.SLASH, proc(e: ast.Expression): ast.Expression = parser.parseInfixExpression(option(e)))
    registerInfix(token.TokenType.ASTERISC, proc(e: ast.Expression): ast.Expression = parser.parseInfixExpression(option(e)))
    registerInfix(token.TokenType.EQ, proc(e: ast.Expression): ast.Expression = parser.parseInfixExpression(option(e)))
    registerInfix(token.TokenType.NOT_EQ, proc(e: ast.Expression): ast.Expression = parser.parseInfixExpression(option(e)))
    registerInfix(token.TokenType.LT, proc(e: ast.Expression): ast.Expression = parser.parseInfixExpression(option(e)))
    registerInfix(token.TokenType.GT, proc(e: ast.Expression): ast.Expression = parser.parseInfixExpression(option(e)))
    registerInfix(token.TokenType.LPAREN, proc(e: ast.Expression): ast.Expression = parser.parseCallExpression(e))
    registerInfix(token.TokenType.LBRACKET, proc(e: ast.Expression): ast.Expression = toExpression(parser.parseIndexExpression(e)))

    return (
        parseProgram: proc(): Option[ast.Program] = parser.parseProgram(),
        errors: proc():       seq[ast.AstException] =  parser.errs
    );

proc nextToken(self: Parser) = 
    self.curToken  = self.peekToken
    self.peekToken = self.lexer.nextToken()

proc expectPeek(self: Parser, t: token.TokenType): bool =
    if self.peekTokenIs(t):
        self.nextToken()
        return true
    else:
        debug("self.peekError" & $t)
        self.peekError(t)
        return false

proc curPrecedence(self: Parser): Priority = return if precedences.hasKey(self.curToken.typ): precedences[self.curToken.typ] else: Priority.LOWEST
proc peekPrecedence(self: Parser): Priority = return if precedences.hasKey(self.peekToken.typ): precedences[self.peekToken.typ] else: Priority.LOWEST
proc peekError(self: Parser, t: token.TokenType) = self.errs.add(ast.AstException(msg: fmt"expected next token to be {t} got {self.peekToken.typ} instead"))
proc noPrefixFnError(self: Parser, t: token.TokenType) = self.errs.add(ast.AstException(msg: fmt"no prefix parse function for {$t} found"))

proc parseProgram(self: Parser): Option[ast.Program] = 
    let program = ast.Program(statements:  @[])
    while not self.curTokenIs token.TokenType.EOF:
        debug("parseProgram curToken: " & $self.curToken)
        let smt = self.parseStatement()
        if not smt.isNone():
            program.statements.add(smt.get())
            debug("add statements: " & $smt.get())
        self.nextToken()
    return option(program)

proc parseStatement(self: Parser): Option[ast.Statement] = 
    return case self.curToken.typ:
    of token.TokenType.LET:    self.parseLetStatement()
    of token.TokenType.RETURN: self.parseReturnStatement()
    else:                      self.parseExpressionStatement()

proc parseLetStatement(self: Parser): Option[ast.Statement] =
    let msg = trace("parseLetStatement")
    defer: untrace(msg & ": " & $result)

    let letStmt = ast.LetStatement(token: self.curToken)

    if not self.expectPeek(token.TokenType.IDENT):
        return none[ast.Statement]()
    
    letStmt.name = ast.Identifier(token: self.curToken, value: self.curToken.literal)

    if not self.expectPeek(token.TokenType.ASSIGN):
        return none[ast.Statement]()

    self.nextToken()

    letStmt.value = self.parseExpression(Priority.LOWEST)
    if self.peekTokenIs(token.TokenType.SEMICOLON):
        self.nextToken()

    return option(ast.Statement(letStmt))

proc parseReturnStatement(self: Parser): Option[ast.Statement] =
    let msg = trace("parseReturnStatement")
    defer: untrace(msg & ": " & $result)

    let returnStmt = ast.ReturnStatement(token: self.curToken)
    self.nextToken()

    returnStmt.value = self.parseExpression(Priority.LOWEST)
    if self.peekTokenIs(token.TokenType.SEMICOLON):
        self.nextToken()

    return option(ast.Statement(returnStmt))

proc parseExpressionStatement(self: Parser): Option[ast.Statement] =
    let msg = trace("parseExpressionStatement")
    defer: untrace(msg & ": " & $result)

    let smt = ast.ExpressionStatement(token: self.curToken)
    smt.expression = self.parseExpression(Priority.LOWEST)
    if self.peekTokenIs token.TokenType.SEMICOLON:
        self.nextToken()
    return option(ast.Statement(smt))

proc parseExpression(self: Parser, priority: Priority): Option[ast.Expression] =
    let msg = trace("parseExpression")
    defer: untrace(msg & ": " & $result)

    if not self.prefixParseFns.hasKey(self.curToken.typ): 
        self.noPrefixFnError(self.curToken.typ)
        return none[ast.Expression]()

    let prefix = self.prefixParseFns[self.curToken.typ]
    var leftExp = prefix()
    while not self.peekTokenIs(token.TokenType.SEMICOLON) and ord(priority) < ord(self.peekPrecedence()):
        if not self.infixParseFns.hasKey(self.peekToken.typ):
            return option(leftExp)
        let infix = self.infixParseFns[self.peekToken.typ]
        self.nextToken()
        leftExp = infix(leftExp)

    return option(leftExp)

proc parseIdentifier(self: Parser): ast.Expression =
    let msg = trace("parseIdentifier")
    defer: untrace(msg & ": " & $result)
    return ast.Identifier(token: self.curToken, value: self.curToken.literal)

proc parseIntegerliteral(self: Parser): ast.Expression = 
    let msg = trace("parseIntegerliteral")
    defer: untrace(msg & ": " & $result)
    return ast.IntegerLiteral(value: parseInt(self.curToken.literal), token: self.curToken)

proc parsePrefixExpression(self: Parser): ast.Expression = 
    let msg = trace("parsePrefixExpression")
    defer: untrace(msg & ": " & $result)

    let expression = ast.PrefixExpression(
        token: self.curToken, 
        operator: self.curToken.literal
    )
    self.nextToken()
    expression.right = self.parseExpression(Priority.PREFIX)

    return expression

proc parseInfixExpression(self: Parser, left: Option[ast.Expression]): ast.Expression =
    let msg = trace("parseInfixExpression")
    defer: untrace(msg & ": " & $result)

    let expressoion = ast.InfixExpression(
        token:    self.curToken, 
        operator: self.curToken.literal,
        left:     left,
    )
    let precedence = self.curPrecedence()
    self.nextToken()
    expressoion.right = self.parseExpression(precedence)

    return expressoion

proc parseBoolean(self: Parser): ast.Expression =
    let msg = trace("parseBoolean")
    defer: untrace(msg & ": " & $result)
    return ast.Boolean(token: self.curToken, value: self.curTokenIs(token.TokenType.TRUE))

proc parseGroupedExpression(self: Parser): Option[ast.Expression] =
    let msg = trace("parseGroupedExpression")
    defer: untrace(msg  & $result)
    self.nextToken()

    let exp = self.parseExpression(Priority.LOWEST)
    if not self.expectPeek(token.TokenType.RPAREN):
        return none[ast.Expression]()
    
    return exp

proc parseIfExpression(self: Parser): Option[ast.Expression] =
    let msg = trace("parseIfExpression")
    defer: untrace(msg & ": " & $result)
    let expression = ast.IfExpression(token: self.curToken)
    if not self.expectPeek(token.TokenType.LPAREN):
        return none[ast.Expression]()

    self.nextToken()

    expression.condition = toExpression(self.parseExpression(Priority.LOWEST))

    if not self.expectPeek(token.TokenType.RPAREN):
        return none[ast.Expression]()

    if not self.expectPeek(token.TokenType.LBRACE):
        return none[ast.Expression]()

    expression.consequence = self.parseBlockStatement()

    if self.peekTokenIs token.TokenType.ELSE:
        self.nextToken()
        if not self.expectPeek token.TokenType.LBRACE:
            return none[ast.Expression]()
        expression.alternative = self.parseBlockStatement()
    
    return option(ast.Expression(expression))

proc parseBlockStatement(self: Parser): Option[ast.BlockStatement] =
    let msg = trace("parseBlockStatement")
    defer: untrace(msg & ": " & $result)

    let blk  = ast.BlockStatement(token: self.curToken, statements: @[])

    self.nextToken()
    while not self.curTokenIs(token.TokenType.RBRACE) and not self.curTokenIs(token.TokenType.EOF):
        let smt = self.parseStatement()
        if not smt.isNone(): blk.statements.add(smt.get())
        self.nextToken()
    
    return option(blk)

proc parseFunctionLiteral(self: Parser): Option[ast.Expression] =
    let msg = trace("parseFunctionLiteral")
    defer: untrace(msg & ": " & $result)

    var lit = ast.FunctionLiteral(token: self.curToken)
    if not self.expectPeek(token.TokenType.LPAREN):
        return none[ast.Expression]()

    lit.parameters = self.parseFunctionParameters()
    if not self.expectPeek(token.TokenType.LBRACE):
        return none[ast.Expression]()

    lit.body = self.parseBlockStatement()

    return some(ast.Expression(lit))

proc parseFunctionParameters(self: Parser): seq[Option[ast.Identifier]] =
    let msg = trace("parseFunctionParameters")
    defer: untrace(msg & ": " & $result)
    
    var identifiers: seq[Option[ast.Identifier]] = @[]
    
    if self.peekTokenIs(token.TokenType.RPAREN):
        self.nextToken()
        return identifiers

    self.nextToken()

    let ident = ast.Identifier(token: self.curToken, value: self.curToken.literal)
    identifiers.add(some(ident))

    while self.peekTokenIs(token.TokenType.COMMA):
        self.nextToken()
        self.nextToken()
        let ident = ast.Identifier(token: self.curToken, value: self.curToken.literal)
        identifiers.add(some(ident))
    
    if not self.expectPeek(token.TokenType.RPAREN):
        return @[]

    return identifiers

proc parseCallExpression(self: Parser, fun: ast.Expression): ast.Expression =
    let msg = trace("parseCallExpression")
    defer: untrace(msg & ": " & $result)

    let exp = ast.CallExpression(token: self.curToken, function: fun)
    exp.arguments = self.parseExpressionList(token.TokenType.RPAREN)
    return exp

proc parseString(self: Parser): ast.Expression =
    let msg = trace("parseString")
    defer: untrace(msg & ": " & $result)
    return ast.StringLiteral(token: self.curToken, value: self.curToken.literal)

proc parseArrayLiteral(self: Parser): ast.Expression =
    let msg = trace("parseArrayLiteral")
    defer: untrace(msg & ": " & $result)

    let arr = ast.ArrayLiteral(token: self.curToken, elements:self.parseExpressionList(token.TokenType.RBRACKET))
    return arr

proc parseExpressionList(self: Parser, endToken: token.TokenType): seq[ast.Expression] =
    let msg = trace("parseExpressionList")
    defer: untrace(msg & ": " & $result)

    result = @[]
    if self.peekTokenIs(endToken):
        self.nextToken() 
        return result

    self.nextToken()
    result.add(self.parseExpression(Priority.LOWEST).get())

    while self.peekTokenIs(token.TokenType.COMMA):
        self.nextToken()
        self.nextToken()
        result.add(self.parseExpression(Priority.LOWEST).get())
    
    if not self.expectPeek(endToken):
        return @[]

    return result

proc parseIndexExpression(self: Parser, left: ast.Expression): Option[ast.Expression] =
    let msg = trace("parseIndexExpression")
    defer: untrace(msg & ": " & $result)

    let exp = ast.IndexExpression(token: self.curToken, left: left)
    
    self.nextToken()
    exp.index = self.parseExpression(Priority.LOWEST).get()

    if not self.expectPeek(token.TokenType.RBRACKET):
        return none[ast.Expression]()

    return option(ast.Expression(exp))

proc parseHashLiteral(self: Parser):  Option[ast.Expression] =
    let msg = trace("parseHashLiteral")
    defer: untrace(msg & ": " & $result)

    let h = ast.HashLiteral(token: self.curToken)
    h.pairs = initTable[ast.Expression, ast.Expression]()
    
    while not self.peekTokenIs(token.TokenType.RBRACE):
        self.nextToken()
        let key = self.parseExpression(Priority.LOWEST)
        if not self.expectPeek(token.TokenType.COLON):
            return none[ast.Expression]()

        self.nextToken()
        let value = self.parseExpression(Priority.LOWEST)
        
        h.pairs[key.get()] = value.get()

        if not self.peekTokenIs(token.TokenType.RBRACE) and not self.expectPeek(token.TokenType.COMMA):
            return none[ast.Expression]()

    if not self.expectPeek(token.TokenType.RBRACE):
        return none[ast.Expression]()

    return option(ast.Expression(h))
