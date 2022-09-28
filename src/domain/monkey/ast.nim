import strutils
import sequtils
import options
import strformat
import tables
import hashes

import token

func optString(v: Option): string {.noSideEffect.} = return if not v.isNone(): $v.get() else: ""

type AstException* = object of CatchableError

type 
    Node* = ref object of RootObj
        token*: token.Token
    Statement*  = ref object of Node
    Expression* = ref object of Node
    Identifier* = ref object of Expression
        value*: string

method tokenLiteral*(self: Node): string {.base.} =  self.token.literal
method `$`*(self: Node): string {.base.} = self.token.literal
method statementNode(self: Statement): void {.base.} = return
method expressionNode*(self: Expression): void {.base.} = return
proc hash*(self: Expression): Hash = self.token.hash

type
    LetStatement* = ref object of Statement
        name*:  Identifier
        value*: Option[Expression]
    ReturnStatement* = ref object of Statement
        value*: Option[Expression]
    ExpressionStatement* = ref object of Statement
        expression*: Option[Expression]
    BlockStatement* = ref object of Statement
        statements*: seq[Statement]

method `$`*(self: LetStatement): string = fmt"{self.tokenLiteral()} {$self.name} = {optString(self.value)};"
method `$`*(self: ReturnStatement): string = fmt"{self.tokenLiteral()} = {optString(self.value)};"
method `$`*(self: ExpressionStatement): string = optString(self.expression) 
func `$`*(self: BlockStatement): string = toSeq(self.statements).map(proc(s: Statement): string = $s).join("")

type 
    IntegerLiteral* = ref object of Expression
        value*: int64
    Boolean* = ref object of Expression
        value*: bool
    PrefixExpression* = ref object of Expression
        operator*: string
        right*:    Option[Expression]
    InfixExpression* = ref object of Expression
        left*:     Option[Expression]
        operator*: string
        right*:    Option[Expression]
    IfExpression* = ref object of Expression
        condition*:   Expression
        consequence*: Option[BlockStatement]
        alternative*: Option[BlockStatement]
    FunctionLiteral* = ref object of Expression
        parameters*: seq[Option[Identifier]]
        body*:       Option[BlockStatement]
    CallExpression* =  ref object of Expression
        function*:  Expression
        arguments*: seq[Expression]
    StringLiteral* = ref object of Expression
        tokenValue*: token.Token
        value*:      string
    ArrayLiteral* = ref object of Expression
        elements*: seq[Expression]
    IndexExpression* = ref object of Expression
        left*:  Expression
        index*: Expression
    HashLiteral* = ref object of Expression
        pairs*: Table[Expression, Expression]
method `$`*(self: PrefixExpression): string = fmt"({$self.operator}{optString(self.right)})"
method `$`*(self: InfixExpression): string = fmt"({optString(self.left)} {$self.operator} {optString(self.right)})"
func `$`*(self: IfExpression): string = return 
    if self.alternative.isSome(): fmt"if{self.condition} {optString(self.consequence)} else {optString(self.alternative)}"
    else:  fmt"if{self.condition} {optString(self.consequence)}"
func `$`*(self: FunctionLiteral): string = 
    let parameters = self.parameters.map(proc(s: Option[Identifier]): string = optString(s)).join(", ")
    return fmt"{self.tokenLiteral()}({parameters}){optString(self.body)}"
method `$`*(self: CallExpression): string = $self.function & "(" & self.arguments.mapIt($it).join(", ") & ")" 
method `$`*(self: ArrayLiteral): string = "[" & self.elements.mapIt($it).join(", ") & "]"
method `$`*(self: IndexExpression): string = fmt"({self.left}[{self.index}])"
method `$`*(self: HashLiteral): string =
    var strings: seq[string] = @[]
    for k, v in self.pairs:
        strings.add($k & ": " & $v)
    return "{" & strings.join(", ") & "}"

type Program* = ref object of Node
    statements*: seq[Statement]
proc tokenLiteral*(self: Program): string = return if self.statements.len > 0: cast[Node](self.statements[0]).tokenLiteral() else: ""
proc `$`*(self: Program): string = toSeq(self.statements).map(proc(s: Statement): string = $s).join("")
