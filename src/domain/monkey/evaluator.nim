import options
import strformat
import segfaults
import tables

import aop/log
import ast
import obj

let 
    NULL*  = obj.Object(obj.Null())
    TRUE*  = obj.Object(obj.Boolean(value: true))
    FALSE* = obj.Object(obj.Boolean(value: false))

func isError(o: obj.Object): bool = o.typ() == obj.ObjectType.ERROR_OBJ
func isSame*(a: obj.Object, b: obj.Object): bool =
    if a.typ != b.typ: return false
    return case a.typ:
    of obj.ObjectType.INTERGER_OBJ: obj.Integer(a).value == obj.Integer(b).value
    of obj.ObjectType.BOOLEAN_OBJ:  a == b
    of obj.ObjectType.NULL_OBJ:     true
    else: raise

proc isTruthy(o: obj.Object): bool = 
    return if isSame(o, NULL): false
    elif isSame(o, TRUE):      true
    elif isSame(o, FALSE):     false
    else:                      true

proc nativeBoolToBooleanObject(v: bool): obj.Object =
    return if v: TRUE else: FALSE

let builtin* = {
    "len" : obj.Builtin(
        fn: proc(objects: varargs[obj.Object]): obj.Object =
            return if len(objects) != 1:
                obj.Error(message: fmt"wrong number of arguments, get={len(objects)}, want=1")
            elif objects[0] of obj.String:
                obj.Integer(value: obj.String(objects[0]).value.len)
            elif objects[0] of obj.Array:
                obj.Integer(value: obj.Array(objects[0]).elements.len)
            else: obj.Error(message: fmt"argument to `len` not supported, got {objects[0].typ()}")
    ),
    "first": obj.Builtin(
        fn: proc(objects: varargs[obj.Object]): obj.Object =
            if objects.len != 1:
                return Error(message: fmt"first: wrong number of arguments got={objects.len}, want=1")
            elif objects[0].typ != obj.ObjectType.ARRAY_OBJ:
                return Error(message: fmt"argument to `first` must be array, got {objects[0].typ}")
            
            let arr = obj.Array(objects[0])
            return if arr.elements.len > 0: arr.elements[0] else: NULL
    ),
    "last": obj.Builtin(
        fn: proc(objects: varargs[obj.Object]): obj.Object =
            if objects.len != 1:
                return Error(message: fmt"last: wrong number of arguments got={objects.len}, want=1")
            elif objects[0].typ != obj.ObjectType.ARRAY_OBJ:
                return Error(message: fmt"argument to `last` must be array, got {objects[0].typ}")
            
            let arr = obj.Array(objects[0])
            return if arr.elements.len > 0: arr.elements[arr.elements.len - 1] else: NULL
    ),
    "rest": obj.Builtin(
        fn: proc(objects: varargs[obj.Object]): obj.Object =
            if objects.len != 1:
                return Error(message: fmt"rest: wrong number of arguments got={objects.len}, want=1")
            elif objects[0].typ != obj.ObjectType.ARRAY_OBJ:
                return Error(message: fmt"argument to `rest` must be array, got {objects[0].typ}")
            
            let arr = obj.Array(objects[0])
            return obj.Array(elements: arr.elements[1..arr.elements.len-1])
    ),
    "push": obj.Builtin(
        fn: proc(objects: varargs[obj.Object]): obj.Object =
            if objects.len != 2:
                return Error(message: fmt"push: wrong number of arguments got={objects.len}, want=2")
            elif objects[0].typ != obj.ObjectType.ARRAY_OBJ:
                return Error(message: fmt"argument to `push` must be array, got {objects[0].typ}")

            let arr = obj.Array(objects[0])
            arr.elements.add(objects[1])

            return arr
    ),
    "puts": obj.Builtin(
        fn: proc(objects: varargs[obj.Object]): obj.Object =
            for o in objects:
                echo o
            return NULL
    ),
}.toTable

proc eval*(node: Node, env: obj.Environment): obj.Object
proc evalProgram(program: ast.Program, env: obj.Environment): obj.Object
proc evalBlockStatement(blk: ast.BlockStatement, env: obj.Environment): obj.Object
proc evalPrefixExpression(operator: string, right: obj.Object, env: obj.Environment): obj.Object
proc evalBangOperatorExpression(right: obj.Object, env: obj.Environment): obj.Object
proc evalMinusPrefixOperatorExpression(operator: string, right: obj.Object, env: obj.Environment): obj.Object
proc evalIntegerInfixExpression(left: obj.Object, operator: string, right: obj.Object, env: obj.Environment): obj.Object
proc evalInfixExpression(left: obj.Object, operator: string, right: obj.Object, env: obj.Environment): obj.Object
proc evalIfExpression(ie: ast.IfExpression, env: obj.Environment): obj.Object
proc evalIdentifier(node: ast.Identifier, env: obj.Environment): obj.Object
proc evalExpressions(exps: seq[ast.Expression], env: obj.Environment): seq[obj.Object]
proc extendFunctionEnv(fn: obj.Function, args: seq[obj.Object]): obj.Environment
proc unwraoReturnValue(o: obj.Object): obj.Object
proc applyFunction(fn: obj.Object, args: seq[obj.Object]): obj.Object
proc evalStringInfixExpression(operator: string, left, right: obj.Object): obj.Object
proc evalArrayIndexExpression(arr: obj.Object, index: obj.Object): obj.Object
proc evalIndexExpression(left: obj.Object, index: obj.Object): obj.Object
proc evalHashLiteral(node: ast.HashLiteral, env: obj.Environment): obj.Object
proc evalHashIndexExpression(h: obj.Object, index: obj.Object): obj.Object
proc eval*(node: Node, env: obj.Environment): obj.Object =
    var debugInstance = ""
    let debugType = proc (typ: string) = 
        debugInstance = typ
        debug("BEGIN: " & $node & " is " & typ, 1)
    defer: debug("DONE: " & debugInstance & ": " & $result, -1)

    if node of ast.Program:
        debugType("ast.Program")
        return evalProgram(ast.Program(node), env)

    elif node of ast.FunctionLiteral:
        debugType("ast.FunctionLiteral")
        let fnLiteral = ast.FunctionLiteral(node)
        return obj.Function(parameters: fnLiteral.parameters, body: fnLiteral.body, env: option(env))
    
    elif node of ast.CallExpression:
        debugType("ast.CallExpression")
        let fn = eval(ast.CallExpression(node).function, env)
        if isError(fn): 
            return fn
        let args = evalExpressions(ast.CallExpression(node).arguments, env)
        if len(args) == 1 and isError(args[0]):
            return args[0]

        return applyFunction(fn, args)

    elif node of ast.ReturnStatement:
        debugType("ast.ReturnStatement")
        let val = eval(ast.ReturnStatement(node).value.get(), env)
        if isError(val): return val
        if isError(val): return val
        return obj.Return(value: val)

    elif node of ast.LetStatement:
        debugType("ast.LetStatement")
        let letNode = ast.LetStatement(node);
        let val = eval(letNode.value.get(), env)
        if isError(val): return val
        discard env.put(letNode.name.value, val)
        return  val

    elif node of ast.Identifier:
        debugType("ast.Identifier")
        return evalIdentifier(ast.Identifier(node), env)

    elif node of ast.BlockStatement:
        debugType("ast.BlockStatement")
        return evalBlockStatement(ast.BlockStatement(node), env)

    elif node of ast.IfExpression:
        debugType("ast.IfExpression")
        return evalIfExpression(ast.IfExpression(node), env)

    elif node of ast.ExpressionStatement:
        debugType("ast.ExpressionStatement")
        return eval(ast.ExpressionStatement(node).expression.get(), env)

    elif node of ast.PrefixExpression:
        debugType("ast.PrefixExpression")
        let prefix = ast.PrefixExpression(node)
        let right = eval(prefix.right.get(), env)
        if isError(right): return right
        return evalPrefixExpression(prefix.operator, right, env)

    elif node of ast.InfixExpression:
        debugType("ast.InfixExpression")
        let infix = ast.InfixExpression(node)
        let left = eval(infix.left.get(), env)
        if isError(left): return left
        let right = eval(infix.right.get(), env)
        if isError(right): return right
        return evalInfixExpression(left, infix.operator, right, env)

    elif node of ast.IntegerLiteral:
        debugType("ast.IntegerLiteral")
        let res = obj.Integer(value: ast.IntegerLiteral(node).value)
        return obj.Object(res)

    elif node of ast.Boolean:
        debugType("ast.Boolean")
        return nativeBoolToBooleanObject(ast.Boolean(node).value)
    
    elif node of ast.StringLiteral:
        debugType("ast.StringLiteral")
        return obj.String(value: ast.StringLiteral(node).value)

    elif node of ast.ArrayLiteral:
        debugType("ast.ArrayLiteral")
        let elements = evalExpressions(ast.ArrayLiteral(node).elements, env)
        return if elements.len == 1 and isError(elements[0]):
            elements[0]
        else:
            obj.Array(elements: elements)
    
    elif node of ast.IndexExpression:
        debugType("ast.IndexExpression")
        let nodeIndex = ast.IndexExpression(node)
        let left = eval(nodeIndex.left, env)
        if isError(left):
            return left
            
        let index = eval(nodeIndex.index, env)
        if isError(index):
            return index

        return evalIndexExpression(left, index)
    
    elif node of ast.HashLiteral:
        debugType("ast.HashLiteral")
        return evalHashLiteral(ast.HashLiteral(node), env)
    
    else:
        debug($node & " is unknown")
        return obj.Object()

proc evalProgram(program: ast.Program, env: obj.Environment): obj.Object =
    for statement in program.statements:
        result = eval(statement, env)
        if result of obj.Return:
            return obj.Return(result).value
        elif result of obj.Error:
            return result

proc evalBlockStatement(blk: ast.BlockStatement, env: obj.Environment): obj.Object =
    for statement in blk.statements:
        result = eval(statement, env)
        let typ = result.typ()
        if typ == obj.ObjectType.RETURN_OBJ or typ == obj.ObjectType.ERROR_OBJ:
            return result

proc evalPrefixExpression(operator: string, right: obj.Object, env: obj.Environment): obj.Object =
    return case operator
        of "!": evalBangOperatorExpression(right, env)
        of "-": evalMinusPrefixOperatorExpression(operator, right, env)
        else:   obj.Error(message: fmt"unknown operator: {operator} {right.typ()}")

proc evalMinusPrefixOperatorExpression(operator: string, right: obj.Object, env: obj.Environment): obj.Object =
    if right.typ() != obj.ObjectType.INTERGER_OBJ:
        return obj.Error(message: fmt"not integer: -{right.typ()}")
    let value = obj.Integer(right).value
    return obj.Object(obj.Integer(value: -value))

proc evalBangOperatorExpression(right: obj.Object, env: obj.Environment): obj.Object =
    return
        if right == TRUE:    FALSE
        elif right == FALSE: TRUE
        elif right == NULL:  TRUE
        else:                FALSE

proc evalInfixExpression(left: obj.Object, operator: string, right: obj.Object, env: obj.Environment): obj.Object =
    return if left.typ() != right.typ():
        obj.Error(message: fmt"type mismatch: {left.typ()} {operator} {right.typ()}")
    elif left.typ() == obj.ObjectType.STRING_OBJ and right.typ() == obj.ObjectType.STRING_OBJ:
        evalStringInfixExpression(operator, left, right)
    elif left.typ() == obj.ObjectType.INTERGER_OBJ and right.typ() == obj.ObjectType.INTERGER_OBJ:
        evalIntegerInfixExpression(left, operator, right, env)
    elif operator == "==":
        nativeBoolToBooleanObject(isSame(left, right))
    elif operator == "!=":
        nativeBoolToBooleanObject(not isSame(left, right))
    else: obj.Error(message: fmt"unknown operator: {left.typ()} {operator} {right.typ()}")

proc evalIntegerInfixExpression(left: obj.Object, operator: string, right: obj.Object, env: obj.Environment): obj.Object =
    let left = obj.Integer(left).value
    let right = obj.Integer(right).value
    return case operator
    of "+":  obj.Object(obj.Integer(value: left + right))
    of "-":  obj.Object(obj.Integer(value: left - right))
    of "*":  obj.Object(obj.Integer(value: left * right))
    of "/":  obj.Object(obj.Integer(value: left div right))
    of "<":  nativeBoolToBooleanObject(left < right)
    of ">":  nativeBoolToBooleanObject(left > right)
    of "==": nativeBoolToBooleanObject(left == right)
    of "!=": nativeBoolToBooleanObject(left != right)
    else:    NULL

proc evalIfExpression(ie: ast.IfExpression, env: obj.Environment): obj.Object =
    let condition = eval(ie.condition, env)
    return if isError(condition): condition
    elif isTruthy(condition):     eval(ie.consequence.get(), env)
    elif ie.alternative.isSome(): eval(ie.alternative.get(), env)
    else:                         NULL

proc evalIdentifier(node: ast.Identifier, env: obj.Environment): obj.Object =
    let val = env.get(node.value)
    return if val.isSome():
        val.get()
    elif builtin.hasKey(node.value):
        builtin[node.value]
    else:
        obj.Error(message: "identifier not found: " & node.value)

proc evalExpressions(exps: seq[ast.Expression], env: obj.Environment): seq[obj.Object] =
    for e in exps:
        let evaluated = eval(e, env)
        if isError(evaluated):
            return @[evaluated]
        result.add(evaluated)

proc applyFunction(fn: obj.Object, args: seq[obj.Object]): obj.Object =
    return if fn of obj.Function:
        let function = obj.Function(fn)
        let extendedEnv = extendFunctionEnv(function, args)
        let evaluated = eval(function.body.get(), extendedEnv)
        unwraoReturnValue(evaluated)
    elif fn of obj.Builtin:
        obj.Builtin(fn).fn(args)
    else: obj.Error(message: "not a function: " & $fn)

proc extendFunctionEnv(fn: obj.Function, args: seq[obj.Object]): obj.Environment =
    let env = obj.newEnclosedEnviroment(fn.env)
    for idx, param in fn.parameters:
        discard env.put(param.get().value, args[idx])
    return env

proc unwraoReturnValue(o: obj.Object): obj.Object =
    return if o of obj.Return: obj.Return(o).value else: o

proc evalStringInfixExpression(operator: string, left, right: obj.Object): obj.Object =
    if operator != "+":
        return obj.Error(message: "unknown operator: " & $left.typ() & " " & operator & " " & $right.typ())

    let leftVal = obj.String(left).value
    let rightVal = obj.String(right).value
    return obj.String(value: leftVal & rightVal)

proc evalIndexExpression(left: obj.Object, index: obj.Object): obj.Object =
    return if left.typ() == obj.ObjectType.ARRAY_OBJ and index.typ() == obj.ObjectType.INTERGER_OBJ:
        evalArrayIndexExpression(left, index)
    elif left.typ() == obj.ObjectType.HASH_OBJ:
        evalHashIndexExpression(left, index)
    else: Error(message: "index operator not supported: " & $left.typ())

proc evalArrayIndexExpression(arr: obj.Object, index: obj.Object): obj.Object =
    let arrayObj = obj.Array(arr)
    let idx = obj.Integer(index).value
    let max = arrayObj.elements.len - 1

    return if idx < 0 or idx > max:
            NULL 
        else: 
            arrayObj.elements[idx]

proc evalHashLiteral(node: ast.HashLiteral, env: obj.Environment): obj.Object =
    var pairs = initTable[obj.HashKey, obj.HashPair]()
    for keyNode, valueNode in node.pairs:
        let key = eval(keyNode, env)
        if isError(key):
            return key

        if not obj.isHashable(key):
            return Error(message: "unusable as hash key: " & $key)

        let value = eval(valueNode, env)
        if isError(value):
            return value

        let hashed = key.hashKey
        pairs[hashed] = obj.HashPair(key: key, value: value)

    return obj.Hash(pairs: pairs)

proc evalHashIndexExpression(h: obj.Object, index: obj.Object): obj.Object =
    let hashObj = obj.Hash(h)

    if not obj.isHashable(index):
        return Error(message: "unusable as hash key " & $index.typ())

    if not hashObj.pairs.hasKey(index.hashkey):
        return NULL

    return hashObj.pairs[index.hashkey].value
