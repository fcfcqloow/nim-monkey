import sequtils
import strformat
import strutils
import tables
import options
import hashes

import ast
import domain/util/optionutils

type 
    HashKey*             = int
    ObjectType* {.pure.} = enum 
        _ = ""
        INTERGER_OBJ = "INTEGER"
        BOOLEAN_OBJ  = "BOOLEAN"
        NULL_OBJ     = "NULL"
        RETURN_OBJ   = "RETURN"
        ERROR_OBJ    = "ERROR"
        FUNCTION_OBJ = "FUNCTION"
        STRING_OBJ   = "STRING"
        BUILTIN_OBJ  = "BUILTIN"
        ARRAY_OBJ    = "ARRAY"
        HASH_OBJ     = "HASH"
    Object* = ref object of RootObj
method typ*(self: Object): ObjectType {.base.} =  ObjectType._
method inspect*(self: Object): string {.base.} = ""
method `$`*(self: Object): string {.base.} = "Object"
method hashKey*(self: Object): HashKey {.base.} = raise
proc hash*(self: HashKey): Hash = self

type
    HashPair* = ref object
        key*, value*: Object
    Hash* = ref object of Object
        pairs*: Table[HashKey, HashPair]
method typ*(self: Hash): ObjectType = ObjectType.HASH_OBJ
method inspect*(self: Hash): string =
    var strings: seq[string] = @[]
    for k, pairs in self.pairs:
        strings.add(pairs.key.inspect() & ": f" & pairs.value.inspect())
    return "{" & strings.join(", ") & "}"
method `$`*(self: Hash): string = self.inspect
proc `$`*(self: HashPair): string = "<" & $self.key & ": (" & $self.value & ")>"

type 
    BuiltinFunction* = proc (objects: varargs[Object]): Object
    Builtin* = ref object of Object
        fn*: BuiltinFunction
method typ*(self: Builtin): ObjectType =  ObjectType.BUILTIN_OBJ
method inspect*(self: Builtin): string = "builtin function"
method `$`*(self: Builtin): string = $ObjectType.BUILTIN_OBJ

type Integer* = ref object of Object
    value*: int64
method typ*(self: Integer): ObjectType =  ObjectType.INTERGER_OBJ
method inspect*(self: Integer): string = $self.value
method `$`*(self: Integer): string = $self.value
method hashKey*(self: Integer): HashKey = 
    var h: int = 0
    h = h !& hash(self.value)
    result = !$h

type Boolean* = ref object of Object
    value*: bool
method typ*(self: Boolean): ObjectType =  ObjectType.BOOLEAN_OBJ
method inspect*(self: Boolean): string = $self.value
method `$`*(self: Boolean): string = $self.value
method hashKey*(self: Boolean): HashKey = 
    var h: int = 0
    h = h !& hash(self.value)
    result = !$h

type String* = ref object of Object
    value*: string
method typ*(self: String): ObjectType =  ObjectType.STRING_OBJ
method inspect*(self: String): string = $self.value
method `$`*(self: String): string = "\"" & self.value & "\""
method hashKey*(self: String): HashKey = 
    var h: int = 0
    h = h !& hash(self.value)
    result = !$h

type Null* = ref object of Object
method typ*(self: Null): ObjectType =  ObjectType.NULL_OBJ
method inspect*(self: Null): string = "null"
method `$`*(self: Null): string = "Null"

type Return* = ref object of Object
    value*: Object
method typ*(self: Return): ObjectType =  ObjectType.RETURN_OBJ
method inspect*(self: Return): string = self.value.inspect()
method `$`*(self: Return): string = "return"

type Error* = ref object of Object
    message*: string
method typ*(self: Error): ObjectType =  ObjectType.ERROR_OBJ
method inspect*(self: Error): string = "ERROR: " & self.message
method `$`*(self: Error): string = "ERROR: " & self.message

type Environment* = ref object of Object
    store*: Table[string, Object]
    outer*: Option[Environment]
proc newEnviroment*(): Environment = Environment(store: initTable[string, Object]())
proc newEnclosedEnviroment*(outer: Option[Environment]): Environment = 
    let env = newEnviroment()
    env.outer = outer
    return env
proc `$`*(self: Environment): string = "Environment" & $self.store & $self.outer
proc get*(self: Environment, name: string): Option[Object] =
    if not self.store.hasKey(name) and self.outer.isSome():
        return self.outer.get().get(name)
    if self.store.hasKey(name): 
        return option(self.store[name])
    return none[Object]()
proc put*(self: Environment, name: string, value: Object): Object =
    self.store[name] = value
    return value

type Function* = ref object of Object
    parameters*: seq[Option[ast.Identifier]]
    body*:       Option[ast.BlockStatement]
    env*:        Option[Environment]
method typ*(self: Function): ObjectType =  ObjectType.FUNCTION_OBJ
proc inspect*(self: Function): string = 
    let pStr = self.parameters.map(proc(p: Option[ast.Identifier]): string = optionutils.optString(p)).join(", ")
    return fmt"fn({pStr})" & "{ " & optionutils.optString(self.body) & " }"
proc `$`*(self: Function): string = self.inspect()

type Array* = ref object of Object
    elements*: seq[Object]
method typ*(self: Array): ObjectType = ObjectType.ARRAY_OBJ
method inspect*(self: Array): string = "[" & self.elements.mapIt(it.inspect()).join(", ") & "]"
method `$`*(self: Array): string = self.inspect()

proc isHashable*(o: obj.Object): bool = o of String or o of Integer or o of Boolean
