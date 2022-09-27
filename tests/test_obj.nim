import unittest
import domain/monkey/obj

suite "obj.nim":
  test "hashKey":
    let hello1 = obj.String(value: "Hello World")
    let hello2 = obj.String(value: "Hello World")
    let diff1 = obj.String(value: "My name is johnny")
    let diff2 = obj.String(value: "My name is johnny")
    assert hello1.hashKey.hash == hello2.hashKey.hash
    assert diff1.hashKey.hash == diff2.hashKey.hash
    assert hello1.hashKey.hash != diff1.hashKey.hash

    
