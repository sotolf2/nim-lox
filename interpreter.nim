import
  std/tables,
  std/strformat,
  ast,
  error,
  token

type
  Environment = ref object
    values: Table[string, LoxObject]

proc newEnvironment(): Environment =
  Environment(values: initTable[string, LoxObject]())

proc define(self: Environment, name: string, value: LoxObject) =
  self.values[name] = value

proc get(self: Environment, name: Token): LoxObject =
  if self.values.hasKey(name.lexeme):
    return self.values[name.lexeme]

  raise newException(RuntimeException, fmt"Undefined variable: {name.lexeme}")

let environment = newEnvironment()

proc isTruthy(self: LoxObject): bool =
  if self.kind == lokNil: return false
  if self.kind == lokBool: return self.boolValue
  true

proc isEqualTo(self, other: LoxObject): bool =
  if self.kind == other.kind:
    case self.kind:
      of lokNumber:
        return self.numberValue == other.numberValue
      of lokBool:
        return self.boolValue == other.boolValue
      of lokNil:
        return true
      of lokString:
        return self.stringValue == other.stringValue
  else:
    return false

proc checkNumberOperand(operator: Token, operand: LoxObject) =
  if operand.kind == lokNumber: return
  raise newException(RuntimeException, fmt"[Line {operator.line}] Operand must be a number")

proc checkNumberOperands(operator: Token, left, right: LoxObject) =
  if left.kind == lokNumber and right.kind == lokNumber: return
  raise newException(RuntimeException, fmt"[Line {operator.line}] Operand must be a number")

proc evaluate(self: Expression): LoxObject =
  case self.kind:
  of Literal:
    return self.value
  of Grouping:
    return self.expr.evaluate()
  of Unary:
    let right = self.uRight.evaluate()
    if self.uOperator.kind == Minus:
      checkNumberOperand(self.uOperator, right)
      return LoxObject(kind: lokNumber, numberValue: -right.numberValue)
    elif self.uOperator.kind == Bang:
      return LoxObject(kind: lokBool, boolValue: not right.isTruthy())
    else:
      return LoxObject(kind: lokNil, nilValue: "nil")
  of Variable:
    return environment.get(self.name)
  of Binary:
    let
      left = self.left.evaluate()
      right = self.right.evaluate()

    case self.operator.kind:
      of Minus:
        checkNumberOperands(self.operator, left, right)
        return LoxObject(kind: lokNumber, numberValue: left.numberValue - right.numberValue)
      of Slash:
        checkNumberOperands(self.operator, left, right)
        return LoxObject(kind: lokNumber, numberValue: left.numberValue / right.numberValue)
      of Star:
        checkNumberOperands(self.operator, left, right)
        return LoxObject(kind: lokNumber, numberValue: left.numberValue * right.numberValue)
      of Plus:
        if left.kind == lokNumber and right.kind == lokNumber:
          return LoxObject(kind: lokNumber, numberValue: left.numberValue + right.numberValue)
        if left.kind == lokString and right.kind == lokString:
          return LoxObject(kind: lokString, stringValue: left.stringValue & right.stringValue)
        raise newException(RuntimeException, fmt"[Line {self.operator.line}] operands must be strings or numbers")
      of Greater:
        checkNumberOperands(self.operator, left, right)
        return LoxObject(kind: lokBool, boolValue: left.numberValue > right.numberValue)
      of GreaterEqual:
        checkNumberOperands(self.operator, left, right)
        return LoxObject(kind: lokBool, boolValue: left.numberValue >= right.numberValue)
      of Less:
        checkNumberOperands(self.operator, left, right)
        return LoxObject(kind: lokBool, boolValue: left.numberValue < right.numberValue)
      of LessEqual:
        checkNumberOperands(self.operator, left, right)
        return LoxObject(kind: lokBool, boolValue: left.numberValue <= right.numberValue)
      of EqualEqual:
        return LoxObject(kind: lokBool, boolValue: left.isEqualTo(right))
      of BangEqual:
        return LoxObject(kind: lokBool, boolValue: not left.isEqualTo(right))
      else:
        assert(false, "evaluate operator binary -- not reachable")

proc execute(self: Statement) =
  case self.kind
  of skPrint:
    echo self.pexpr.evaluate()
  of skExpression:
    discard self.expr.evaluate()
  of skVar:
    var value: LoxObject
    if not(self.initialiser == nil):
      value = self.initialiser.evaluate()
    environment.define(self.name.lexeme, value)

proc interpret*(statements: seq[Statement]) =
  try:
    for statement in statements:
      statement.execute()
  except RuntimeException:
    runtimeError(getCurrentExceptionMsg())

