import
  std/math,
  std/tables,
  std/strformat,
  std/times,
  ast,
  error,
  token

type
  Environment = ref object
    enclosing: Environment
    functions: Table[string, LoxFunction]
    values: Table[string, LoxObject]

  LoxFunction = ref object
    arity: int
    closure: Environment
    call: (proc (arguments: seq[LoxObject]): LoxObject)

proc newEnvironment(): Environment =
  Environment(enclosing: nil, values: initTable[string, LoxObject](), functions: initTable[string, LoxFunction]())

proc newEnvironment(enclosing: Environment): Environment =
  Environment(enclosing: enclosing, values: initTable[string, LoxObject](), functions: initTable[string, LoxFunction]())

proc define(self: Environment, name: string, value: LoxObject) =
  self.values[name] = value

proc defineFunction(self: Environment, name: string, loxFunction: LoxFunction) =
  self.values[name] = LoxObject(kind: lokIdentifier, name: name)
  self.functions[name] = loxFunction

proc get(self: Environment, name: Token): LoxObject =
  if self.values.hasKey(name.lexeme):
    return self.values[name.lexeme]

  if not self.enclosing.isNil():
    return self.enclosing.get(name)

  raise newException(RuntimeException, fmt"Undefined variable: {name.lexeme}")

proc getFunction(self: Environment, identifier: LoxObject): LoxFunction =
  if identifier.kind != lokIdentifier:
    raise newException(RuntimeException, "Can't call something that is not an identifier")

  if self.functions.hasKey(identifier.name):
    return self.functions[identifier.name]

  if not self.enclosing.isNil():
    return self.enclosing.getFunction(identifier)

proc assign(self: Environment, name: Token, value: LoxObject) =
  if self.values.hasKey(name.lexeme):
    self.values[name.lexeme] = value
    return
  
  if not self.enclosing.isNil():
    self.enclosing.assign(name, value)
    return

  raise newException(RuntimeException, fmt"Undefined variable: {name.lexeme}")

var globals = newEnvironment()
var environment = newEnvironment(globals)

# Defining glabal functions here
proc clock(arguments: seq[LoxObject]): LoxObject =
  let epoch = epochTime().trunc
  LoxObject(kind: lokNumber, numberValue: epoch)

globals.defineFunction("clock", LoxFunction(arity: 0, call: clock))

# Defining the interpreter
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
      of lokIdentifier:
        return self.name == other.name
  else:
    return false

proc checkNumberOperand(operator: Token, operand: LoxObject) =
  if operand.kind == lokNumber: return
  raise newException(RuntimeException, fmt"[Line {operator.line}] Operand must be a number")

proc checkNumberOperands(operator: Token, left, right: LoxObject) =
  if left.kind == lokNumber and right.kind == lokNumber: return
  raise newException(RuntimeException, fmt"[Line {operator.line}] Operand must be a number")

proc isCallable(self: LoxObject): bool =
  if self.kind == lokIdentifier:
    result = true

proc arity(self: LoxObject): int =
  let me = environment.getFunction(self)
  result = me.arity

proc call(self: LoxObject, expr: Expression, arguments: seq[LoxObject]): LoxObject =
  let me = environment.getFunction(self)
  let prevEnv = environment
  environment = me.closure
  result = me.call(arguments)
  environment = prevEnv

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
  of Assign:
    let value = evaluate(self.aValue)
    environment.assign(self.aName, value)
  of Logical:
    let left = evaluate(self.lleft)

    if self.loperator.kind == Or:
      if left.isTruthy(): return left
    else:
      if not left.isTruthy(): return left

    return evaluate(self.lright)
  of Call:
    let callee = evaluate(self.callee)

    var arguements: seq[LoxObject] = @[]
    for arguement in self.arguements:
      arguements.add(evaluate(arguement))

    if not callee.isCallable:
      raise newException(RuntimeException, "Can only call functions and classes")

    if arguements.len != callee.arity:
      raise newException(RuntimeException, fmt"{self.paren} Expected {callee.arity} arguements but got {arguements.len}.")
    return callee.call(self, arguements)
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

proc execute(self: Statement)

proc executeBlock(statements: seq[Statement], env: Environment) =
  let prev = environment
  try:
    environment = env
    for statement in statements:
      statement.execute()
  finally:
    environment = prev

type Return = ref object of CatchableError
  value: LoxObject

proc execute(self: Statement) =
  case self.kind
  of skBlock:
    executeBlock(self.statements, newEnvironment(environment))
  of skPrint:
    echo self.pexpr.evaluate()
  of skExpression:
    discard self.expr.evaluate()
  of skVar:
    var value: LoxObject
    if not(self.initialiser == nil):
      value = self.initialiser.evaluate()
    environment.define(self.name.lexeme, value)
  of skIf:
    if evaluate(self.condition).isTruthy():
      execute(self.thenBranch)
    elif not self.elseBranch.isNil():
      execute(self.elseBranch)
  of skWhile:
    while evaluate(self.wcondition).isTruthy():
      execute(self.wbody)
  of skReturn:
    var value: LoxObject = LoxObject(kind: lokNil, nilValue: "nil")
    if not self.rValue.isNil:
      value = evaluate(self.rValue)
    var ret = Return(value: value)
    raise ret

  of skFun:
    proc callFun(arguements: seq[LoxObject]): LoxObject =
      var environment = newEnvironment(environment)
      for idx, param in self.fParams:
        environment.define(param.lexeme, arguements[idx])

      try:
        executeBlock(self.fBody, environment)
      except Return:
        let ret: Return = getCurrentException().Return
        return ret.value

    var arity = self.fParams.len()
    globals.defineFunction(self.fName.lexeme, LoxFunction(arity: arity, call: callFun, closure: environment))


proc interpret*(statements: seq[Statement]) =
  try:
    for statement in statements:
      statement.execute()
  except RuntimeException:
    runtimeError(getCurrentExceptionMsg())

