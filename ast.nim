import 
  error,
  token,
  std/strformat


type
  ExpressionKind* = enum
    Assign,
    Call,
    Binary,
    Grouping,
    Literal,
    Logical,
    Unary,
    Variable

  Expression* = ref object
    case kind*: ExpressionKind
    of Binary:
      left*: Expression
      operator*: Token
      right*: Expression
    of Grouping:
      expr*: Expression
    of Literal:
      value*: LoxObject
    of Logical:
      lleft*: Expression
      loperator*: Token
      lright*: Expression
    of Unary:
      uOperator*: Token
      uRight*: Expression
    of Variable:
      name*: Token
    of Assign:
      aName*: Token
      aValue*: Expression
    of Call:
      callee*: Expression
      paren*: Token
      arguements*: seq[Expression]

  StatementKind* = enum
    skBlock,
    skExpression,
    skFun,
    skIf,
    skPrint,
    skReturn,
    skVar,
    skWhile

  Statement* = ref object
    case kind*: StatementKind
    of skBlock:
      statements*: seq[Statement]
    of skFun:
      fName*: Token
      fParams*: seq[Token]
      fbody*: seq[Statement]
    of skExpression:
      expr*: Expression
    of skIf:
      condition*: Expression
      thenBranch*: Statement
      elseBranch*: Statement
    of skPrint:
      pexpr*: Expression
    of skReturn:
      keyword*: Token
      rValue*: Expression
    of skVar:
      name*: Token
      initialiser*: Expression 
    of skWhile:
      wcondition*: Expression
      wbody*: Statement

  Parser = ref object
    tokens: seq[Token]
    current: int

proc `$`*(self: Expression): string =
  case self.kind
  of Binary: fmt"({self.operator.lexeme} {self.left} {self.right})"
  of Grouping: fmt"(group {self.expr})"
  of Literal: fmt"{self.value}"
  of Logical: fmt"({self.loperator.lexeme} {self.lleft} {self.lright})"
  of Unary: fmt"({self.uOperator.lexeme} {self.uRight})"
  of Variable: $self.name
  of Assign: fmt"(assign {self.aName} {self.aValue})"
  of Call: fmt"(call {self.callee} {self.arguements})"

proc `$`*(self: Statement): string =
  case self.kind:
  of skBlock: fmt"(block [{self.statements}])"
  of skPrint: fmt"(print {self.pexpr})"
  of skExpression: $self.expr
  of skVar: fmt"(var {self.name} {self.initialiser})"
  of skIf: fmt"(if {self.condition} {self.thenBranch} {self.elseBranch})"
  of skWhile: fmt"(while {self.wcondition} {self.wbody})"
  of skFun: fmt"(defn {self.fName} {self.fParams} {self.fBody})"
  of skReturn: fmt"(return {self.rValue})"

proc newParser*(tokens: seq[Token]): Parser =
  result = new(Parser)
  result.tokens = tokens
  result.current = 0

proc previous(self: Parser): Token =
  self.tokens[self.current - 1]

proc peek(self: Parser): Token =
  self.tokens[self.current]

proc isAtEnd(self: Parser): bool =
  self.peek().kind == Eof

proc advance(self: Parser) =
  if not self.isAtEnd:
    self.current += 1

proc check(self: Parser, kind: TokenKind): bool =
  if self.isAtEnd():
    return false
  self.peek().kind == kind

proc match(self: Parser, kinds: varargs[TokenKind]): bool =
  for kind in kinds:
    if self.check(kind):
      self.advance()
      return true
  false

proc consume(self: Parser, kind: TokenKind, message: string) =
  if self.check(kind):
    self.advance()
    return

  error(self.peek, message)
  raise newException(ParseException, message)

proc consumerReturnToken(self: Parser, kind: TokenKind, message: string): Token =
  if self.check(kind):
    result = self.tokens[self.current]
    self.advance()
    return

  error(self.peek, message)
  raise newException(ParseException, message)

proc syncronise(self: Parser) =
  self.advance()

  while not self.isAtEnd():
    if self.previous.kind == Semicolon:
      return

    case self.peek.kind
    of Class, Fun, Var, For, If, While, Print, Return:
      return
    else: self.advance()

  


# Forward decalration because we have to recurse 
proc expression(self: Parser): Expression 

proc primary(self: Parser): Expression =
  if self.match(False): 
    return Expression(kind: Literal, value: LoxObject(kind: lokBool, boolValue: false))
  if self.match(True):
    return Expression(kind: Literal, value: LoxObject(kind: lokBool, boolValue: true))
  if self.match(Nil):
    return Expression(kind: Literal, value: LoxObject(kind: lokNil, nilValue: "nil"))

  if self.match(Number, String):
    return Expression(kind: Literal, value: self.previous.literal)

  if self.match(Identifier):
    return Expression(kind: Variable, name: self.previous)

  if self.match(LeftParen):
    let expr = self.expression()
    self.consume(RightParen, "Expect ')' after expression")
    return Expression(kind: Grouping, expr: expr)

  error(self.peek, "Expect expression.")
  raise newException(ParseException, "Expect expression.")

proc finishCall(self: Parser, callee: Expression): Expression =
  var arguements: seq[Expression] = @[]
  if not self.check(RightParen):
    while true:
      if arguements.len >= 255:
        error(self.peek, "Can't have more than 255 arguements")
      arguements.add(self.expression())
      if not self.match(Comma):
        break
  let token = self.consumerReturnToken(RightParen, "Expect ) after arguements")

  Expression(kind: Call, callee: callee, paren: token, arguements: arguements)

proc call(self: Parser): Expression =
  result = self.primary()

  while true:
    if self.match(LeftParen):
      result = self.finishCall(result)
    else:
      break

proc unary(self: Parser): Expression =
  if self.match(Bang, Minus):
    let
      operator = self.previous
      right = self.unary()
    return Expression(kind: Unary, uOperator: operator, uRight: right)

  self.call()

proc factor(self: Parser): Expression =
  result = self.unary()

  while self.match(Slash, Star):
    let
      operator = self.previous
      right = self.unary
    result = Expression(kind: Binary, left: result, operator: operator, right: right)

proc term(self: Parser): Expression =
  result = self.factor()

  while self.match(Minus, Plus):
    let
      operator = self.previous
      right = self.factor
    result = Expression(kind: Binary, left: result, operator: operator, right: right)

proc comparison(self: Parser): Expression =
  result = self.term()
  
  while self.match(Greater, GreaterEqual, Less, LessEqual):
    let
      operator = self.previous()
      right = self.term()
    result = Expression(kind: Binary, left: result, operator: operator, right: right)

proc equality(self: Parser): Expression =
  result = comparison(self)

  while self.match(BangEqual, EqualEqual):
    let 
      operator = previous(self)
      right = comparison(self)
    result = Expression(kind: Binary, left: result, operator: operator, right: right)

proc land(self: Parser): Expression =
  var expr = self.equality()

  while self.match(And):
    let
      operator = self.previous()
      right = self.equality()
    expr = Expression(kind: Logical, lleft: expr, loperator: operator, lright: right)

  return expr

proc lor(self: Parser): Expression =
  var expr = self.land()

  while self.match(Or):
    let 
      operator = self.previous()
      right = self.land()
    expr = Expression(kind: Logical, lleft: expr, loperator: operator, lright: right)

  return expr

proc assignment(self: Parser): Expression =
  let expr = self.lor()

  if self.match(Equal):
    let 
      equals = self.previous()
      value = self.assignment()

    if expr.kind == Variable:
      let name = expr.name
      return Expression(kind: Assign, aName: name, aValue: value)

    error(equals, "Invalid assignment target")

  return expr


proc expression(self: Parser): Expression =
  self.assignment()

proc printStatement(self: Parser): Statement =
  let value = self.expression()
  self.consume(Semicolon, "Expect ; after value.")
  Statement(kind: skPrint, pexpr: value)

proc expressionStatement(self: Parser): Statement =
  let value = self.expression()
  self.consume(Semicolon, "Expect ; after value.")
  Statement(kind: skExpression, expr: value)

proc declaration(self: Parser): Statement

proc blockStatement(self: Parser): Statement =
  var statements: seq[Statement]
  while (not self.check(RightBrace)) and (not self.isAtEnd()):
    statements.add(self.declaration())

  self.consume(RightBrace, "Expect } after block")
  return Statement(kind:skBlock, statements: statements)

proc statement(self: Parser): Statement

proc ifStatement(self: Parser): Statement =
  self.consume(LeftParen, "Expect ( after if")
  let condition = self.expression()
  self.consume(RightParen, "Expect ) after if condition")

  let thenBranch = self.statement()
  var elseBranch: Statement
  if self.match(Else):
    elseBranch = self.statement()

  Statement(kind: skIf, condition: condition, thenBranch: thenBranch, elseBranch: elseBranch)

proc whileStatement(self: Parser): Statement =
  self.consume(LeftParen, "Expect ( after while")
  let condition = self.expression()
  self.consume(RightParen, "Expect ) after while condition")
  let body = self.statement()

  Statement(kind: skWhile, wcondition: condition, wbody: body)

proc varDeclaration(self: Parser): Statement =
  let name = self.consumerReturnToken(Identifier, "Expect variable name")

  var initialiser: Expression
  if self.match(Equal):
    initialiser = self.expression()
  self.consume(Semicolon, "Expect ; after variable declaration")
  return Statement(kind: skVar, name: name, initialiser: initialiser)

proc function(self: Parser, kind: string): Statement =
  let name: Token = self.consumerReturnToken(Identifier, fmt"Expect {kind} name.")
  self.consume(LeftParen, fmt"Expect ( after {kind} name.")
  var parameters: seq[Token] = @[]
  if not self.check(RightParen):
    while true:
      if parameters.len >= 255:
        error(self.peek, "Can't have more than 255 parameters")
      parameters.add(self.consumerReturnToken(Identifier, "Expect parameter name"))
      if not self.match(Comma):
        break
  self.consume(RightParen, "Expect ) after parameter")

  self.consume(LeftBrace, fmt"Expect {{ after {kind} body.")
  let body: seq[Statement] = self.blockStatement().statements
  return Statement(kind: skFun, fname: name, fParams: parameters, fBody: body)

proc returnStatement(self: Parser): Statement =
  let keyword = self.previous
  var value: Expression 
  if not self.check(Semicolon):
    value = self.expression()
  self.consume(Semicolon, "Expect ; after return value")
  return Statement(kind: skReturn, keyword: keyword, rValue: value)

proc forStatement(self: Parser): Statement =
  self.consume(LeftParen, "Expect ( after for")

  var initialiser: Statement
  if self.match(Semicolon):
    initialiser = nil
  elif self.match(Var):
    initialiser = self.varDeclaration()
  else:
    initialiser = self.expressionStatement()

  var condition: Expression
  if not self.check(Semicolon):
    condition = self.expression()
  self.consume(Semicolon, "Expect ; after loop condition")

  var increment: Expression
  if not self.check(RightParen):
    increment = self.expression()
  self.consume(RightParen, "Expect ) after for clauses")

  var body = self.statement()

  if not increment.isNil():
    body = Statement(kind: skBlock, statements: @[body, Statement(kind: skExpression, expr: increment)])

  if condition.isNil():
    condition = Expression(kind: Literal, value: LoxObject(kind: lokBool, boolvalue: true))
  body = Statement(kind: skWhile, wcondition: condition, wbody: body)

  if not initialiser.isNil():
    body = Statement(kind: skBlock, statements: @[initialiser, body])

  return body



proc statement(self: Parser): Statement =
  if self.match(Print):
    return self.printStatement()
  if self.match(Return):
    return self.returnStatement()
  if self.match(LeftBrace):
    return self.blockStatement()
  if self.match(If):
    return self.ifStatement()
  if self.match(While):
    return self.whileStatement()
  if self.match(For):
    return self.forStatement()
  self.expressionStatement()


proc declaration(self: Parser): Statement =
  try:
    if self.match(Fun):
      return self.function("function")
    if self.match(Var):
      return self.varDeclaration
    return self.statement()
  except ParseException:
    self.syncronise()
    return nil

proc parse*(self: Parser): seq[Statement] =
  while not self.isAtEnd:
    result.add(self.declaration())

         

