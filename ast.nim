import 
  error,
  token,
  std/strformat


type
  ExpressionKind* = enum
    Binary,
    Grouping,
    Literal,
    Unary

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
    of Unary:
      uOperator*: Token
      uRight*: Expression

proc `$`*(self: Expression): string =
  case self.kind
  of Binary: fmt"({self.operator.lexeme} {self.left} {self.right})"
  of Grouping: fmt"(group {self.expr})"
  of Literal: fmt"{self.value}"
  of Unary: fmt"({self.uOperator.lexeme} {self.uRight})"


type
  Parser = ref object
    tokens: seq[Token]
    current: int

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

  if self.match(LeftParen):
    let expr = self.expression()
    self.consume(RightParen, "Expect ')' after expression")
    return Expression(kind: Grouping, expr: expr)

  error(self.peek, "Expect expression.")
  raise newException(ParseException, "Expect expression.")

proc unary(self: Parser): Expression =
  if self.match(Bang, Minus):
    let
      operator = self.previous
      right = self.unary()
    return Expression(kind: Unary, uOperator: operator, uRight: right)

  self.primary()

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

proc expression(self: Parser): Expression =
  equality(self)

proc parse*(self: Parser): Expression =
  try:
    return self.expression
  except ParseException:
    return nil

if isMainModule:
  let tree = Expression(
    kind: Binary,
    left: Expression(
      kind: Unary,
      uOperator: Token(kind: Minus, lexeme: "-", literal: nil, line: 1),
      uRight: Expression(
        kind: Literal,
        value: LoxObject(kind: lokNumber, numberValue: 123)
      )
    ),
    operator: Token(kind: Star, lexeme: "*", literal: nil, line: 1),
    right: Expression(
      kind: Grouping,
      expr: Expression(
        kind: Literal,
        value: LoxObject(kind: lokNumber, numberValue: 45.67)
      )
    )
  )
  echo tree
