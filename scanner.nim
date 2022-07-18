import
  std/strformat,
  std/strutils,
  std/tables,
  error,
  token

const keywords = {"and": And,
      "class": Class,
      "else": Else,
      "false": False,
      "for": For,
      "fun": Fun,
      "if": If,
      "nil": Nil,
      "or": Or,
      "print": Print,
      "return": Return,
      "super": Super,
      "this": This,
      "true": True,
      "var": Var,
      "while": While}.toTable

type Scanner* = ref object
  source*: string
  tokens*: seq[Token]
  start: int
  current: int
  line: int
  atEnd: bool

proc newScanner*(source: string): Scanner =
  result = new(Scanner)
  result.source = source
  result.tokens = @[]
  result.start = 0
  result.current = 0
  result.line = 1

proc atEnd(self: Scanner): bool =
  self.current >= self.source.len

proc advance(self: Scanner): char =
  result = self.source[self.current]
  self.current += 1

proc addToken(self: Scanner, kind: TokenKind, literal: LoxObject) =
  let text = self.source[self.start..<self.current]
  self.tokens.add(Token(kind: kind, lexeme: text, literal: literal, line: self.line))

proc addToken(self: Scanner, kind: TokenKind) =
  self.addToken(kind, LoxObject(kind: lokNil, nilValue: "nil"))

proc match(self: Scanner, expected: char): bool =
  if atEnd(self): return false
  if self.source[self.current] != expected: return false

  self.current += 1
  return true

proc peek(self: Scanner): char =
  if atEnd(self): '\0'
  else: self.source[self.current]

proc string(self: Scanner) =
  while self.peek() != '"' and not(atEnd(self)):
    if self.peek() == '\n': self.line += 1
    discard self.advance()

  if atEnd(self):
    loxError(self.line, "Unterminated string")
    return
  
  # eat last "
  discard self.advance()
  let value = LoxObject(kind: lokString, stringValue: self.source[self.start+1..<self.current-1])
  self.addToken(String, value)

proc peekNext(self: Scanner): char =
  if self.current + 1 >= self.source.len:
    return '\0'
  else:
    return self.source[self.current + 1]

proc number(self: Scanner) =
  while self.peek.isDigit:
    discard self.advance()
    # fractional
    if self.peek == '.' and self.peekNext.isDigit:
      # eat dot
      discard self.advance()
      while self.peek.isDigit:
        discard self.advance()
  self.addToken(Number, LoxObject(kind: lokNumber, numberValue: parseFloat(self.source[self.start..<self.current])))

proc identifier(self: Scanner) =
  while self.peek.isAlphaNumeric:
    discard self.advance()

  let text = self.source[self.start..<self.current]
  let kind = 
    if not keywords.hasKey(text):
      Identifier
    else:
      keywords[text]
  self.addToken(kind)


proc scanToken(self: Scanner) =
  let c = self.advance()
  case c
  of '(': self.addToken(TokenKind.LeftParen)
  of ')': self.addToken(TokenKind.RightParen)
  of '{': self.addToken(TokenKind.LeftBrace)
  of '}': self.addToken(TokenKind.RightBrace)
  of ',': self.addToken(TokenKind.Comma)
  of '.': self.addToken(TokenKind.Dot)
  of '-': self.addToken(TokenKind.Minus)
  of '+': self.addToken(TokenKind.Plus)
  of ';': self.addToken(TokenKind.Semicolon)
  of '*': self.addToken(TokenKind.Star)
  of '!': self.addToken(if self.match('='): TokenKind.BangEqual else: TokenKind.Bang)
  of '=': self.addToken(if self.match('='): TokenKind.EqualEqual else: TokenKind.Equal)
  of '<': self.addToken(if self.match('='): TokenKind.LessEqual else: TokenKind.Less)
  of '>': self.addToken(if self.match('='): TokenKind.GreaterEqual else: TokenKind.Greater)
  of '/':
    if self.match('/'):
      while self.peek() != '\n' and not atEnd(self):
        discard self.advance()
    else:
      self.addToken(TokenKind.Slash)
  of ' ', '\t', '\r': return
  of '\n': self.line += 1
  of '"': self.string()
  of '0'..'9': self.number()
  of 'a'..'z', 'A'..'Z', '_': self.identifier()
  else: 
    loxError(self.line, fmt"Unexpected character {c}")

proc scanTokens*(self: Scanner): seq[Token] =
  while not atEnd(self):
    self.start = self.current
    self.scanToken()
  
  self.tokens.add(Token(kind: TokenKind.Eof, lexeme: "", literal: LoxObject(kind:lokNil, nilValue: "nil"), line: self.line ))
  result = self.tokens

