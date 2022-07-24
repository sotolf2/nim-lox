import
  std/strutils

type 
  TokenKind* = enum
    # Single character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    # One or two character tokens
    Bang, BangEqual, 
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    # Literals
    Identifier, String, Number,

    # Keywords
    And, Class, Else, False, Fun, For If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Eof
  
  LoxObjectKind* = enum
    lokNumber,
    lokNil,
    lokString,
    lokBool

  LoxObject* = ref object
    case kind*: LoxObjectKind
    of lokNil: nilValue*: string
    of lokString: stringValue*: string
    of lokNumber: numberValue*: float
    of lokBool: boolValue*: bool

  Token* = object
    kind*: TokenKind
    lexeme*: string
    literal*: LoxObject
    line*: int

proc `$`*(self: LoxObject): string =
  case self.kind
  of lokNumber:
    let strnum = $self.numberValue
    if strnum.endsWith(".0"):
      strnum[0..^3]
    else:
      strnum
  of lokNil: "nil"
  of lokString: self.stringValue
  of lokBool: $self.boolValue

proc `$`*(self: Token): string =
  if self.literal != nil:
    $self.literal
  elif self.lexeme != "":
    $self.lexeme
  else:
    "nil"


