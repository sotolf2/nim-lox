import
  std/strformat

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
    lokString

  LoxObject* = ref object
    case kind*: LoxObjectKind
    of lokNil: nilValue*: string
    of lokString: stringValue*: string
    of lokNumber: numberValue*: float

  Token* = object
    kind*: TokenKind
    lexeme*: string
    literal*: LoxObject
    line*: int

proc `$`*(self: LoxObject): string =
  case self.kind
  of lokNumber: $self.numberValue
  of lokNil: "nil"
  of lokString: self.stringValue

proc `$`*(self: Token): string =
  fmt"{self.kind} {self.lexeme} {self.literal}"


