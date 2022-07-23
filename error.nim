import
  token,
  std/strformat

var
  hadError* = false

type ParseException* = object of CatchableError

proc report(line: int, where, message: string) =
  stdErr.writeLine(fmt"[Line {line}] Error {where}: {message}")
  hadError = true;

proc error*(token: Token, message: string)=
  if token.kind == Eof:
    report(token.line, "at end", message)
  else:
    report(token.line, fmt"at '{token.lexeme}''", message)

proc loxError*(line: int, message: string) =
  report(line, "", message)

