import
  std/strformat

var
  hadError* = false

proc report(line: int, where, message: string) =
  stdErr.writeLine(fmt"[Line {line}] Error {where}: {message}")
  hadError = true;

proc loxError*(line: int, message: string) =
  report(line, "", message)

