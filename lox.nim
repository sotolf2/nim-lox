import 
  std/os,
  error,
  scanner,
  token

proc run(source: string) =
  let scanner = newScanner(source)
  let tokens: seq[Token] = scanner.scanTokens()

  for token in tokens:
    echo token

  echo "running"

proc runPrompt() =
  while true:
    write(stdout, "> ")
    let line = readLine(stdin)
    if line == "":
      break
    line.run()
    hadError = false
  quit(0)

proc runFile(file: string) =
  let lines = readFile(file)
  lines.run()
  if hadError:
    quit(65)
  quit(0)

if isMainModule:
  let params = commandLineParams()
  case params.len
  of 0:
    runPrompt()
  of 1:
    runFile(params[0])
  else:
    echo "usage: lox [script]"
    quit(64)

