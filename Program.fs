
[<EntryPoint>]
let main argv =

  Lang.Interpreter.interpret "./tests/test.txt"

  0