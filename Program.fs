
[<EntryPoint>]
let main argv =

  Lang.Interpreter.interpret $"./tests/{argv.[0]}.txt"

  0