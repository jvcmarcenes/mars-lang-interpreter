
[<EntryPoint>]
let main argv =

  Lang.Interpreter.interpret argv.[0]

  0