
[<EntryPoint>]
let main argv =

  if Array.length argv <= 0 then failwithf "No file to source code specified"

  let dev = if Array.length argv > 1 then (argv.[1] = "dev") else false

  Lang.Interpreter.interpret argv.[0] dev

  0