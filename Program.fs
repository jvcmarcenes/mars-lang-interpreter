
[<EntryPoint>]
let main argv =

  Lang.Interpreter.interpret "./tests/test.txt"

  0

(* 
  DONE // what the pseudolang is capable of:

  - IO
    - printing to the screen with WRITE and WRITELINE
    - reading console input in different forms with READ, READINT and READFLOAT
  - Variables and Expressions
    - defining lexical patterns with DEFINTION: let varname = EXPRESSION
    - operation evaluation
  - Arrays
    - defining arrays
    - referencing arrays by index
    - pushing values to an array with += syntax
  - Structures
    - looping a given number of times with a iteration variable with LOOP
    - if and if-else statements with IF
    - Procedures
      - methods with no return value and arbitrary amount of parameters
      - procedures without parameters are called by postfixing a "!" symbol
  - Meta
    - Scopes and scope-chaining
  - Other
    - single and multi line comments
    - holding execution with SLEEP
  -

  TODO

  - fix parser conflicts
    - example -
      - cannot name variable or method "writesum", as it conflicts with the write parser
  - find a way to do indendation based syntax?
    - currently using an "end tag" to close blocks
  - add type casting?
  - add syntax structure
    - functions / procedures / methods
    - make inlined else if statements
  - add prim type
    - switch ints and floats to generic number type ?
    - array
      - example -
        arr[2] = 12 // should be done, I expect some bugs still tho
    - map?
      - example -
        let map = {
          name = "AAA"
          age = "7"
        } // definition
        map.name // map access
        map.name = "BBB" // map re-assignment
    - add default properties into default type structures
      - e.g.: get length of an array / string
  - implement better error message for execution errors

  - after complete, move to a pascal (or portugol) interpreter

*)