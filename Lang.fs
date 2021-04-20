module Lang

  open System
  open FParsec

  open Railway

  module Model =
    type VarType = Int | Float | String | Array | Any // not in use yet

    type Identifier = string

    type ParamList = string list // alias for procedure and function parameters

    type AssignmentOperation = Set | PlusEquals | MinusEquals

    type UnaryOperation = NumberNegation | Identity | BooleanNegation

    type BinaryOperation =
      | Add | Sub | Mul | Div | Mod // arithmetic operators
      | Equals | NotEquals // equality operators
      | And | Or // boolean operators
      | GreaterThan | GreaterThanOrEquals | LesserThan | LesserThanOrEquals //comparison operators

    type Expression =
      | StringLiteral of string
      | IntLiteral of float
      | FloatLiteral of float
      | BooleanLiteral of bool
      | ArrayLiteral of Expression list
      | MapLiteral of Map<Identifier, Expression>
      | Read
      | ReadNumber
      | Random of Expression
      | VarReference of Identifier
      | ArrayAccess of Identifier * Expression
      | MapAccess of Identifier list
      | Unary of UnaryOperation * Expression
      | Binary of BinaryOperation * Expression * Expression
      | Group of Expression
      | Empty
      | FunctionCall of Identifier * Expression list

    type Statement =
      | Comment
      | Write of Expression
      | WriteLine of Expression
      | Definition of Identifier * Expression
      | Assignment of AssignmentOperation * Expression * Expression
      | Loop of Identifier * Expression * Block
      | If of Expression * Block * Block
      | Sleep of Expression
      | End
      | ProcedureDefinition of Identifier * ParamList * Block
      | ProcedureCall of Identifier * Expression list
      | FunctionDefintion of Identifier * ParamList * Block
      | Return of Expression
      | Use of string

    and Block = Statement list // type alias for a list of statements

    type Method = {
      statements: Block
      plist: ParamList
    }

    type Scope = { 
      parent: Option<Scope>
      self: Map<Identifier, obj>
      quit: Option<Expression> // aaa I hate having "quit" on this record type
    }

    // let objefy x = x :> obj

    module Scope =
      let defaultScope = {
        parent = None
        self = Map.empty
        quit = None
      }

      let hasvar id scope = 
        Map.containsKey id scope.self

      let updscope f scope = { scope with self = f scope.self }

      let recscope id scope act pact fact =
        if hasvar id scope then act ()
        else match scope.parent with Some p -> pact p | None -> fact ()

      let addvar id value scope = 
        if hasvar id scope then failwithf $"{id} is already defined" 
        else updscope (Map.add id value) scope

      let remvar id scope = 
        updscope (Map.remove id) scope

      let rec updvar id value scope = 
        let changer = function
          | Some _ -> Some value
          | None -> failwithf $"{id} is not defined"
        let upd = Map.change id changer
        let act () = updscope upd scope
        let pact p = updvar id value p
        let fact () = failwithf $"{id} is not defined"
        recscope id scope act pact fact

      let rec getvar id scope = 
        let act () = Map.find id scope.self
        let pact p = getvar id p
        let fact () = failwithf $"{id} is not defined"
        recscope id scope act pact fact

  module Parser =
    open Model

    // 
    let (<?|>) p1 p2 = attempt p1 <|> p2

    let rec attemptChoice = function
      | p1::tail -> p1 <?|> attemptChoice tail
      | [] -> pzero

    // helper method for statements with 3 values
    let fix f ((a, b), c) = f (a, b, c)

    // Characters Constructs
    let ws = skipMany (skipChar ' ')
    let ws1 = skipMany1 (skipChar ' ')

    let nl = skipMany1 (ws .>> skipNewline .>> ws)

    let quote = skipChar '"'
    let comma = skipChar ','

    let dslash = skipString "//"

    let lpar = skipChar '('
    let rpar = skipChar ')'
    let lsbr = skipChar '['
    let rsbr = skipChar ']'
    let lbra = skipChar '{'
    let rbra = skipChar '}'

    let endtag = skipString "end"

    let empty = followedByNewline <|> followedBy eof >>% Empty
    //

    let id = identifier (new IdentifierOptions()) |>> fun s -> s

    // Expressions
    let opp = OperatorPrecedenceParser<Expression, _, _>()

    let expr = opp.ExpressionParser

    let stringLiteral = quote >>. manyCharsTill anyChar quote |>> StringLiteral 
    let intLiteral = pint32 |>> fun i -> IntLiteral (float i)
    let floatLiteral = pfloat |>> FloatLiteral
    let boolLiteral = stringReturn "true" true <|> stringReturn "false" false |>> BooleanLiteral

    let arrayLiteral = lsbr >>. sepBy expr comma .>> rsbr |>> ArrayLiteral

    let mapLiteral = lbra >>. manyTill (spaces >>. id .>> ws .>> skipChar '=' .>> ws .>>. expr .>> nl) rbra |>> (Map.ofList >> MapLiteral)

    let read = stringReturn "read" Read
    let readnumber = stringReturn "readnumber" ReadNumber
    let random = skipString "random" >>. ws1 >>. expr |>> Random

    let varReference = id |>> VarReference
    let arrayAccess = id .>> lsbr .>>. expr .>> rsbr |>> ArrayAccess
    let mapAccess = id .>> skipChar '.' .>>. sepBy1 id (skipChar '.') |>> fun (h, t) -> MapAccess (h::t)

    // let methodcaller = (ws1 >>. sepBy1 expr (followedBy expr)) <?|> (stringReturn "!" []) // this method caller should not parse expr as funccalls 
    let methodcaller = (skipChar '!' >>. sepBy expr (followedBy expr))

    let funccall = id .>>. methodcaller |>> FunctionCall

    let group = lpar >>. expr .>> rpar |>> Group

    opp.TermParser <- ws >>. attemptChoice [
      stringLiteral <??> nameof stringLiteral
      floatLiteral <??> nameof floatLiteral
      intLiteral <??> nameof intLiteral
      boolLiteral <??> nameof boolLiteral
      arrayLiteral <??> nameof arrayLiteral
      mapLiteral <??> nameof mapLiteral
      readnumber <??> nameof readnumber
      read <??> nameof read
      random <??> nameof random
      arrayAccess <??> nameof arrayAccess
      mapAccess <??> nameof mapAccess
      funccall <??> nameof funccall
      varReference <??> nameof varReference // VarReference must come after arrayAccess and funccall due to parser conflicts
      group <??> nameof group
    ] .>> ws

    let unary op x = Unary (op, x)
    let binary op x y = Binary (op, x, y)

    opp.AddOperator <| InfixOperator("+", ws, 4, Associativity.Left, binary Add)
    opp.AddOperator <| InfixOperator("-", ws, 4, Associativity.Left, binary Sub)
    opp.AddOperator <| InfixOperator("*", ws, 5, Associativity.Left, binary Mul)
    opp.AddOperator <| InfixOperator("/", ws, 5, Associativity.Left, binary Div)
    opp.AddOperator <| InfixOperator("%", ws, 4, Associativity.Left, binary Mod)

    opp.AddOperator <| InfixOperator("&&", ws, 1, Associativity.None, binary And)
    opp.AddOperator <| InfixOperator("||", ws, 1, Associativity.None, binary Or)

    opp.AddOperator <| InfixOperator("==", ws, 2, Associativity.None, binary Equals)
    opp.AddOperator <| InfixOperator("!=", ws, 2, Associativity.None, binary NotEquals)
    opp.AddOperator <| InfixOperator(">", ws, 2, Associativity.None, binary GreaterThan)
    opp.AddOperator <| InfixOperator(">=", ws, 2, Associativity.None, binary GreaterThanOrEquals)
    opp.AddOperator <| InfixOperator("<", ws, 2, Associativity.None, binary LesserThan)
    opp.AddOperator <| InfixOperator("<=", ws, 2, Associativity.None, binary LesserThanOrEquals)

    opp.AddOperator <| PrefixOperator("-", ws, 3, true, unary NumberNegation)
    opp.AddOperator <| PrefixOperator("+", ws, 3, true, unary Identity)
    opp.AddOperator <| PrefixOperator("!", ws, 1, true, unary BooleanNegation)
    //

    let equassign = stringReturn "=" Set
    let addassign = stringReturn "+=" PlusEquals
    let minassign = stringReturn "-=" MinusEquals

    let assignops = attemptChoice [equassign; addassign; minassign]

    // Statements
    let statement, statementRef = createParserForwardedToRef<Statement, unit>()

    let singlecomment = dslash >>. skipManyTill skipAnyChar (skipNewline <|> eof) >>% Comment
    let multicomment = skipString "/*" >>. skipManyTill skipAnyChar (skipString "*/") >>% Comment
    let comment = singlecomment <|> multicomment

    let writeline = skipString "writeline" >>. (ws1 >>. expr <|> empty) |>> WriteLine
    let write = skipString "write" >>. ws1 >>. expr |>> Write

    let definition = skipString "let" >>. ws1 >>. id .>> ws .>> skipChar '=' .>> ws .>>. expr |>> Definition
    let assignment = (id .>>. (opt (lsbr >>. expr .>>  rsbr))) .>> ws .>>. assignops .>> ws .>>. expr |>> fun (((i, a), o), e) ->
      let id = 
        match a with
        | Some ae -> ArrayAccess (i, ae)
        | None -> VarReference i
      Assignment (o, id, e)

    let sleep = skipString "sleep" >>. ws1 >>. intLiteral |>> Sleep

    let blockEndBy endp = ws >>. manyTill statement (ws >>. endp)

    let loop = skipString "loop" >>. ws1 >>. id .>> ws1 .>>. expr .>> nl .>>. blockEndBy endtag |>> fix Loop 

    let ifstmt, ifstmtRef = createParserForwardedToRef<Statement, unit>()
    let elseblock = skipString "else" >>. ((ws1 >>. ifstmt |>> fun s -> [s]) <|> (ws >>. nl >>. blockEndBy endtag))
    do ifstmtRef := skipString "if" >>. ws1 >>. expr .>> nl .>>. blockEndBy (endtag <|> followedByString "else") .>>. (elseblock <|> preturn []) |>> fix If

    let procdef = skipString "procedure" >>. ws1 >>. id .>>. ((ws1 >>. sepBy id ws1) <|> preturn []) .>> nl .>>. blockEndBy endtag |>> fix ProcedureDefinition
    let proccall = id .>>. methodcaller |>> ProcedureCall

    let funcdef = skipString "function" >>. ws1 >>. id .>>. ((ws1 >>. sepBy id ws1) <|> preturn []) .>> nl .>>. blockEndBy endtag |>> fix FunctionDefintion

    let ret = skipString "return" >>. ws1 >>. expr |>> Return

    let _use = skipString "use" >>. ws1 >>. quote >>. manyCharsTill anyChar quote |>> Use

    do statementRef := spaces >>. attemptChoice [
          writeline <??> nameof writeline
          write <??> nameof write
          comment <??> nameof comment
          ret <??> nameof ret
          sleep <??> nameof sleep
          definition <??> nameof definition
          funcdef <??> nameof funcdef
          procdef <??> nameof procdef
          loop <??> nameof loop
          ifstmt  <??> nameof ifstmt
          assignment <??> nameof assignment
          proccall <??> nameof proccall
          _use <??> "use"
        ] .>> spaces .>> optional (followedByL eof "end of file")
    //

    // Module
    let moduleParser = skipString "module" >>. ws >>. nl >>. manyTill (spaces >>. (funcdef <|> procdef <|> definition <|> comment) .>> spaces) eof

    let parseModule path =
      match runParserOnFile moduleParser () path Text.Encoding.UTF8 with
      | ParserResult.Success (res, _, _) -> Success res
      | ParserResult.Failure (err, _, _) -> Failure ("[module parser]: " + err)
    //

    // Program parsing
    let program = blockEndBy eof

    let parse path =
      match runParserOnFile program () path Text.Encoding.UTF8 with
      | ParserResult.Success (res, _, _) -> Success res
      | ParserResult.Failure (err, _, _) -> Failure ("[parser]: " + err)
    
  module Interpreter =
    open Model
    open Model.Scope
    open Parser

    let addop (l: obj) (r: obj) =
      match l, r with
      | (:? float as _l), (:? float as _r) -> _l + _r :> obj
      | (:? string as _l), (:? string as _r) -> _l + _r :> obj
      | (:? string as _l), (:? float as _r) -> _l + (string _r) :> obj
      | (:? float as _l), (:? string as _r) -> (string _l) + _r :> obj
      | _ -> failwithf $"Invalid operation for types {l.GetType()} and {r.GetType()}"

    let oper<'t,'r> (f: 't -> 't -> 'r) (l: obj) (r: obj) =
      match l, r with
      | (:? 't as _l), (:? 't as _r) -> f _l _r :> obj
      | _ -> failwith $"Invalid operation for types {l.GetType()} and {r.GetType()}"

    let rec execute stmt (scope: Scope) =
      let runBlock block scope =
        let rec fold block scope =
          match block with
          | head::tail -> 
            let _scope = execute head scope
            match _scope.quit with
            | Some _ -> _scope
            | None -> fold tail _scope
          | [] -> scope
        fold block scope

      let rec evaluate expr (scope: Scope) =
        match expr with
        | StringLiteral s -> s :> obj
        | IntLiteral n -> n :> obj
        | FloatLiteral n -> n :> obj
        | BooleanLiteral b -> b :> obj
        | ArrayLiteral arr -> arr |> List.map (fun expr -> evaluate expr scope) :> obj
        | MapLiteral m -> m |> Map.map (fun _ expr -> evaluate expr scope) :> obj
        | VarReference id -> getvar id scope
        | ArrayAccess (id, expr) -> 
          let index = int32 (evaluate expr scope :?> float)
          (getvar id scope :?> obj list).[index]
        | MapAccess ids ->
          match ids with
          | h::t ->
            let rec access m b =
              match b with
              | f::r -> access (Map.find f (m :?> Map<Identifier, obj>)) r
              | [] -> m
            access (getvar h scope :?> Map<Identifier, obj>) t
          | [] -> failwithf $"wah"
        | Read -> Console.ReadLine() :> obj
        | ReadNumber -> float (Console.ReadLine ()) :> obj
        | Random expr -> (new Random()).Next() % (evaluate expr scope :?> int) :> obj
        | Unary (op, x) ->
          let _x = evaluate x scope
          match op with
          | NumberNegation -> -(_x :?> int) :> obj
          | Identity -> _x
          | BooleanNegation -> not (_x :?> bool) :> obj
        | Binary (op, l, r) ->
          let _l = evaluate l scope
          let _r = evaluate r scope
          match op with
          | Add -> addop _l _r
          | Sub -> oper<float, float> (-) _l _r
          | Mul -> oper<float, float> (*) _l _r
          | Div -> oper<float, float> (/) _l _r
          | Mod -> oper<float, float> (%) _l _r
          | And -> oper<bool, bool> (&&) _l _r
          | Or -> oper<bool, bool> (||) _l _r
          | Equals -> oper<obj, bool> (=) _l _r
          | NotEquals -> oper<obj, bool> (<>) _l _r
          | GreaterThan -> oper<IComparable, bool> (>) _l _r
          | GreaterThanOrEquals -> oper<IComparable, bool> (>=) _l _r
          | LesserThan -> oper<IComparable, bool> (<) _l _r
          | LesserThanOrEquals -> oper<IComparable, bool> (<=) _l _r
        | Group expr  -> evaluate expr scope
        | Empty -> "" :> obj
        | FunctionCall (id, elist) ->
          let func = scope |> getvar id :?> Method
          if List.length func.plist = List.length elist then
            let innerScope = {
              parent = Some scope
              self = func.plist |> List.mapi (fun i v -> v, (evaluate elist.[i] scope)) |> Map.ofSeq
              quit = None
            }
            let _scope = runBlock func.statements innerScope
            match _scope.quit with
            | Some rexpr -> evaluate rexpr _scope
            | None -> failwithf $"function did not return a value"
          else
            failwithf $"number of parameters passed to \"{id}\" call did not match expected number of params"

      let evaluateToInt expr space = int32 (evaluate expr space :?> float)

      match stmt with
      | Comment -> scope
      | Write expr -> printf $"{evaluate expr scope}"; scope
      | WriteLine expr -> printfn $"{evaluate expr scope}"; scope
      | Definition (id, expr) -> scope |> addvar id (evaluate expr scope)
      | Assignment (op, ref, expr) ->
        match ref with
        | VarReference id -> 
          match op with
          | Set -> scope |> updvar id (evaluate expr scope)
          | PlusEquals -> 
            let v = getvar id scope
            match v with
            | :? (obj list) as _v -> updvar id (List.append _v [evaluate expr scope]) scope
            | :? int | :? float | :? string as _v -> updvar id (addop _v (evaluate expr scope)) scope
            // | :? string as _v -> updvar id (_v + (evaluate expr scope :?> string) :> obj) scope
            | _ -> failwithf $"Invalid operator (+=) for type {v.GetType()}"
          | MinusEquals -> 
            let v = getvar id scope
            match v with
            | :? (obj list) as _v -> 
              let rem = evaluateToInt expr scope
              if rem < 0 || rem >= List.length _v then failwithf $"index out of bounds"
              let _list = 
                _v
                |> List.mapi (fun i v -> i <> rem, v)
                |> List.filter fst
                |> List.map snd
              updvar id _list scope
            | :? int | :? float as _v -> updvar id (oper<float, float> (-) _v (evaluate expr scope)) scope
            | _ -> failwithf $"Invalid operator (-=) for type {v.GetType()}"
        | ArrayAccess (id, iexpr) ->
          let index = evaluateToInt iexpr scope
          let arr = getvar id scope :?> obj list
          match op with
          | Set ->
            let narr = arr |> List.mapi (fun i v -> if i = index then (evaluate expr scope) else v)
            scope |> updvar id narr
          | PlusEquals -> 
            let v = arr.[index]
            match v with
            | :? int | :? float as _v -> 
              let narr = arr |> List.mapi (fun i v -> if i = index then (_v :?> float + (evaluate expr scope :?> float) :> obj) else v)
              scope |> updvar id narr
            | :? (obj list) as _v -> updvar id (List.append _v [evaluate expr scope]) scope
            | _ -> failwithf $"Invalid operator (+=) for type {v.GetType()}"
          | MinusEquals -> scope
        | _ -> failwithf "Assignment statement expects a varReference, not just any expression, this error should never be raised"
      | Loop (id, expr, block) ->
        let n = evaluate expr scope :?> float
        let rec innerLoop (space: Scope) =
          if (getvar id space :?> float) > n then
            remvar id space
          else
            block
            |> List.fold (fun spc stmt -> execute stmt spc) space
            |> fun spc -> updvar id (getvar id spc :?> float + 1.0) spc
            |> innerLoop
        innerLoop (addvar id 0.0 scope)
      | If (expr, tblock, fblock) ->
        let b = evaluate expr scope :?> bool
        if b then tblock else fblock 
        |> List.fold (fun spc stmt -> execute stmt spc) scope
      | End -> scope
      | Sleep expr -> 
        let timeout = evaluate expr scope :?> int
        System.Threading.Thread.Sleep(timeout)
        scope
      | ProcedureDefinition (id, plist, block) ->
        scope |> addvar id {
          statements = block
          plist = plist
        }
      | ProcedureCall (id, elist) ->
        let proc = scope |> getvar id :?> Method
        if List.length proc.plist = List.length elist then
          let innerScope = {
            parent = Some scope
            self = proc.plist |> List.mapi (fun i v -> v, (evaluate elist.[i] scope)) |> Map.ofSeq
            quit = None
          }
          let _scope = runBlock proc.statements innerScope
          match _scope.parent with
          | Some p -> p
          | None -> failwithf $"somehow the procedure did not have a parent, this exception should never be raised"
        else
          failwithf $"number of parameters passed to \"{id}\" call did not match expected number of params"
      | FunctionDefintion (id, plist, block) ->
        scope |> addvar id {
          statements = block
          plist = plist
        }
      | Return expr -> { scope with quit = Some expr }
      | Use path ->
        $"{path}" // TODO make this path relative to the program file, not to the interpreter
        |> parseModule
        |> bind (fun m -> // TODO push "run" function above so I can use it here
          try
            List.fold (fun s b -> execute b s) defaultScope m |> Success
          with
          | err -> Failure $"[module execution]: {err.ToString()}"
        )
        |> (function
          | Success succ -> 
            let par = 
              match scope.parent with
              | Some p -> { p with self = Map.fold (fun cur key value -> Map.add key value cur) p.self succ.self }
              | None -> succ
            { scope with parent = Some par }
          | Failure fail -> failwithf $"{fail}"
        )

    let run program =
      try
        List.fold (fun space block -> execute block space) defaultScope program
        |> Success
      with
      | err -> Failure ("[execution]: " + err.ToString())

    let finish = function
      | Success _ -> printfn "\nProgram finished execution without errors."
      | Failure fail -> printfn $"{fail}"

    let debug res =
      match res with
      | Success succ -> printfn $"[debug]: {succ}"
      | Failure _ -> ()
      res

    let interpret path =
      path
      |> parse 
      // |> debug
      |> bind run 
      // |> debug
      |> finish
