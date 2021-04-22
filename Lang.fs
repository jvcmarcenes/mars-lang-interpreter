module Lang

  open System
  open FParsec

  open Railway

  module Model =
    // not in use yet // type VarType = Int | Float | String | Array | Any

    type Identifier = string

    type ParamList = string list // alias for procedure and function parameters

    type AssignmentOperation = Set | PlusEquals | MinusEquals

    type UnaryOperation = NumberNegation | Identity | BooleanNegation

    type BinaryOperation =
      | Add | Sub | Mul | Div | Mod // arithmetic operators
      | Equals | NotEquals // equality operators
      | And | Or // boolean operators
      | GreaterThan | GreaterThanOrEquals | LesserThan | LesserThanOrEquals //comparison operators

    type SymbolReference =
      | VarReference of Identifier
      | ArrayAccess of Identifier * Expression
      | RecursiveAccess of SymbolReference * SymbolReference

    and Expression =
      // Literals
      | StringLiteral of string
      | IntLiteral of float
      | FloatLiteral of float
      | BooleanLiteral of bool
      | ArrayLiteral of Expression list
      | MapLiteral of Map<Identifier, Expression>
      | FunctionDefinition of ParamList * Block
      | ProcedureDefinition of ParamList * Block
      // Input
      | Read
      | ReadNumber
      // Operations
      | Unary of UnaryOperation * Expression
      | Binary of BinaryOperation * Expression * Expression
      | Group of Expression
      // Symbol References
      | SymbolAccess of SymbolReference
      | FunctionCall of SymbolReference * Expression list
      // Others
      | Random of Expression
      | Use of string
      | Empty

    and Statement =
      // Output
      | Write of Expression
      | WriteLine of Expression
      // Scope
      | Definition of Identifier * Expression
      | Assignment of AssignmentOperation * SymbolReference * Expression
      | Return of Expression
      // Flow Structure
      | If of Expression * Block * Block
      | Loop of Identifier * Expression * Block
      | ProcedureCall of SymbolReference * Expression list
      // Others
      | Comment
      | Sleep of Expression

    and Block = Statement list

    type Method = 
      {
        statements: Block
        plist: ParamList
      }
      static member create block plist = { statements = block; plist = plist }

    type Scope = { 
      parent: Option<Scope>
      self: Map<Identifier, obj>
      quit: Option<Expression> // TODO rethink how return statements work?
    }

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

    let rec either = function
      | p1::tail -> p1 <?|> either tail
      | [] -> pzero

    // helper method for statements with 3 values
    let fix f ((a, b), c) = f (a, b, c)

    // Characters Constructs
    let whitespace = pchar ' ' <|> tab |>> ignore
    let ws = skipMany whitespace
    let ws1 = skipMany1 whitespace

    let nl = skipMany1 (ws .>> (skipNewline <|> skipChar ';') .>> ws)

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

    let followedByEof = followedByL eof "end of file"
    //

    let id = identifier (new IdentifierOptions()) |>> fun s -> s

    let statement, statementRef = createParserForwardedToRef<Statement, unit>()

    let blockEndBy endp = ws >>. manyTill (statement .>> (nl <|> followedByEof)) (ws >>. endp)

    // Expressions
    let opp = OperatorPrecedenceParser<Expression, _, _>()

    let expr = opp.ExpressionParser

    let stringLiteral = quote >>. manyCharsTill anyChar quote |>> StringLiteral 
    let intLiteral = pint32 |>> fun i -> IntLiteral (float i)
    let floatLiteral = pfloat |>> FloatLiteral
    let boolLiteral = stringReturn "true" true <|> stringReturn "false" false |>> BooleanLiteral

    let arrayLiteral = lsbr >>. sepBy expr comma .>> rsbr |>> ArrayLiteral
    let mapLiteral = lbra >>. manyTill (spaces >>. id .>> ws .>> skipChar '=' .>> ws .>>. expr .>> nl) rbra |>> (Map.ofList >> MapLiteral)

    let paramlist = (ws1 >>. sepBy id ws1) <|> (ws >>. preturn []) .>> ws
    let funcdef = skipString "function" >>. paramlist .>> nl .>>. blockEndBy endtag |>> FunctionDefinition
    let procdef = skipString "procedure" >>. paramlist .>> nl .>>. blockEndBy endtag |>> ProcedureDefinition

    let read = stringReturn "read" Read
    let readnumber = stringReturn "readnumber" ReadNumber
    let random = skipString "random" >>. ws1 >>. expr |>> Random

    let symbolref = 
      sepBy1 (id .>>. opt (lsbr >>. expr .>> rsbr)) (skipChar '.') 
      |>> fun symbolList ->
        let rec createSymbolRef map rest = match rest with first::second -> RecursiveAccess (map, createSymbolRef first second) | [] -> map
        symbolList
        |> List.map (fun (id, oacc) -> match oacc with Some expr -> ArrayAccess (id, expr) | None -> VarReference id) 
        |> (function h::t -> createSymbolRef h t | [] -> failwithf $"the parser sepBy1 will make sure that there is at least one element, this exception should never be raised")

    let symbolaccess = symbolref |>> SymbolAccess

    // let methodcaller = (ws1 >>. sepBy1 expr (followedBy expr)) <?|> (stringReturn "!" []) // this method caller should not parse expr as funccalls 
    let methodcaller = (skipChar '!' >>. sepBy expr (followedBy expr))

    let funccall = symbolref .>>. methodcaller |>> FunctionCall

    let group = lpar >>. expr .>> rpar |>> Group

    let _use = skipString "use" >>. ws1 >>. quote >>. manyCharsTill anyChar quote |>> Use

    opp.TermParser <- ws >>. either [
      stringLiteral <??> "String Literal"
      floatLiteral <??> "Float Litereal"
      intLiteral <??> "Integer Literal"
      boolLiteral <??> "Boolean Literal"
      arrayLiteral <??> "Array Literal"
      mapLiteral <??> "Map Literal"
      funcdef <??> "Function Definition"
      procdef <??> "Procedure Definition"
      readnumber <??> nameof readnumber
      read <??> nameof read
      random <??> nameof random
      _use <??> "Use Expression"
      funccall <??> "Function Call"
      symbolaccess <??> "Symbol Reference"
      group <??> "Parenthesis Grouping"
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

    let assignops = either [equassign; addassign; minassign]

    // Statements
    let singlecomment = dslash >>. restOfLine false >>% Comment
    let multicomment = skipString "/*" >>. skipManyTill skipAnyChar (skipString "*/") >>% Comment
    let comment = singlecomment <|> multicomment

    let writeline = skipString "writeline" >>. (ws1 >>. expr <|> empty) |>> WriteLine
    let write = skipString "write" >>. ws1 >>. expr |>> Write

    let definition = skipString "let" >>. ws1 >>. id .>> ws .>> skipChar '=' .>> ws .>>. expr |>> Definition
    let assignment = symbolref .>> ws .>>. assignops .>> ws .>>. expr |>> fun ((s, o), e) -> Assignment (o, s, e)

    let sleep = skipString "sleep" >>. ws1 >>. intLiteral |>> Sleep

    let loop = skipString "loop" >>. ws1 >>. id .>> ws1 .>>. expr .>> nl .>>. blockEndBy endtag |>> fix Loop 

    let ifstmt, ifstmtRef = createParserForwardedToRef<Statement, unit>()
    let elseblock = skipString "else" >>. ((ws1 >>. ifstmt |>> fun s -> [s]) <|> (ws >>. nl >>. blockEndBy endtag))
    do ifstmtRef := skipString "if" >>. ws1 >>. expr .>> nl .>>. blockEndBy (endtag <|> followedByString "else") .>>. (elseblock <|> preturn []) |>> fix If

    let proccall = symbolref .>>. methodcaller |>> ProcedureCall

    let ret = skipString "return" >>. ws1 >>. expr |>> Return

    do statementRef := spaces >>. either [
          writeline <??> nameof writeline
          write <??> nameof write
          comment <??> "Comment"
          ret <??> nameof ret
          sleep <??> nameof sleep
          definition <??> "Symbol Definition"
          loop <??> "Loop Statement"
          ifstmt  <??> "If Statement"
          assignment <??> "Symbol Assignment"
          proccall <??> "Procedure Call"
        ] .>> ws // .>> optional (followedByL eof "end of file")
    //

    // Module
    // let blockEndBy endp = ws >>. manyTill (statement .>> (nl <|> followedByEof)) (ws >>. endp)

    let moduleParser = skipString "module" >>. ws >>. nl >>. manyTill (spaces >>. (definition <?|> comment) .>> ws .>> (nl <|> followedByEof)) (ws >>. eof)

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

    let path = ref ""

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

    let rec getparent par ref =
      match ref with
      | VarReference _ -> par
      | ArrayAccess _ -> par
      | RecursiveAccess (head, tail) -> getparent (Some head) tail

    let rec execute stmt scope =
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

      let rec evaluate expr scope =
        let rec dereference ref scope = 
          match ref with
          | VarReference id -> getvar id scope
          | ArrayAccess (id, expr) -> 
            let index = int (evaluate expr scope :> obj :?> float)
            (getvar id scope :?> obj list).[index]
          | RecursiveAccess (head, tail) -> // I don't like how I have to create a dummy scope here
            dereference tail { parent = None; self = (dereference head scope :?> Map<Identifier, obj>); quit = None }

        match expr with
        | StringLiteral s -> s :> obj
        | IntLiteral n -> n :> obj
        | FloatLiteral n -> n :> obj
        | BooleanLiteral b -> b :> obj
        | ArrayLiteral arr -> arr |> List.map (fun expr -> evaluate expr scope) :> obj
        | MapLiteral m -> m |> Map.map (fun _ expr -> evaluate expr scope) :> obj
        | FunctionDefinition (plist, block) -> Method.create block plist :> obj
        | ProcedureDefinition (plist, block) -> Method.create block plist :> obj
        | SymbolAccess symbolref -> dereference symbolref scope
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
        | FunctionCall (ref, elist) ->
          let func = dereference ref scope :?> Method
          if List.length func.plist <> List.length elist then failwithf $"number of parameters passed to \"{id}\" call did not match expected number of params"
          let parent = 
            getparent None ref
            |> (function Some pref -> { parent = Some scope; self = dereference pref scope :?> Map<Identifier, obj>; quit = None } | None -> scope)
          { parent = Some parent; self = func.plist |> List.mapi (fun i  v -> v, (evaluate elist.[i] scope)) |> Map.ofSeq; quit = None }
          |> runBlock func.statements
          |> (fun _scope -> match _scope.quit with Some rexpr -> evaluate rexpr _scope | None -> failwith $"function \"{id}\" did not return a value")
        | Use _path ->
          let dirpath = 
            (!path).Split('/')
            |> Array.rev |> (fun arr -> if Array.length arr > 1 then Array.tail arr else [|"./"|]) |> Array.rev |> (fun s -> String.Join('/', s))
          $"{dirpath}/{_path}"
          |> parseModule
          |> bind (fun _module -> // TODO push "run" function above so I can use it here
            try
              List.fold (fun _scope stmt -> execute stmt _scope) defaultScope _module |> Success
            with
            | err -> Failure $"[module execution]: {err.ToString()}"
          )
          |> (function Success succ -> succ.self :> obj | Failure fail -> failwithf $"{fail}")

      let evaluateToInt expr space = int32 (evaluate expr space :?> float)

      let rec dereference ref scope = 
        match ref with
        | VarReference id -> getvar id scope
        | ArrayAccess (id, expr) -> (getvar id scope :?> obj list).[int (evaluate expr scope :?> float)]
        | RecursiveAccess (head, tail) -> dereference tail { parent = None; self = (dereference head scope :?> Map<Identifier, obj>); quit = None }

      match stmt with
      | Comment -> scope
      | Write expr -> printf $"{evaluate expr scope}"; scope
      | WriteLine expr -> printfn $"{evaluate expr scope}"; scope
      | Definition (id, expr) -> scope |> addvar id (evaluate expr scope)
      | Assignment (op, ref, expr) ->
        let rec update ref value _scope =
          match ref with
          | VarReference id -> updvar id value _scope
          | ArrayAccess (id, iexpr) -> 
            let index = evaluateToInt iexpr scope
            let newarr = (getvar id _scope :?> obj list) |> List.mapi (fun i v -> if i = index then value else v)
            updvar id newarr _scope
          | RecursiveAccess (head, tail) -> 
            match head with
            | VarReference hid -> 
              let map = dereference head _scope :?> Map<Identifier, obj>
              updvar hid (update tail value { parent = None; self = map; quit = None }).self _scope
            | ArrayAccess (hid, iexpr) -> 
              let map = (dereference head _scope :?> obj list).[evaluateToInt iexpr scope] :?> Map<Identifier, obj>
              updvar hid (update tail value { parent = None; self = map; quit = None }).self _scope
            | RecursiveAccess _ -> failwithf "head of recursive access should not be a recursive access, this exception should never be raised"
        let _value = evaluate expr scope
        let curvalue = dereference ref scope
        let newvalue =
          match op with
          | Set -> _value
          | PlusEquals ->
            match curvalue with
            | :? int | :? float | :? string as _c -> addop _c _value
            | :? (obj list) as _c -> 
              if _value :? (obj list) then List.append _c (_value :?> obj list) :> obj
              else List.append _c [_value] :> obj
            | _ -> failwithf $"Invalid operator (+=) for type {curvalue.GetType()}"
          | MinusEquals ->
            match curvalue with
            | :? int | :? float as _c -> oper<float, float> (-) _c _value
            | :? (obj list) as _c -> 
              let rem = int (_value :?> float)
              if rem < 0 || rem >= List.length _c then failwithf "index out of bounds"
              _c |> List.mapi (fun i v -> i <> rem, v) |> List.filter fst |> List.map snd :> obj
            // | :? Map<Identifier, obj> as _c -> Map.remove (_value :?> string) _c :> obj
            | _ -> failwithf $"Invalid operator (+=) for type {curvalue.GetType()}"
        update ref newvalue scope
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
      | Sleep expr -> 
        let timeout = evaluate expr scope :?> int
        System.Threading.Thread.Sleep(timeout)
        scope
      | ProcedureCall (ref, elist) ->
        let func = dereference ref scope :?> Method
        if List.length func.plist <> List.length elist then failwithf $"number of parameters passed to \"{id}\" call did not match expected number of params"
        let parent = 
          getparent None ref
          |> (function Some pref -> { parent = Some scope; self = dereference pref scope :?> Map<Identifier, obj>; quit = None } | None -> scope)
        { parent = Some parent; self = func.plist |> List.mapi (fun i  v -> v, (evaluate elist.[i] scope)) |> Map.ofSeq; quit = None }
        |> runBlock func.statements
        |> (fun _scope -> match _scope.parent with Some p -> p | None -> failwithf $"somehow the procedure did not have a parent, this exception should never be raised")
      | Return expr -> { scope with quit = Some expr }
      

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

    let interpret _path =
      do path := _path

      !path
      |> parse 
      // |> debug
      |> bind run 
      // |> debug
      |> finish
