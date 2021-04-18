module Lang

  open System
  open FParsec

  open Railway

  module Model =
    type VarType = Int | Float | String | Array | Any

    type Identifier = string

    type Scope = {
      parent: Option<Scope>
      self: Map<Identifier, obj>
    }

    type ParamList = string list // alias for procedure and function parameters

    type AssignmentOperation = Set | PlusEquals | MinusEquals

    type UnaryOperation = NumberNegation | Identity | BooleanNegation

    type BinaryOperation =
      | Add | Sub | Mul | Div | Mod // arithmetic operators
      | Equals | NotEquals // equality operators
      | And | Or // boolean operators
      | GreaterThan | GreaterThanOrEquals | LesserThan | LesserThanOrEquals //comparison operators

    type Expression = // types of expression, evaluates to a value
      | StringLiteral of string
      | IntLiteral of float
      | FloatLiteral of float
      | BooleanLiteral of bool
      | ArrayLiteral of Expression list
      | Read
      | ReadInt
      | ReadFloat
      | Random of Expression
      | VarReference of Identifier
      | ArrayAccess of Identifier * Expression
      | Unary of UnaryOperation * Expression
      | Binary of BinaryOperation * Expression * Expression
      | Group of Expression
      | Empty

    type Statement = // types os statements, get executed
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

    and Block = Statement list // type alias for a list of statements

    type Procedure = {
      statements: Block
      plist: ParamList
    }

    type Program = {
      statements: Block
      space: Scope // Program very likely doesn't need to keep VariableScope
    }

  module Parser =
    open Model

    // helper method for statements with more than 3 values
    let fix f ((a, b), c) = f (a, b, c)

    // Characters Constructs
    let ws = skipMany (skipChar ' ')
    let ws1 = skipMany1 (skipChar ' ')

    let nl = skipMany1 skipNewline

    let quote = skipChar '"'
    let comma = skipChar ','

    let dslash = skipString "//"

    let lpar = skipChar '('
    let rpar = skipChar ')'
    let lsbr = skipChar '['
    let rsbr = skipChar ']'

    let endtag = skipString "end"

    let empty = followedByNewline <|> notFollowedByEof >>% Empty
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

    let read = stringReturn "read" Read
    let readint = stringReturn "readint" ReadInt
    let readfloat = stringReturn "readfloat" ReadFloat
    let random = skipString "random" >>. ws1 >>. expr |>> Random

    let varRef = id .>>. opt (lsbr >>. expr .>> rsbr) |>> fun (i, a) -> match a with Some a -> ArrayAccess (i, a) | None -> VarReference i

    let group = lpar >>. expr .>> rpar |>> Group

    opp.TermParser <- ws >>. choice [
        stringLiteral; floatLiteral; intLiteral; boolLiteral; arrayLiteral; 
        readint; readfloat; read; random; varRef; group
      ] .>> ws

    let unary op x = Unary (op, x)
    let binary op x y = Binary (op, x, y)

    opp.AddOperator <| InfixOperator("+", ws, 3, Associativity.Left, binary Add)
    opp.AddOperator <| InfixOperator("-", ws, 3, Associativity.Left, binary Sub)
    opp.AddOperator <| InfixOperator("*", ws, 4, Associativity.Left, binary Mul)
    opp.AddOperator <| InfixOperator("/", ws, 4, Associativity.Left, binary Div)
    opp.AddOperator <| InfixOperator("%", ws, 3, Associativity.Left, binary Mod)
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

    let assignops = choice [equassign; addassign]

    // Statements
    let statement, statementRef = createParserForwardedToRef<Statement, unit>()

    let singlecomment = dslash >>. skipManyTill skipAnyChar (skipNewline <|> eof) >>% Comment
    let multicomment = skipString "/*" >>. skipManyTill skipAnyChar (skipString "*/") >>% Comment
    let comment = singlecomment <|> multicomment

    let writeline = skipString "writeline" >>. (ws1 >>. expr <|> empty) |>> WriteLine
    let write = skipString "write" >>. ws1 >>. expr |>> Write

    let definition = skipString "let" >>. ws1 >>. id .>> ws .>> skipChar '=' .>> ws .>>. expr |>> Definition
    let assignment = varRef .>> ws .>>. assignops .>> ws .>>. expr |>> fun ((i, o), e) -> Assignment (o, i, e)

    let sleep = skipString "sleep" >>. ws1 >>. intLiteral |>> Sleep

    let blockEndBy endp = ws >>. manyTill statement (ws >>. endp)

    let loop = skipString "loop" >>. ws1 >>. id .>> ws1 .>>. expr .>> nl .>>. blockEndBy endtag |>> fix Loop 

    let elseblock = skipString "else" >>. nl >>. blockEndBy endtag
    let ifstmt = skipString "if" >>. ws1 >>. expr .>> nl .>>. blockEndBy (endtag <|> followedByString "else") .>>. (elseblock <|> preturn []) |>> fix If

    let procdef = skipString "procedure" >>. ws1 >>. id .>>. ((ws1 >>. sepBy id ws1) <|> preturn []) .>> nl .>>. blockEndBy endtag |>> fix ProcedureDefinition
    let proccall = id .>>. ( stringReturn "!" [] <|> (ws1 >>. sepBy expr ws1)) |>> ProcedureCall

    do statementRef := spaces >>. choice [writeline; write; comment; definition; procdef; proccall; loop; ifstmt; sleep; assignment] .>> spaces .>> optional (followedByL eof "end of file")
    //

    // Program parsing
    let createProgram s = {
      statements = s
      space = {
        parent = None
        self = Map.empty
      }
    }

    let program = blockEndBy eof |>> createProgram

    let parse path =
      match runParserOnFile program () path Text.Encoding.UTF8 with
      | ParserResult.Success (res, _, _) -> Success res
      | ParserResult.Failure (err, _, _) -> Failure ("[parser]: " + err)
    
  module Interpreter =
    open Model
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

    let hasvar id scope = Map.containsKey id scope.self
    let recscope id scope act sact fact =
      if hasvar id scope then act ()
      else match scope.parent with Some p -> sact p | None -> fact ()
    let addvar id value scope = if hasvar id scope then failwithf $"{id} is already defined" else { scope with self = Map.add id (value :> obj) scope.self}
    let remvar id scope = { scope with self = Map.remove id scope.self }
    let rec updvar id value scope = 
      recscope id scope (fun _ -> { scope with self = Map.change id (function Some _ -> Some (value :> obj) | None -> failwithf $"{id} is not defined") scope.self }) (fun p -> updvar id value p) (fun _ -> failwithf $"{id} is not defined")
    let rec getvar id scope = 
      recscope id scope (fun _ -> Map.find id scope.self) (fun p -> getvar id p) (fun _ -> failwithf $"{id} is not defined")

    let rec evaluate expr (scope: Scope) =
      match expr with
      | StringLiteral s -> s :> obj
      | IntLiteral n -> n :> obj
      | FloatLiteral n -> n :> obj
      | BooleanLiteral b -> b :> obj
      | ArrayLiteral arr -> arr |> List.map (fun expr -> evaluate expr scope) :> obj
      | VarReference id -> getvar id scope
      | ArrayAccess (id, expr) -> 
        let index = int32 (evaluate expr scope :?> float)
        (getvar id scope :?> obj list).[index]
      | Read -> Console.ReadLine() :> obj
      | ReadInt -> int32 (Console.ReadLine ()) :> obj
      | ReadFloat -> float (Console.ReadLine ()) :> obj
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

    let evaluateToInt expr space = int32 (evaluate expr space :?> float)

    let rec execute stmt (scope: Scope) =
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
            | :? int | :? float as _v -> updvar id (_v :?> float + (evaluate expr scope :?> float) :> obj) scope
            | :? (obj list) as _v -> updvar id (List.append _v [evaluate expr scope]) scope
            | _ -> failwithf $"Invalid operator (+=) for type {v.GetType()}"
          | MinusEquals -> scope
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
        let proc = scope |> getvar id :?> Procedure
        if List.length proc.plist = List.length elist then
          let innerScope = {
            parent = Some scope
            self = proc.plist |> List.mapi (fun i v -> v, (evaluate elist.[i] scope)) |> Map.ofSeq
          }
          proc.statements |> List.fold (fun s b -> execute b s) innerScope |> ignore
          scope
        else
          failwithf $"number of parameters passed to \"{id}\" call did not match expected number of params"
    
    let run program =
      try
        List.fold (fun space block -> execute block space) program.space program.statements |> ignore
        Success ()
      with
      | err -> Failure ("[execution]: " + err.ToString())

    let finish = function
      | Success _ -> printfn "\nProgram finished execution without errors."
      | Failure fail -> printfn $"{fail}"

    let interpret path =
      path
      |> parse
      |> bind run
      |> finish
