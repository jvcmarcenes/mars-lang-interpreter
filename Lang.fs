module Lang

  open System
  open FParsec

  open Railway

  module Model =
    // not in use yet // type VarType = Int | Float | String | Array | Any

    type Identifier = string

    type AssignmentOperation = Set | PlusEquals | MinusEquals

    type UnaryOperation = NumberNegation | Identity | BooleanNegation

    type BinaryOperation =
      | Add | Sub | Mul | Div | Mod // arithmetic operators
      | Equals | NotEquals // equality operators
      | And | Or // boolean operators
      | GreaterThan | GreaterThanOrEquals | LesserThan | LesserThanOrEquals //comparison operators
      // | Access

    type SymbolReference =
      | VarReference of Identifier
      | ArrayAccess of SymbolReference * Expression
      | RecursiveAccess of SymbolReference * SymbolReference

    and Expression =
      // Literals
      | ObjectValue of obj
      | StringLiteral of string
      | TemplateString of Expression list
      | NumLiteral of float
      | BooleanLiteral of bool
      | ArrayLiteral of Expression list
      | MapLiteral of Map<Identifier, Expression>
      | FunctionDefinition of Identifier list * Block
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

    and Statement = // merge statements and expressions
      // Output
      | Write of Expression
      | WriteLine of Expression
      // Scope
      | Definition of Identifier * Expression
      | Assignment of AssignmentOperation * SymbolReference * Expression
      | Return of Expression
      // Flow Control
      | If of Expression * Block * Block
      | Continue
      | Break
      | Loop of Block
      | FunctionCallVoid of SymbolReference * Expression list
      // Others
      | Comment
      | Sleep of Expression // remove
      | Fail of Expression

    and Block = Statement list

    type Method = 
      {
        Statements: Block
        Plist: Identifier list
      }
      static member Create block plist = { Statements = block; Plist = plist }

    type ScopeControl =
      | SReturn of Expression
      | SContinue
      | SBreak

    type Scope = { 
      Parent: Option<Scope>
      Self: Map<Identifier, obj>
      Control: Option<ScopeControl> // TODO rethink how return statements work?
    }

    module Scope =

      let defaultScope = {
        Parent = None
        Self = Map.empty
        Control = None
      }

      let hasvar id scope = 
        Map.containsKey id scope.Self

      let updscope f scope = { scope with Self = f scope.Self }

      let recscope id scope act pact fact =
        if hasvar id scope then act ()
        else match scope.Parent with Some p -> pact p | None -> fact ()

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
        let act () = Map.find id scope.Self
        let pact p = getvar id p
        let fact () = failwithf $"{id} is not defined"
        recscope id scope act pact fact

  module Parser =
    open Model

    // 
    let (<?|>) p1 p2 = attempt p1 <|> p2

    let either list = List.reduce (<?|>) list

    let enclosed popen pclose p = popen >>. manyTill p pclose

    // helper method for statements with 3 values
    let fix f ((a, b), c) = f (a, b, c)

    // Characters Constructs
    let whitespace = pchar ' ' <|> tab |>> ignore
    let ws = skipMany whitespace
    let ws1 = skipMany1 whitespace

    let fnl = skipMany1 (ws .>> skipNewline >>. ws)
    let onl = ws .>> skipMany (ws .>> skipNewline .>> ws)
    let nl = skipMany1 (ws .>> (skipNewline <|> skipChar ';') .>> ws)

    let comma = skipChar ','
    let dot   = skipChar '.'

    let lpar = skipChar '('
    let rpar = skipChar ')'
    let lsbr = skipChar '['
    let rsbr = skipChar ']'
    let lbra = skipChar '{'
    let rbra = skipChar '}'

    let endtag = skipString "end"

    let followedByEof = followedByL eof "end of file"

    let empty = nl <?|> followedByEof >>% Empty
    //

    let id = identifier (IdentifierOptions ())

    let statement, statementRef = createParserForwardedToRef<Statement, unit>()

    let blockEndBy endp = ws >>. manyTill (statement .>> (nl <|> followedByEof)) (ws >>. endp)

    // Expressions
    let opp = OperatorPrecedenceParser<Expression, _, _>()

    let expr = opp.ExpressionParser

    let clistToString = function [] -> "" | c -> c  |> List.map string |> List.reduce (+)
    let stringParser = enclosed (skipChar '"') (skipChar '"') anyChar |>> clistToString
    
    let stringLiteral = stringParser |>> StringLiteral
    let templateString = enclosed (skipChar ''') (skipChar ''') ((skipString "#{" >>. expr .>> skipString "}") <?|> ((manyTill anyChar (followedByString "'" <?|> followedByString "#{")) |>> (clistToString >> StringLiteral))) |>> TemplateString

    let numLiteral = pfloat |>> NumLiteral
    let boolLiteral = stringReturn "true" true <|> stringReturn "false" false |>> BooleanLiteral

    let separator = either [comma .>> nl; comma; nl]
    let sepOrFollowedBy other = ws >>. (separator <?|> followedBy other) .>> ws

    let arrayLiteral = lsbr >>. onl >>. manyTill (ws >>. expr .>> sepOrFollowedBy rsbr) rsbr |>> ArrayLiteral
    let mapLiteral = lbra >>. onl >>. manyTill (ws >>. id .>> ws .>> skipChar '=' .>> ws .>>. expr .>> sepOrFollowedBy rbra) rbra |>> (Map.ofList >> MapLiteral)

    let paramlist = (ws1 >>. sepBy id ws1) <|> (ws >>. preturn []) .>> ws
    let funcdef = skipString "function" >>. paramlist .>> nl .>>. blockEndBy endtag |>> FunctionDefinition

    let read = stringReturn "read" Read
    let readnumber = stringReturn "readnumber" ReadNumber
    let random = skipString "random" >>. ws1 >>. expr |>> Random

    let symbolref = 
      sepBy1 (id .>>. many (lsbr >>. expr .>> rsbr)) dot 
      |>> fun symbolList ->
        let rec createSymbolRef map rest = match rest with first::second -> RecursiveAccess (map, createSymbolRef first second) | [] -> map
        symbolList
        |> List.map (fun (id, oacc) -> 
          let rec loop arrayAccesses =
            match arrayAccesses with 
            | head::tail -> ArrayAccess (loop tail, head)
            | [] -> VarReference id
          loop (List.rev oacc)
        )
        |> (function h::t -> createSymbolRef h t | [] -> failwithf $"the parser sepBy1 will make sure that there is at least one element, this exception should never be raised")

    let symbolaccess = symbolref |>> SymbolAccess

    // let methodcaller = (ws1 >>. sepBy1 expr (followedBy expr)) <?|> (stringReturn "!" []) // this method caller should not parse expr as funccalls 
    // let methodcaller = ws1 >>. sepBy expr (followedBy expr)
    let methodcaller = (skipChar '!' >>. sepBy expr (followedBy expr))

    let funccall = symbolref .>>. methodcaller |>> FunctionCall

    let group = lpar >>. expr .>> rpar |>> Group

    let _use = skipString "use" >>. ws1 >>. stringParser |>> Use

    opp.TermParser <- ws >>. either [
      templateString <??> "Template String"
      stringLiteral <??> "String Literal"
      numLiteral <??> "Numeral Litereal"
      boolLiteral <??> "Boolean Literal"
      arrayLiteral <??> "Array Literal"
      mapLiteral <??> "Map Literal"
      funcdef <??> "Function Definition"
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

    opp.AddOperator <| PrefixOperator("-", notFollowedBy ws1, 3, true, unary NumberNegation)
    opp.AddOperator <| PrefixOperator("+", notFollowedBy ws1, 3, true, unary Identity)
    opp.AddOperator <| PrefixOperator("!", notFollowedBy ws1, 1, true, unary BooleanNegation)

    // Try to move '.' (accessor operator), [] (array accessor), and '!' (function caller) to operations??
    //

    let equassign = stringReturn "=" Set
    let addassign = stringReturn "+=" PlusEquals
    let minassign = stringReturn "-=" MinusEquals

    let assignops = either [equassign; addassign; minassign]

    // Statements
    let singlecomment = skipChar '#' >>. restOfLine false >>% Comment
    let multicomment = skipString "#[" >>. skipManyTill skipAnyChar (skipString "]#") >>% Comment
    let comment = singlecomment <|> multicomment

    let writeline = skipString "writeline" >>. ((ws1 >>. expr) <|> empty) |>> WriteLine
    let write = skipString "write" >>. ws1 >>. expr |>> Write

    let definition = skipString "let" >>. ws1 >>. id .>> ws .>>. ((skipChar '=' .>> ws >>. expr) <?|> empty) |>> Definition
    let assignment = symbolref .>> ws .>>. assignops .>> ws .>>. expr |>> fun ((s, o), e) -> Assignment (o, s, e)

    let sleep = skipString "sleep" >>. ws1 >>. numLiteral |>> Sleep
    let _fail = skipString "fail" >>. ws1 >>. expr |>> Fail

    let _break = skipString "break" >>% Break
    let _continue = skipString "continue" >>% Continue 

    let loop = skipString "loop" >>. nl >>. blockEndBy endtag |>> Loop 

    let ifstmt, ifstmtRef = createParserForwardedToRef<Statement, unit>()
    let elseblock = skipString "else" >>. ((ws1 >>. ifstmt |>> fun s -> [s]) <|> (ws >>. nl >>. blockEndBy endtag))
    do ifstmtRef := skipString "if" >>. ws1 >>. expr .>> nl .>>. blockEndBy (endtag <|> followedByString "else") .>>. (elseblock <|> preturn []) |>> fix If

    let voidcall = symbolref .>>. methodcaller |>> FunctionCallVoid

    let ret = skipString "return" >>. ((ws1 >>. expr) <|> empty) |>> Return

    do statementRef := spaces >>. either [
          writeline <??> nameof writeline
          write <??> nameof write
          comment <??> "Comment"
          ret <??> nameof ret
          sleep <??> nameof sleep
          _fail <??> "Fail Command"
          definition <??> "Symbol Definition"
          _break <??> "Break Command"
          _continue <??> "Continue Command"
          loop <??> "Loop Statement"
          ifstmt  <??> "If Statement"
          assignment <??> "Symbol Assignment"
          voidcall <??> "Funtion Call"
        ] .>> ws
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
        let rec fold statements blockScope =
          match statements with
          | statement::rest -> 
            let resScope = execute statement blockScope
            match resScope.Control with
            | Some _ -> resScope   
            | None -> fold rest resScope
          | [] -> blockScope
        fold block scope

      let rec evaluate expr scope =
        match expr with
        | ObjectValue o -> o
        | StringLiteral s -> s :> obj
        | TemplateString exprList -> exprList |> List.map ((fun x -> evaluate x scope) >> string) |> List.reduce (+) :> obj
        | NumLiteral n -> n :> obj
        | BooleanLiteral b -> b :> obj
        | ArrayLiteral arr -> arr |> List.map (fun expr -> evaluate expr scope) :> obj
        | MapLiteral m -> m |> Map.map (fun _ expr -> evaluate expr scope) :> obj
        | FunctionDefinition (plist, block) -> Method.Create block plist :> obj
        | SymbolAccess symbolref -> dereference symbolref scope
        | Read -> Console.ReadLine() :> obj
        | ReadNumber -> float (Console.ReadLine ()) :> obj
        | Random expr -> float ((System.Random ()).Next() % (int (evaluate expr scope :?> float))) :> obj
        | Unary (op, x) ->
          let _x = evaluate x scope
          match op with
          | NumberNegation -> -(_x :?> float) :> obj
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
        | Group expr -> evaluate expr scope
        | Empty -> () :> obj
        | FunctionCall (ref, elist) ->
          let func = try dereference ref scope :?> Method with e -> failwith $"{getname ref scope} is not a function and cannot be called"
          if List.length func.Plist <> List.length elist then failwithf $"number of parameters passed to \"{getname ref scope}\" call did not match expected number of params"
          let parent = 
            getparent None ref
            |> (function
              | Some pref -> 
                match (dereference pref scope) with 
                | :? Map<Identifier, obj> as map -> { Parent = Some scope; Self = map; Control = None }
                | _ -> scope
              | None -> scope
            )
          { Parent = Some parent; Self = func.Plist |> List.mapi (fun i  v -> v, (evaluate elist.[i] scope)) |> Map.ofSeq; Control = None }
          |> runBlock func.Statements
          |> (fun _scope -> 
            match _scope.Control with 
            | Some control -> 
              match control with
              | SReturn expression -> evaluate expression _scope
              | SBreak -> failwith $"unexpected break command"
              | SContinue -> failwith $"unexpected continue command" 
            | None -> failwith $"function \"{id}\" did not return a value"
          )
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
          |> (function Success succ -> succ.Self :> obj | Failure fail -> failwithf $"{fail}")

      and dereference ref scope = 
        match ref with
        | VarReference id -> getvar id scope
        | ArrayAccess (head, expr) ->
          let index = int (evaluate expr scope :?> float)
          (dereference head scope :?> obj list).[index]
        | RecursiveAccess (head, tail) ->
          let symbol = dereference head scope
          match symbol with
          | :? Map<Identifier, obj> as map -> 
            match tail with
            | VarReference id ->
              match id with
              // | "size" -> float (map |> Map.toList |> List.length) :> obj
              | "toList" -> Method.Create [Return (map |> Map.toList |> List.map ((fun (key, value) -> [StringLiteral key; ObjectValue value] |> ArrayLiteral)) |> ArrayLiteral)] [] :> obj
              | _ -> dereference tail { Parent = None; Self = map; Control = None }
            | _ -> dereference tail { Parent = None; Self = map; Control = None } // I don't like that we have to create a dummy scope here
          | :? string as s ->
            match tail with
            | VarReference id ->
              match id with
              | "size" -> float (String.length s) :> obj
              | _ -> failwith $"illegal recursive access"
            | _ -> failwith $"illegal recursive access"
          | :? (obj list) as array -> 
            match tail with
            | VarReference id -> 
              match id with
              | "size" -> float (List.length array) :> obj
              | "iter" -> Method.Create (array |> List.map(fun v -> FunctionCallVoid (VarReference "#f", [ObjectValue v]))) ["#f"] :> obj
              | "iteri" -> Method.Create (array |> List.mapi(fun i v -> FunctionCallVoid (VarReference "#f", [NumLiteral (float i); ObjectValue v]))) ["#f"] :> obj
              | "map" -> Method.Create [Return (array |> List.map (fun v -> FunctionCall (VarReference "#f", [ObjectValue v])) |> ArrayLiteral)] ["#f"] :> obj
              | "filter" ->
                Method.Create [
                  Return (
                    array
                    |> List.filter (fun v -> evaluate (FunctionCall (VarReference "#f", [ObjectValue v])) scope :?> bool)
                    |> List.map ObjectValue
                    |> ArrayLiteral
                  )
                ] ["#f"] :> obj
              | "sort" -> Method.Create [Return(array |> List.map (fun v -> v :?> IComparable) |> List.sort |> List.map (fun v -> ObjectValue (v :> obj)) |> ArrayLiteral)] [] :> obj
              | _ -> failwith $"illegal recursive access"
            | _ -> failwith $"illegal recursive access"
          | _ -> failwith $"illegal recursive access"

      and getname ref scope =
        match ref with
        | VarReference id -> id
        | ArrayAccess (head, expr) -> $"{getname head}[{evaluate expr scope}]"
        | RecursiveAccess (head, tail) -> $"{getname tail}.{getname head}"

      let evaluateToInt expr space = int32 (evaluate expr space :?> float)

      match stmt with
      | Comment -> scope
      | Write expr -> printf $"{evaluate expr scope}"; scope
      | WriteLine expr -> printfn $"{evaluate expr scope}"; scope
      | Definition (id, expr) -> scope |> addvar id (evaluate expr scope)
      | Assignment (op, ref, expr) ->
        let rec update ref value _scope =
          match ref with
          | VarReference id -> updvar id value _scope
          | ArrayAccess (head, iexpr) -> 
            let index = evaluateToInt iexpr scope
            let arr = dereference head _scope :?> obj list
            match head with
            | VarReference id -> updvar id (List.mapi (fun i o -> if i = index then value else o) arr) _scope
            | ArrayAccess (shead, siexpr) -> update (ArrayAccess (shead, siexpr)) (List.mapi (fun i o -> if i = index then value else o) arr) _scope
            | RecursiveAccess _ -> failwith "head of array cannot be a recursive access, this exceptioin should never be raised"
          | RecursiveAccess (head, tail) -> 
            match head with
            | VarReference hid -> 
              let map = dereference head _scope :?> Map<Identifier, obj>
              updvar hid (update tail value { Parent = None; Self = map; Control = None }).Self _scope
            | ArrayAccess (hid, iexpr) -> 
              let map = (dereference head _scope :?> obj list).[evaluateToInt iexpr scope] :?> Map<Identifier, obj>
              update hid (update tail value { Parent = None; Self = map; Control = None }).Self _scope
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
            | :? Map<Identifier, obj> as map ->
              if _value :? (Map<Identifier, obj>) then
                curvalue :?> Map<Identifier, obj>
                |> Map.toList
                |> List.append (Map.toList (_value :?> Map<Identifier, obj>))
                |> Map.ofList
                :> obj
              else failwithf $"Invalid operator (+=) for type {curvalue.GetType()}"
            | _ -> failwithf $"Invalid operator (+=) for type {curvalue.GetType()}"
          | MinusEquals ->
            match curvalue with
            | :? int | :? float as _c -> oper<float, float> (-) _c _value
            | :? (obj list) as _c -> 
              let rem = int (_value :?> float)
              if rem < 0 || rem >= List.length _c then failwithf "index out of bounds"
              _c |> List.mapi (fun i v -> i <> rem, v) |> List.filter fst |> List.map snd :> obj
            | :? Map<Identifier, obj> as _c -> Map.remove (_value :?> string) _c :> obj
            | _ -> failwithf $"Invalid operator (+=) for type {curvalue.GetType()}"
        update ref newvalue scope
      | Break -> { scope with Control = Some SBreak }
      | Continue -> { scope with Control = Some SContinue }
      | Loop block ->
        let rec innerLoop statements blockScope =
          match statements with
          | statement::rest -> 
            let resScope = execute statement blockScope
            match resScope.Control with
            | Some (SReturn _) -> resScope
            | Some SBreak -> { resScope with Control = None }
            | Some SContinue -> innerLoop block { resScope with Control = None }
            | None -> innerLoop rest { resScope with Control = None }
          | [] -> innerLoop block blockScope
        innerLoop block scope
      | If (condition, true_block, false_block) ->
        let flag = evaluate condition scope :?> bool
        if flag then true_block else false_block 
        |> List.fold (fun spc stmt -> execute stmt spc) scope
      | Sleep expr -> 
        let timeout = evaluate expr scope :?> int
        System.Threading.Thread.Sleep(timeout)
        scope
      | Fail expr -> failwith (string (evaluate expr scope))
      | FunctionCallVoid (ref, elist) ->
        let func = try dereference ref scope :?> Method with _ -> failwithf $"{getname ref scope} is not a function and cannot be called"
        if List.length func.Plist <> List.length elist then failwithf $"number of parameters passed to \"{getname ref scope}\" call did not match expected number of params"
        let parent = 
          getparent None ref
          |> (function
            | Some pref -> 
              match (dereference pref scope) with 
              | :? Map<Identifier, obj> as map -> { Parent = Some scope; Self = map; Control = None }
              | _ -> scope
            | None -> scope
          )
        { Parent = Some parent; Self = func.Plist |> List.mapi (fun i  v -> v, (evaluate elist.[i] scope)) |> Map.ofSeq; Control = None }
        |> runBlock func.Statements
        |> (fun _scope -> match _scope.Parent with Some p -> p | None -> failwithf $"somehow the procedure did not have a parent, this exception should never be raised")
      | Return expr -> { scope with Control = Some (SReturn expr) }
      

    let run dev program =
      try
        List.fold (fun space block -> execute block space) defaultScope program
        |> Success
      with
      | err -> Failure ("[execution]: " + if dev then err.ToString () else err.Message)

    let finish = function
      | Failure fail -> printfn $"{fail}"
      | _ -> ()

    let debug f dev res =
      if dev then
        match res with
        | Success succ -> f succ
        | Failure _ -> ()
        res
      else res

    let interpret _path dev =
      do path := _path

      !path
      |> parse 
      // |> debug (fun x -> printfn "[debug]: %A" x) dev
      |> bind (run dev)
      // |> debug (fun x -> printfn "[debug]: {x}") dev
      |> finish
