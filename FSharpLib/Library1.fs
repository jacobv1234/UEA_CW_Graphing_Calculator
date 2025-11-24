// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

module FSInterpreter
    open System
    open System.Collections.Generic

    type terminal = 
        Add | Sub | UnarySub | Mul | Div | Mod | Exp | Dot | SF                 // SF stands for Standard Form
        | Sin | Cos | Tan | Log | Ln
        | Lpar | Rpar | Ass | Var of string | Num of int | X of double    // it's not rude, it's short for Assignment

    let str2lst s = [for c in s -> c]
    let isblank c = System.Char.IsWhiteSpace c
    let isdigit c = System.Char.IsDigit c
    let isalpha c = System.Char.IsLetter c
    let lexError message = raise (System.Exception(sprintf "Lexer error: %s" message))
    let intVal (c:char) = (int)((int)c - (int)'0')
    let parseError message = raise (System.Exception(sprintf "Parser error: %s" message))
    let mathError message = raise (System.Exception(sprintf "Maths error: %s" message))
    let nameError message = raise (System.Exception(sprintf "Name error: %s" message))
    let stackError() = raise (System.Exception("Error: Stack overflow"))


    // get how many of the first characters in a string are 0s
    // used for reading decimals
    // count should have an initial value of 0 passed in
    let rec getLeadingZeroesStep(iStr, count) =
        match iStr with
        | '0' :: tail -> getLeadingZeroesStep(tail, count+1)
        | _ -> (iStr, count)

    let getLeadingZeroes(iStr) = getLeadingZeroesStep(iStr,0)

    let rec removeTrailingZeroes(num) = 
        if (num = 0) then
            0
        elif (num % 10 = 0) then
            removeTrailingZeroes(num / 10)
        else
            num

    let rec scInt(iStr, iVal) = 
        match iStr with
        | c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
        | _ -> (iStr, iVal)

    // convert two integers a, b into double a.b
    // with rules of leading zeroes explained in lexer
    let intsToDouble(a:int, b:int) =
        match b with
        | 0 -> (double) a
        | _ -> let digitsInB = floor(log10((double)b))+1.0
               let truncB = removeTrailingZeroes(b)
               (double)a + (double)truncB*(10.0 ** -(digitsInB))

    // check if a double ends in .0
    let doubleIsInt a = a.Equals(floor(a))

    // find variable names
    let rec scanVarName(iStr, iVal: string) =
        match iStr with
        | c :: tail when isalpha c -> scanVarName(tail, iVal + string c)
        | _ -> (iStr, iVal)

    // replace all instances of Var "x" in a tList with X value
    // used for graph data generation
    let rec replaceX tList (value:double) =
        match tList with
        | [] -> tList
        | Var "x" :: tail -> X value :: replaceX tail value
        | t :: tail -> t :: replaceX tail value




    let lexer input = 
        let rec scan input last_was_digit =
            match input with
            | [] -> []
            | '+'::tail -> Add :: scan tail false
            | '-'::tail -> match last_was_digit with
                            | true -> Sub :: scan tail false
                            | false -> UnarySub :: scan tail false
            | '*'::tail -> Mul :: scan tail false
            | '/'::tail -> Div :: scan tail false
            | '%'::tail -> Mod :: scan tail false
            | '^'::tail -> Exp :: scan tail false
            | '('::tail -> Lpar:: scan tail false
            | ')'::tail -> Rpar:: scan tail false
            | '='::tail -> Ass:: scan tail  false
            | '.'::tail -> Dot :: scan tail true 
            | 'E'::tail -> SF  :: scan tail false // treated as not a number to prevent decimal stuff below
            | 's'::'i'::'n'::'('::tail -> Sin :: scan tail false
            | 'c'::'o'::'s'::'('::tail -> Cos :: scan tail false
            | 't'::'a'::'n'::'('::tail -> Tan :: scan tail false
            | 'l'::'o'::'g'::'('::tail -> Log :: scan tail false
            | 'l'::'n'::     '('::tail -> Ln  :: scan tail false
            | c :: tail when isblank c -> scan tail last_was_digit

            // it seems a bit odd but in order to store leading 0s in decimal numbers they are moved to the end of the number
            // only if the last symbol was a number (i.e. this is a decimal) - this is so eg. 04 -> Num 4 but 23.04 -> Num 23 Dot Num 40
            // there's almost definitely a cleaner way to do this but it works
            | c :: tail when isdigit c -> match last_was_digit with
                                          | true -> let full_str = c :: tail
                                                    let (iStr, count) = getLeadingZeroes(full_str)
                                                    let (iStr, iVal) = scInt(tail, intVal c)
                                                    let trunc_iVal = removeTrailingZeroes(iVal)
                                                    let modified_iVal = trunc_iVal * (int)(10.0 ** ((double)count))
                                                    Num modified_iVal :: scan iStr true
                                          | false -> let (iStr, iVal) = scInt(tail, intVal c)
                                                     Num iVal :: scan iStr true
            
            // letters - variable name
            | c :: tail when isalpha c -> let (tail, var) = scanVarName(tail, string c)
                                          Var var :: scan tail true
                                          


            | _ -> lexError (sprintf "Unrecognised character '%c'" input[0])

        scan (str2lst input) false

    let getInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()

    // Grammar in BNF:
    // <St>       ::= <var> '=' <E> | <E>
    // <E>        ::= <T> <Eopt>
    // <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
    // <T>        ::= <P> <Topt>
    // <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
    // <P>        ::= <S> <Popt>
    // <Popt>     ::= "^" <S> <Popt> | <empty>
    // <S>        ::= <NR> <Sopt>
    // <Sopt>     ::= "E" <NR> | <empty>
    // <NR>       ::= "Num" <value> | "(" <E> ")" | "- (unary)" <NR> | <value> '.' <value> | 
    //                  "sin(" <E> ")" | "cos(" <E> ")" | "tan(" <E> ")" | "log(" <E> ")" | <var> | 'x'

    let parser tList (symbolTable: Map<string, double>) = 
        let rec St tList =                // >> is forward function composition operator: let inline (>>) f g x = g(f x)
            match tList with
            | Var value :: Ass :: tail -> E tail
            | _ -> E tList
        and E tList = (T >> Eopt) tList
        and Eopt tList = 
            match tList with
            | Add :: tail -> (T >> Eopt) tail
            | Sub :: tail -> (T >> Eopt) tail
            | _ -> tList

        and T tList = (P >> Topt) tList
        and Topt tList =
            match tList with
            | Mul :: tail -> (P >> Topt) tail
            | Div :: tail -> (P >> Topt) tail
            | Mod :: tail -> (P >> Topt) tail
            | _ -> tList

        and P tList = (S >> Popt) tList
        and Popt tList = 
            match tList with
            | Exp :: tail -> (S >> Popt) tail
            | _ -> tList

        and S tList = (NR >> Sopt) tList
        and Sopt tList =
            match tList with
            | SF :: tail -> (NR) tail
            | _ -> tList

        and NR tList =
            match tList with 
            | Num value :: tail -> match tail with
                                   | Dot :: Num subvalue :: subtail -> subtail                           // float
                                   | Dot :: subtail -> parseError "Missing value after decimal point"    // incomplete float
                                   | _ -> tail                                                           // int
            | X value :: tail -> tail // special X num used for graph data generation
            | Sin :: tail  -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> parseError "Missing right bracket for sin"
            | Cos :: tail  -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> parseError "Missing right bracket for cos"
            | Tan :: tail  -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> parseError "Missing right bracket for tan"
            | Log :: tail  -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> parseError "Missing right bracket for log"
            | Ln :: tail  -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> parseError "Missing right bracket for ln"
            | Lpar :: tail -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> parseError "Missing right bracket"
            | UnarySub :: tail -> (NR) tail
            | Var varName :: tail -> try
                                        let value = symbolTable.[varName]
                                        tail
                                     with
                                     | :? KeyNotFoundException ->
                                        nameError (sprintf "Unrecognised variable name: '%s'" varName)
                                     
            | _ -> parseError "Unknown syntax error"
        St tList

    // every time values are passed around they are followed by a vIsInt argument
    // this is a boolean. True = the value is an int, False = the value is a double
    // this is used for integer arithmetic as all values are internally doubles

    // for integer arithmetic mode to be active both must have it set to True
    // though most operations don't actually change except division

    // derivMode:
    // 'n' : plot graph normally (or other character)
    // 'd' : plot derivative
    // 'i' : calculate definite integral between xstart and xstop

    let parseNeval tList (symbolTable: Map<string, double>) (typeTable: Map<string, bool>) xstart xstop xstep (derivMode:char) = 
        let rec St tList (symbolTable: Map<string, double>) =
            match tList with
            | Var varName :: Ass :: tail -> match varName with
                                            // reserved variable name
                                            | "x" -> nameError "The variable name 'x' is reserved for graph plots."

                                            // graphing mode
                                            | "y" -> let rec genGraph expr (xstart: double) =
                                                            if xstart >= xstop then
                                                                []
                                                            else
                                                                let x_expr = replaceX expr xstart                             // set value of x
                                                                let tList, result, resIsInt = E x_expr                        // evaluate RHS for value of x

                                                                match derivMode with
                                                                | 'd' -> let dx:double = 0.000001                                    // find derivative by
                                                                         let x_expr = replaceX expr (xstart + dx)                    // evaluate RHS for x + dx
                                                                         let tList, dx_result, dxIsInt = E x_expr
                                                                         let dy = dx_result - result                                 // find dy
                                                                         [xstart, (double) dy/dx] :: genGraph expr (xstart+xstep)    // repeat for next x

                                                                // note that in integration mode xstep is dx
                                                                | 'i' -> let x_expr = replaceX expr (xstart + xstep)                       // evaluate RHS for x + dx
                                                                         let tList, dx_result, dxIsInt = E x_expr
                                                                         let area = abs((double)((result+dx_result) / 2.0) * xstep)        // calculate area of trapezium
                                                                         
                                                                         [xstart, area] :: genGraph expr (xstart+xstep)                    // repeat for next x
                                                                         

                                                                | _ -> [xstart, result] :: genGraph expr (xstart+xstep)       // repeat for next x
                                                                    

                                                     let graphPoints = genGraph tail xstart

                                                     
                                                     (graphPoints, symbolTable, typeTable)
                                                     

                                            // standard assignment
                                            | _ -> let tList, result, resIsInt = E tail // evaluate RHS
                                                   let newSymbolTable = symbolTable |> Map.add varName result
                                                   let newTypeTable = typeTable |> Map.add varName resIsInt
                                                   ([[result,0.0]], newSymbolTable, newTypeTable)
            | _ -> let tList, result, resIsInt = E tList 
                   ([[result,0.0]], symbolTable, typeTable)
        and E tList = (T >> Eopt) tList
        and Eopt (tList, value: double, vIsInt) = 
            match tList with
            | Add :: tail -> let (tLst, tval, tvIsInt) = T tail
                             Eopt (tLst, value + tval, tvIsInt && vIsInt)
            | Sub :: tail -> let (tLst, tval, tvIsInt) = T tail
                             Eopt (tLst, value - tval, tvIsInt && vIsInt)
            | _ -> (tList, value, vIsInt)

        and T tList = (P >> Topt) tList
        and Topt (tList, value: double, vIsInt) =
            match tList with
            | Mul :: tail -> let (tLst, tval, tvIsInt) = P tail
                             Topt (tLst, value * tval, tvIsInt && vIsInt)
            | Div :: tail -> let (tLst, tval, tvIsInt) = P tail
                             match tval with
                             | 0.0 ->  mathError "Division by zero" // check for division by 0
                             | _ -> if (tvIsInt && vIsInt) then
                                        Topt (tLst, floor(value / tval), tvIsInt && vIsInt) // integer division
                                    else
                                        Topt (tLst, value / tval, tvIsInt && vIsInt)        // standard division
            | Mod :: tail -> let (tLst, tval, tvIsInt) = P tail
                             Topt (tLst, value % tval, tvIsInt && vIsInt)
            | _ -> (tList, value, vIsInt)

        and P tList = (S >> Popt) tList
        and Popt (tList, value: double, vIsInt) =
            match tList with
            | Exp :: tail -> let (tLst, tval, tvIsInt) = S tail
                             let result = (value ** tval)
                             Popt (tLst, result, vIsInt && tvIsInt)
            | _ -> (tList, value, vIsInt)

        and S tList = (NR >> Sopt) tList
        and Sopt (tList, value: double, vIsInt) =
            match tList with
            | SF :: tail -> let (tLst, tval, tvIsInt) = NR tail
                            let result = (value * (10.0 ** tval))
                            (tLst, result, doubleIsInt result)
            | _ -> (tList, value, vIsInt)

        and NR tList =
            match tList with 
            | Num value :: tail -> match tail with
                                   | Dot :: Num subvalue :: subtail -> (subtail, intsToDouble(value,subvalue), false) // float
                                   | Dot :: subtail -> parseError "No value after decimal point"                      // incomplete float
                                   | _ -> (tail, value, true)                                                         // int

            | X value :: tail -> (tail, value, false)                                                                 // special number for graphs

            | Sin :: tail  -> let (tLst, tval, tvIsInt) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail, sin(tval), false)    // false as trig functions should always return a double
                              | _ -> parseError "Missing right bracket for sin"
            | Cos :: tail  -> let (tLst, tval, tvIsInt) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail, cos(tval), false)
                              | _ -> parseError "Missing right bracket for cos"
            | Tan :: tail  -> let (tLst, tval, tvIsInt) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail, tan(tval), false)
                              | _ -> parseError "Missing right bracket for tan"
            | Log :: tail  -> let (tLst, tval, tvIsInt) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail, log10(tval), false)
                              | _ -> parseError "Missing right bracket for log"
            | Ln  :: tail  -> let (tLst, tval, tvIsInt) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail, log(tval), false)
                              | _ -> parseError "Missing right bracket for natural log"
            | Lpar :: tail -> let (tLst, tval, tvIsInt) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail, tval, tvIsInt)
                              | _ -> parseError "Missing right bracket"

            | UnarySub :: tail -> let (tLst, tval, tvIsInt) = T tail
                                  Eopt (tLst, 0.0 - tval, tvIsInt)
            | Var varName :: tail -> try
                                        let value = symbolTable.[varName]
                                        let isInt = typeTable.[varName]
                                        (tail, value, isInt)
                                     with
                                     | :? KeyNotFoundException ->
                                        nameError (sprintf "Unrecognised variable name: '%s'" varName)

            | _ -> parseError "Unknown syntax error"
        St tList symbolTable

    let rec printTList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTList tail
                  
        | [] -> Console.Write("EOL\n")
                []

    let calcLine(str: string, symbolTable: Map<string, double>, typeTable: Map<string, bool>, xstart, xstop, xstep, derivMode) =
        let oList = lexer str
        let Out, newSymbolTable, newTypeTable = parseNeval oList symbolTable typeTable xstart xstop xstep derivMode
        (Out, newSymbolTable, newTypeTable)

    let rec processLines(lines: string list, symbolTable: Map<string, double>, typeTable: Map<string, bool>, xstart, xstop, xstep, derivMode) =
        match lines with
        | head::tail -> let Out, newSymbolTable, newTypeTable = calcLine(head, symbolTable, typeTable, xstart, xstop, xstep, derivMode)
                        match tail.Length with
                        | 0 -> Out
                        | _ -> processLines(tail, newSymbolTable, newTypeTable, xstart, xstop, xstep, derivMode)
        | [] -> [[0.0,0.0]] // empty code, default value


    // Evaluate an input string
    let calculate(str: string) =
        let lines = Array.toList(str.Split(';'))

        // F# Map help: https://livebook.manning.com/book/get-programming-with-f-sharp/chapter-17#50
        let symbolTable = 
            ["pi", double 3.141592653589793] // this is the most accurate value of pi that can fit in a double
            |> Map.ofList
        let typeTable =
            ["pi", false]
            |> Map.ofList

        let result = processLines(lines, symbolTable, typeTable, 0.0,10.0,0.01, 'n') // plotting arguments aren't used, just give default values
        // extract answer
        let group = result[0]
        let answer, answer2 = group[0]
        answer

    // Plot the graph for an input string
    // Arguments:
    // str: input code
    // start: initial x value
    // stop: ending x value
    // step: x value precision
    // derivMode: if True calculates derivative graph of str instead of just plotting str
    let plot(str: string, start: double, stop: double, step: double, derivMode: bool) =
        if start >= stop then
            mathError "Ending value should be greater than the starting value."
        if step <= 0 then
            mathError "Step should be greater than 0."
        let lines = Array.toList(str.Split(';'))

        let symbolTable = 
            ["pi", double 3.141592653589793]
            |> Map.ofList
        let typeTable =
            ["pi", false]
            |> Map.ofList
        
        match derivMode with
            | true  -> processLines(lines, symbolTable, typeTable, start,stop,step, 'd')
            | false -> processLines(lines, symbolTable, typeTable, start,stop,step, 'n')


    // Calculate the definite integral for an input string
    // Arguments:
    // str: input code
    // start: initial x value
    // stop: ending x value
    // trapezia: number of trapezia in approximation
    let integral(str: string, start: double, stop: double, trapezia: int) =
        if start >= stop then
            mathError "Ending value should be greater than the starting value."

        let lines = Array.toList(str.Split(';'))

        let symbolTable = 
            ["pi", double 3.141592653589793]
            |> Map.ofList
        let typeTable =
            ["pi", false]
            |> Map.ofList

        let dx = (stop - start) / ((double) trapezia)
        
        
        try
            processLines(lines, symbolTable, typeTable, start,stop,dx, 'i')
        with
        | :? System.StackOverflowException ->
            stackError()


        

    // no longer needed due to having a GUI
    //[<EntryPoint>]
    //let main argv  =
    //    Console.WriteLine("Simple Interpreter")
    //    let input:string = getInputString()
    //    let oList = lexer input
    //    let sList = printTList oList;
    //    let pList = printTList (parser oList)
    //    let tList, Out, isInt = parseNeval oList
    //    Console.WriteLine("Result = {0}", Out)
    //    0

