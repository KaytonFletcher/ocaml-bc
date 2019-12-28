type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string * expr list

type statement = 
    | Assign of string*expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list 
    | Return of expr
    | Break
    | Continue
    | Normal
    | Error of string
    | Printf of float
    | Print of string



type block = statement list 

type env = Tbl of (string, float) Hashtbl.t 

type envQueue = env list


type functions = Functions of (string, string list * statement list) Hashtbl.t

let varEval (_v: string) (_q:envQueue): float  = 
    match (List.hd _q) with
        Tbl(env) -> if(Hashtbl.mem env _v) then
                        Hashtbl.find env _v
                    else 0.0    

let evalOp1 (s: string) (flt: float) (var:string) (_q: envQueue): float =
    match s with
        | "-" ->  flt *. -1.
        | "++" -> begin match (List.hd _q) with 
                    Tbl(env) -> Hashtbl.add env var (flt +. 1.) ; flt +. 1.
                end

        | "--" -> begin match (List.hd _q) with 
                    Tbl(env) -> Hashtbl.add env var (flt -. 1.) ; flt -. 1.
            end
        | "!" -> begin
            if flt != 0.0 then 0.0 else 1.0
            end    
        | _ -> 0.0


let evalOp2 (s: string) (op1: float) (op2: float) : float =
    match s with    
        | "+" -> op1 +. op2
        | "-" -> op1-.op2
        | "*" -> op1*.op2
        | "/" -> op1/.op2
        | "^" -> op1**op2
        | "%" -> mod_float op1 op2
        | "<" -> begin
            match op1<op2 with
                | true -> 1.0
                | false -> 0.0
        end
        | "<=" -> begin
            match op1<=op2 with
                | true -> 1.0
                | false -> 0.0
        end
        | ">" -> begin
            match op1>op2 with
                | true -> 1.0
                | false -> 0.0
        end
        | ">=" -> begin
            match op1>=op2 with
                | true -> 1.0
                | false -> 0.0
        end
        | "==" -> begin
            match op1=op2 with
                | true -> 1.0
                | false -> 0.0
        end
        | "!=" -> begin
            match op1<>op2 with
                | true -> 1.0
                | false -> 0.0
        end
        | "&&" -> begin
            if op1 != 0.0 && op2 != 0.0 then 1.0 else 0.0
        end
        | "||" -> begin
            if op1 != 0.0 || op2 != 0.0 then 1.0 else 0.0
        end
        | _ -> 0.0                  


let evalParams (e: expr list) (s: string list): bool  =
    if(List.length e == List.length s) then true else false  


let rec evalCode (statements: block) (_q:envQueue) (_f: functions): statement =  
   
        if (List.length statements == 0) then Normal
        else 
            (match (evalStatement _q (List.hd statements)) _f with 
            | Return(expr) -> Return(expr)
            | Printf(flt) ->  print_float flt ; print_newline () ; (evalCode (List.tl statements) _q _f)
            | Print(str) -> print_string str  ; (evalCode (List.tl statements) _q _f)
            | Break -> Break
            | Continue -> Continue
            | _ -> (evalCode (List.tl statements) _q _f)
            )

and evalStatement (q:envQueue) (s: statement) (_f: functions): statement =
    match s with 
        | Assign(_v, _e) ->   
           ( match (List.hd q) with
                    (*There is no way for assignment to fail, always normal *)
                 Tbl(env) -> Hashtbl.add env _v (evalExpr _e q _f) ; Normal )
          
        | If(e, codeT, codeF) -> 
            (let cond = evalExpr e q _f in
                if(cond>0.0) then
                   ( match (evalCode codeT q _f) with
                     | Normal -> Normal 
                     | Break -> Break
                     | Continue -> Continue
                     | Printf(f) -> Printf(f) 
                     | Return(expr) -> Return(expr)
                     | _ -> Error ("if broken")
                   ) 
                else  ( match (evalCode codeF q _f) with
                    | Normal -> Normal
                    | Break -> Break
                    | Continue -> Continue
                    | Return(expr) ->  Return(expr)
                    | Printf(f) -> Printf(f) 
                    | _ -> Error ("else broken")
                    )
             )       
        | Expr(expr) ->  Printf (evalExpr expr q _f)
        | While(ex, blk) -> 
        (
            let rec whileloop (e: expr) (blk: statement list) (q:envQueue) (_f: functions) : statement =
                if((evalExpr e q _f) > 0.0) then (
                    match (evalCode blk q _f) with
                        | Normal -> whileloop e blk q _f
                        | Printf(flt) -> if(flt > 0.0) then () ; whileloop e blk q _f
                        | Break -> Break
                        | Return(expr) -> Return(expr)
                        | Continue -> whileloop e blk q _f
                        | _ -> Error ("While loop error")
                ) 
                else Return(Num(0.0))
            in whileloop ex blk q _f        
        )
        | For(st, ex1, st2, blk) -> 
            ( match (evalStatement q st _f) with
                | Normal -> let rec forloop (e: expr) (s: statement) (blk: statement list) (q: envQueue) (_f: functions) : statement = 
                                if((evalExpr e q _f) > 0.0) then

                                ( match (evalCode blk q _f) with
                                    | Normal  -> 
                                        ( match (evalStatement q s  _f) with
                                            | Normal -> forloop e s blk q _f 
                                            | Printf(flt) -> if(flt > 0.0) then () ; forloop e s blk q _f
                                            | _ -> Error ("Forloop error from normal")
                                        )
                                    | Return(expr) -> Return(expr)
                                    | Break -> Break
                                    | Continue -> (match (evalStatement q s _f) with
                                                    | Normal -> forloop e s blk q _f 
                                                    | Printf(flt) -> if(flt > 0.0) then () ; forloop e s blk q _f
                                                    | _ -> Error ("Forloop error from continue")
                                                )
                                    | Printf(f) -> Printf(f)            
                                    | _  -> Error ("undefined forloop behavior") 
                                )   
                                else Normal                
                            in forloop ex1 st2 blk q _f


                | _ -> Error("Syntax error")
            )
        | FctDef(name, params, block) ->( match _f with 
                                            Functions(tbl) -> 
                                            if(Hashtbl.mem tbl name) then    
                                                Error("Re-definition error") 
                                            else (Hashtbl.add tbl name (params, block) ; Normal)   
                                        )     
        | Return(expr) -> Return(expr)
        | Continue -> Continue
        | Break -> Break
        | Printf (flt) -> print_float flt ; Normal
        | Print(str) -> Print(str)
        | _ -> Error ("No statement match")

and evalExpr (_e: expr) (_q:envQueue) (_f:functions): float  = 
    match _e with
    | Num(flt) -> flt
    | Var(str) -> varEval str _q
    | Op1(str, expr) -> (
                        match expr with 
                        | Var(valStr) -> (evalOp1 str (evalExpr expr _q _f) valStr _q )
                        | _ -> Float.infinity
    )
    | Op2(str, expr1, expr2) -> evalOp2 str (evalExpr expr1 _q _f) (evalExpr expr2 _q _f)
    | Fct(str, lst) -> 
        match _f with
        Functions(tbl) -> 
            match (Hashtbl.find tbl str) with
                (params, block) -> 
                    if(evalParams lst params) then
                        let rec fillParams (e: expr list) (s: string list) (oldque:envQueue) (que:envQueue) : envQueue = 
                            if(List.length e == 0) then que
                            else begin 
                                    match (List.hd que) with
                                    Tbl(tbl) -> begin 
                                        Hashtbl.add tbl (List.hd s) (evalExpr (List.hd e) oldque _f) ;
                                        fillParams (List.tl e) (List.tl s) oldque que 
                                    end
                                end               
                        in 
                            let newq = (Tbl(Hashtbl.create 100) :: _q) in 
                                                    match (evalCode block (fillParams lst params _q newq) _f) with
                                                    | Normal -> 0.0
                                                    | Return(expr) -> (evalExpr expr newq _f)           
                                                    | _ -> Float.infinity
                                                
                                            else 0.0
                                            (*throw error here for no function found*)

and runCode (code: block): unit =
    (*Passed first scope and code block to be evaluated *)
    match (evalCode code ([Tbl (Hashtbl.create 100) ]) (Functions (Hashtbl.create 100))) with
     | Printf(flt) -> print_float flt ; print_newline ()
     | (Error(str) | Print(str)) -> print_string str ; print_newline ()
     | _ -> ()
;;


let p1: block = [
Assign("v", Num(2.0)) ;
Expr( Op2 ( "+",  Var("v"), Num(4.0)) )
]

let%expect_test "p1" =
    runCode p1; 
    [%expect {| 
                6.   
                |}] 

    

let w: block = [While( Op2("<=", Var("x"), Num(4.0)), 
                        [ Expr(Op1("++", Var("x"))) ; 
                            If( Op2 ("==", Var("x"), Num(2.0) ) , [ Break ], [] ) ]  
                )]

let%expect_test "w" =
    runCode w; 
    [%expect {| 
                1. 
                2.  
                |}] 


let ret: block = [ 
    FctDef("add", ["x"; "y"], [ Return( Op2("+", Var("x"), Var("y")) ) ] ) ;
    Expr( Fct("add", [ Num(3.0) ; Num(4.0) ]) ) 
    ] 
    
let%expect_test "ret" =
runCode ret; 
[%expect {| 
            7.   
            |}]     


let ret2: block = [ 
FctDef("add", ["x"; "y"], 
    [ 
    Expr(Var("x")) ; 
    If(Op2 ("==", Var("x"), Num(4.0) ), 
        [ Return( Op2("+", Var("x"), Var("y")) ) ] ,
        [Return( Fct("add", [Op2("+", Var("x"),Num(1.0)) ; Var("y") ] ) )]
    )
    ]) ;
Expr( Fct("add", [ Num(3.0) ; Num(4.0) ]) ) ;
Expr(Var("x"))  
]


let%expect_test "ret2" =
runCode ret2; 
[%expect {| 
            3.
            4.
            8.
            0.   
            |}]   
            
(** THIS TEST WAS PROVIDED BY ANOTHER STUDENT 
chose to keep it as it demonstrates a lot of functionality **)
let fizzbuzz: block = 
[
    For(
        Assign("i", Num(1.0)),
        Op2("<=", Var("i"), Num(100.0)),
        Expr(Op1("++", Var("i"))),
        [
            Assign("w",Num(0.0));
            If(
                Op2("==",Op2("%",Var("i"), Num(3.0)),Num(0.0)),
                [Print("Fizz");
                    Assign("w",Num(1.0))],
                []
            );
            If(
                Op2("==",Op2("%",Var("i"), Num(5.0)),Num(0.0)),
                [Print("Buzz");
                    Assign("w",Num(1.0))],
                []
            );
            If(
                Op2("==",Var("w"), Num(0.0)),
                [Expr(Var("i"))],
                []
            );
            If(
                Op2("==",Var("w"), Num(1.0)),
                [Print("\n")],
                []
            );
        ]
    )
]
   
                
let%expect_test "fizzbuzz" =
    runCode fizzbuzz ; 
    [%expect {| 
        1.
        2.
        Fizz
        4.
        Buzz
        Fizz
        7.
        8.
        Fizz
        Buzz
        11.
        Fizz
        13.
        14.
        FizzBuzz
        16.
        17.
        Fizz
        19.
        Buzz
        Fizz
        22.
        23.
        Fizz
        Buzz
        26.
        Fizz
        28.
        29.
        FizzBuzz
        31.
        32.
        Fizz
        34.
        Buzz
        Fizz
        37.
        38.
        Fizz
        Buzz
        41.
        Fizz
        43.
        44.
        FizzBuzz
        46.
        47.
        Fizz
        49.
        Buzz
        Fizz
        52.
        53.
        Fizz
        Buzz
        56.
        Fizz
        58.
        59.
        FizzBuzz
        61.
        62.
        Fizz
        64.
        Buzz
        Fizz
        67.
        68.
        Fizz
        Buzz
        71.
        Fizz
        73.
        74.
        FizzBuzz
        76.
        77.
        Fizz
        79.
        Buzz
        Fizz
        82.
        83.
        Fizz
        Buzz
        86.
        Fizz
        88.
        89.
        FizzBuzz
        91.
        92.
        Fizz
        94.
        Buzz
        Fizz
        97.
        98.
        Fizz
        Buzz
    |}]

(* Fixed Fib functions to work appropriately *)
let fib: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<=", Var("x"), Num(1.0)),
                [Return(Var("x"))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(2.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]

let%expect_test "fib" =
    runCode fib ; 
    [%expect {| 
        2. 
        5.      
    |}]




let hi: block = [FctDef("hi", ["x"], [Expr(Op2("+", Var("x"), Num(1.0)))]);
        Expr(Fct("hi",[Num(2.0)])) ]

let%expect_test "hi" =
runCode hi; 
[%expect {| 
            3.
            0.   
            |}]          



let frloop: block = [ For( Assign("i", Num(0.0)) , Op2("<",Var("i"), Num(4.0)), Expr(Op1("++", Var("i"))), [Expr(Op1("++", Var("x")))])] 

let%expect_test "frloop" =
runCode frloop; 
[%expect {| 
            1.
            2.
            3.
            4.   
            |}]


let pr: block = [Printf(3.0)]

let%expect_test "pr" =
runCode pr;
[%expect {| 3. |}]


let p6 : block = [
   Assign("v", Num(2.0));
   Expr(Op1("++", Var("v")));
]

let%expect_test "p6" =
   runCode p6;
   [%expect {|3.|}]

   (* define yeet() {
    for(i=0; i < 5; i++){
        z += 2
        if(z < 8){
            print yeet
            continue

            //doesnt happen
            print x
        }
        # when z is 8 -> 7,
        # when z is 9 -> 8
        z--
    }
}    *)

let cont: block = [
For( Assign("i", Num(0.0)) , Op2("<", Var("i"), Num(5.0)) , Expr(Op1("++", Var("i"))) , 
[
    Assign("z",  Op2("+", Var("z"), Num(2.0) ));
    If(Op2("<", Var("z"), Num(8.0)), [Print("yeet\n") ; Continue ; (*This doesnt get run -> *)Expr(Var("x"))], [Expr(Op1("--" , Var("z")))] )

]) ]

let%expect_test "cont" =
   runCode cont;
   [%expect {|
            yeet
            yeet
            yeet
            7.
            8.
   |}]


let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]

let%expect_test "p2" =
    runCode p2; 
    [%expect {| 362880. |}]   