(***
======= BC INTERPRETER ========
Marco Pagani | Ximena Jaramillo
===============================
Thanks for helping us get this
     far Prof. Dobra <3
===============================
***)

open Core

type sExpr = 
  | Atom of string
  | List of sExpr list

type expr = 
  | Num of float
  | Var of string
  | Op1 of string*expr
  | Op2 of string*expr*expr
  | Fct of string * expr list

type statement = 
  | Assign of string*expr
  | Return of expr
  | BreakL of unit
  | ContinueL of unit
  | Expr of expr
  | If of expr*statement list * statement list
  | While of expr*statement list
  | For of statement*expr*statement*statement list
  | FctDef of string * string list * statement list 

type block = statement list

type env = (string, float) Hashtbl.t

type envQueue = env list

let functionList = Hashtbl.create (module String)
let paramList = Hashtbl.create (module String)

type exitType =
  | Normal of unit
  | FReturn of float
  | Break of unit
  | Continue of unit

let addParams (q:env) param arg  =
  Hashtbl.set q ~key:param ~data:arg

let rec varEval (_v: string) (_q:envQueue): float  =
  match _q with
  | hd::rest -> (
      match Hashtbl.find hd _v with
      | Some n -> (*printf "Found var %S: %F\n" _v n;*)n
      | None -> (varEval _v (rest) )
    )
  |[] -> 0.0


let rec evalCode (_code: block) (_q:envQueue)  =
  match _code with
  | hd::rest -> 
    let (new_q, ret) = evalStatement hd _q in (
      match ret with 
      | Normal() -> evalCode rest new_q
      | FReturn(i) -> (new_q, FReturn(i))
      | Break() -> (new_q, Break())
      | Continue() -> (new_q, Continue())
    )
  | [] -> (_q, Normal())

and evalExpr (_e: expr) (_q:envQueue): float  = 
  match _e with
  | Num(i) -> i
  | Var(x) -> varEval x _q
  | Op1(op, n) ->(
      let i = evalExpr n _q in
      match op with
      | "++" ->  i +. 1.0       
      | "--" -> i -. 1.0
      | "-"  -> 0.0 -. i
      | _ -> 0.0
    )
  | Op2(op, m, n) -> (
      let i = evalExpr m _q in
      let j = evalExpr n _q in
      match op with
      | "+" -> i +. j
      | "-" -> i -. j
      | "*" -> i *. j
      | "/" -> i /. j
      | "^" -> i ** j

      | "<" -> if (i < j) then 1.0 else 0.0
      | ">" -> if (i > j) then 1.0 else 0.0
      | "<=" -> if (i <= j) then 1.0 else 0.0
      | ">=" -> if (i >= j) then 1.0 else 0.0
      | "==" -> if (i == j) then 1.0 else 0.0

      | _ -> 0.0
    )
  | Fct(name, params) -> evalFunc name params _q

and evalStatement (s: statement) (q:envQueue)  = 
  match s with 
  | Assign(_v, _e) -> (
      match q with
      | hd::_ ->(
          Hashtbl.set hd ~key:_v  ~data:(evalExpr _e q) ;
          (*printf "Stored var %S: %F\n" _v (evalExpr _e q);*)
          (q, Normal())
        )
      | [] -> (q, Normal())
    )

  | Expr(e) -> 
    let res = evalExpr e q in 
    print_float res;
    print_newline(); 
    (q, Normal())
  | Return(e) -> let res = evalExpr e q in (q, FReturn(res))
  | If(e, codeT, codeF) -> 
    let cond = evalExpr e q in(
      if(cond>0.0) then(
        let (new_q, ret) = evalCode codeT q in
        match ret with
        | Normal() -> new_q, Normal()
        | FReturn(x) -> new_q, FReturn(x)
        | Break() -> new_q,Break()
        | Continue() -> new_q,Continue()
      )
      else(
        let (new_q, ret) = evalCode codeF q in
        match ret with
        | Normal() -> new_q, Normal()
        | FReturn(x) -> new_q, FReturn(x)
        | Break() -> new_q,Break()
        | Continue() -> new_q,Continue()
      )
    )
  | While(cond, body) -> evalWhile cond body q
  | For(init, cond, inc, body) -> evalFor init cond inc body q
  | FctDef(name, params, body) -> 
    Hashtbl.add_exn functionList ~key:name ~data:body;
    Hashtbl.add_exn paramList ~key:name ~data:params;
    (q, Normal())
  | BreakL() -> q,Break()
  | ContinueL() -> q,Continue()

and evalWhile (_cond: expr) (_body: block) (_q: envQueue)  =

  let condition = (evalExpr _cond _q) in
  if (condition>0.1) then (
    (*printf "condtion is %F, true" condition;*)
    let new_q, ret = evalCode _body _q in 
    match ret with
    |Normal() -> evalWhile _cond _body new_q 
    |FReturn(x) -> (new_q, FReturn(x))
    |Break() -> (new_q, Normal())
    |Continue() -> evalWhile _cond _body new_q
  )
  else
    (_q, Normal())

and evalFor (_init: statement) (_cond: expr) (_inc: statement) (_body: statement list) (_q: envQueue) =
  let q1, _ = evalStatement _init _q in
  let condition = (evalExpr _cond _q) in
  if (condition>0.1) then (
    let q2, ret2 = evalCode _body q1 in
    match ret2 with
    |Normal() -> evalFor _inc _cond _inc _body q2
    |FReturn(x) -> q2,FReturn(x)
    |Break() -> q2, Normal()
    |Continue() -> evalFor _inc _cond _inc _body q2
  )
  else
    (_q, Normal())


and evalFunc (name: string) (args: expr list) (q: envQueue): float = 

  let (code: block) = Hashtbl.find_exn functionList name in
  let (params: string list) = Hashtbl.find_exn paramList name in
  let q1 = Hashtbl.create(module String) in
  let argVals = List.map args ~f:(Fn.flip(evalExpr) q) in (
    let _ = List.iter2 ~f:(addParams q1) params argVals in
    let new_q = [q1]@q in
    let _, ret = evalCode code new_q in
    match ret with
    | FReturn(x) -> x
    | Normal() -> 0.0
    | _-> 0.0
  )


let run (_code: block): unit = 
  let scope = (Hashtbl.create(module String) :: []) in
  let q, return = evalCode _code scope in (
    (
      match q with 
      |_::[] -> ()
      |_ -> prerr_string "something went wrong with scopes"; ()
    );
    (
      match return with
      | Normal() -> ()
      | _ -> prerr_string "something went wrong with returns"; ()
    );
  )

(* ========== TESTS ========== *)

(* ========== Expressions ========== *)
let pExpr: block = [
  Expr(Num 10.0);
  Expr(Num 5.0) 
]
let%expect_test "pExpr" = 
  run pExpr;
  [%expect {| 
  10.
  5. |}]

(* ========== Variable Assignment ========== *)

let pVar: block = [
  Assign("v", Num(1.0));
  Expr(Var("v")) 
]

let%expect_test "pVar" =
  run pVar; 
  [%expect {| 1. |}]

(* ========== Single value operators ========== *)

let pOp1: block = [
  Assign("v", Num(1.0));
  Expr(Op1("++",Var("v")));
  Expr(Op1("--",Var("v")));
  Expr(Op1("-",Var("v"))) 
]

let%expect_test "pOp1" =
  run pOp1; 
  [%expect {| 
    2.
    0.
    -1.
   |}]

(* ========== Two value operators ========== *)

let pOp2: block = [
  Assign("v", Num(1.0));
  Expr(Op2("+",Var("v"), Num(2.0)));
  Expr(Op2("-",Var("v"), Var("v")));
  Expr(Op2("^",Var("v"), Num(2.0)));
]

let%expect_test "pOp2" =
  run pOp2; 
  [%expect {|
   3.
   0.
   1.  
   |}]

(* ========== Reassign vars ========== *)

let pAssign2: block = [
  Assign("v", Num(2.0));
  Expr(Var("v"));
  Expr(Op2("+", Var("v"), Num(1.0)));
  Assign("v", Op2("+", Var("v"), Num(1.0)));
  Expr(Var("v"))
]

let%expect_test "pAssign2" =
  run pAssign2; 
  [%expect {| 
  2.
  3.
  3. |}]

(* ========== If Statements ========== *)

let pIf: block = [
  If (Op2(">", Num(1.0), Num(2.0)),
      [Expr(Num(69.0))],
      [Expr(Num(420.0))];
     )
]

let%expect_test "pIf" =
  run pIf; 
  [%expect {| 420. |}]

(* ========== While loops ========== *)

let pWhile: block = [
  Assign("v", Num(1.0));
  If(
    Op2(">", Var("v"), Num(10.0)), 
    [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
    [
      Assign("i", Num(2.0));
      While(
        Op2("<", Var("i"), Num(10.0)),
        [
          Assign("v", Op2("*", Var("v"), Var("i")));
          Assign("i", Op2("+", Var("i"), Num(1.0)));
        ]
      )]
  );
  Expr(Var("v"))
]

let%expect_test "pWhile" =
  run pWhile ; 
  [%expect {| 362880. |}]  

(* ========== For loops ========== *)

let pFor: block = [
  Assign("v", Num(1.0));
  If(
    Op2(">", Var("v"), Num(10.0)), 
    [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
    [For(
        Assign("i", Num(2.0)),
        Op2("<", Var("i"), Num(10.0)),
        Assign("i", Op1("++", Var("i"))),
        [
          Assign("v", Op2("*", Var("v"), Var("i")))
        ]
      )]
  );
  Expr(Var("v"))
]

let%expect_test "pFor" =
  run pFor ; 
  [%expect {| 362880. |}] 

(* ========== Recursive functions ========== *)

let pRec: block = 
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
    Expr(Fct("f", [Num(0.0)]));
    Expr(Fct("f", [Num(1.0)]));
    Expr(Fct("f", [Num(2.0)]));
    Expr(Fct("f", [Num(3.0)]));
    Expr(Fct("f", [Num(4.0)]));
    Expr(Fct("f", [Num(5.0)]));
    Expr(Fct("f", [Num(6.0)]));
    Expr(Fct("f", [Num(7.0)]));
  ]

let%expect_test "pRec" =
  run pRec; 
  [%expect {| 
        0. 
        1.  
        1. 
        2. 
        3. 
        5. 
        8. 
        13.     
    |}]

(* ========== Multi var functions ========== *)

let pMultiVar: block =
  [
    FctDef("threeSum", ["x";"y";"z"], [
        Return((Op2("+",(Op2("+",Var("x"), Var("y"))), Var("z"))))
      ]);
    Expr(Fct("threeSum",[Num(1.0);Num(2.0);Num(3.0)]));
  ]

let%expect_test "pMultiVar" =
  run pMultiVar; 
  [%expect {| 6. |}]

