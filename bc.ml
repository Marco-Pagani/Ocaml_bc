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

let rec addParams (q:env) params args : env =
  match params with
  | hdp::restp -> (
      match args with
      |hda::resta -> (
          Hashtbl.add_exn q ~key:hdp ~data:hda;
          addParams q restp resta
        )
      |[]-> q
    )
  | [] -> q



let rec varEval (_v: string) (_q:envQueue): float  =
  match _q with
  | hd::rest -> (
      match Hashtbl.find hd _v with
      | Some n -> n
      | None -> (varEval _v (rest) )
    )
  |[] -> 0.0


let rec evalCode (_code: block) (_q:envQueue)  = (*:  (envQueue, exitType)*)
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
      | "==" -> if (i == j) then 1.0 else 0.0

      | _ -> 0.0
    )
  | Fct(name, params) -> evalFunc name params _q

and evalStatement (s: statement) (q:envQueue)  = (*:  (envQueue, exitType)*)
  match s with 
  | Assign(_v, _e) -> (
      match q with
      | hd::rest ->(
          Hashtbl.remove hd _v;
          (*Hashtbl.add_exn hd ~Key:_v  ~Data:(evalExpr _e q) ;*)
          Hashtbl.add_exn hd _v  (evalExpr _e q) ;
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
    let cond = evalExpr e q in
    let (new_q, ret) = 
      if(cond>0.0) then
        evalCode codeT q 
      else
        evalCode codeF q
    ; in 
    (new_q, ret)
  | While(cond, body) -> evalWhile cond body q
  | For(init, cond, inc, body) -> evalFor init cond inc body q
  | FctDef(name, params, body) -> 
    Hashtbl.add_exn functionList name body;
    Hashtbl.add_exn paramList name params;
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
  let q1, ret1 = evalStatement _init _q in
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

and getArgPair p a q = 
  match p with
  | hdp::rest ->(
      match a with
      |hda::rest -> (
          let argval = evalExpr hda q in
          (hdp, argval)
        )
      |_ -> ("F", 0.0)
    )
  |_ -> ("F", 0.0)

and evalFunc (name: string) (args: expr list) (q: envQueue): float = 

  let (code: block) = Hashtbl.find_exn functionList name in
  let (params: string list) = Hashtbl.find_exn paramList name in
  let q1 = Hashtbl.create(module String) in
  let p, a = getArgPair params args q in (
    Hashtbl.add_exn q1 p a;
  (*
  let argVals = List.map args evalExpr q in
  q1 = addParams q1 params argVals
  *)
    let new_q = [q1]@q in
    let q2, ret = evalCode code new_q in
    match ret with
    | FReturn(x) -> x
    | Normal() -> 0.0
    | _-> 0.0
  )


(* let rec addParams (par: string list) (args: expr list) (q: envQueue) : envQueue = *)


let run (_code: block): unit = 
  let scope = (Hashtbl.create(module String) :: []) in
  let q, return = evalCode _code scope in (
    match q with
    |hd::[] -> ()
    |_ -> ()
  )
  (*
  let q, return = evalCode _code scope in (
    match q with
    |hd::[] -> ()
    |_ -> Error.of_string "something went wrong with scopes"
            match return with
            | Normal() -> ()
            | _ -> Error.of_string "something went wrong with returns"
  )
*)


(* ========== Tests ========== *)

let%expect_test "evalNum" = 
  evalExpr (Num 10.0) [] |>
  printf "%F";
  [%expect {| 10. |}]
(* 
    v = 10; 
    v // display v
 *)
let p1: block = [
  Assign("v", Num(1.0));
  Expr(Var("v")) 
]

let%expect_test "p1" =
  run p1; 
  [%expect {| 1. |}]



let p12: block = [
  If (Op2(">", Num(1.0), Num(2.0)),
      [Expr(Num(69.0))],
      [Expr(Num(420.0))];
     )
]

let%expect_test "p12" =
  run p12; 
  [%expect {| 420. |}]
   (*
    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v *)

let p2: block = [
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
(*
   let%expect_test "p2" =
   run p2 ; 
   [%expect {| 3628800. |}] 

*)
let p22: block = [
  Assign("v", Num(1.0));
  If(
    Op2("<", Var("v"), Num(10.0)), 
    [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
    [
      Assign("i", Num(2.0));
      While(
        Op2("<", Var("i"), Num(10.0)),
        [
          Assign("v", Op2("*", Var("v"), Var("i")));
          Assign("i", Op2("+", Var("i"), Num(1.0)));
          BreakL()
        ]
      )]
  );
  Expr(Var("v"))
]

let%expect_test "p22" =
  run p22 ; 
  [%expect {| 3628800. |}]  

(*  Fibbonaci sequence
    define f(x) {
     if (x<1.0) then
         return (1.0)
     else
         return (f(x-1)+f(x-2))
    }

    f(3)
    f(5)
*)
let p3: block = 
  [
    FctDef("f", ["x"], [
        If(
          Op2("<", Var("x"), Num(1.1)),
          [Return(Num(1.0))],
          [Return(Op2("+",
                      Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                      Fct("f", [Op2("-", Var("x"), Num(2.0))])
                     ))])
      ]);
    Expr(Fct("f", [Num(2.0)]));
    Expr(Fct("f", [Num(4.0)]));
  ]

let%expect_test "p3" =
  run p3; 
  [%expect {| 
        2. 
        5.      
    |}]


