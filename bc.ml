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
  | Expr of expr
  | If of expr*statement list * statement list
  | While of expr*statement list
  | For of statement*expr*statement*statement list
  | FctDef of string * string list * statement list 

type block = statement list 

type env = N of (string, float) Hashtbl.t

type envQueue = S of env list

let functionList = Hashtbl.create (module String)

type exitType =
  | Normal of unit
  | FReturn of expr
  | Break of string
  | Continue of string

let rec varEval (_v: string) (_q:envQueue): float  =
  match _q with
  | hd::rest -> (
      match hd with
      |N(htbl) ->   ( 
        try (Hashtbl.find_exn htbl _v)
        with Not_found -> (varEval _v rest )
        )
    )
  | [] -> 0.0


let rec evalCode (_code: block) (_q:envQueue): (envQueue, exitType) = 
  match _code with
  | hd::rest -> (
    let new_q, ret = evalStatement hd _q in
      match ret with
      | Normal() ->
      | FReturn(i) -> 
      | Break(s) ->
      | Continue(s) -> 

    

    evalCode rest _q
    )
  | _ -> (_q, Normal())

and evalExpr (_e: expr) (_q:envQueue): float  = 
  match _e with
  | Num(i) -> i
  | Var(x) -> varEval x _q
  | Op1(op, n) ->(
    let i = evalExpr n _q in
    match op with
    | "++" -> i + 1
    | "--" -> i - 1
    | "-"  -> -i
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
    | _ -> 0.0
  )
  | Fct(name, params) -> evalFunc name params _q
  | _ -> 0.0 

and evalStatement (s: statement) (q:envQueue): (envQueue, exitType) =
  match s with 
  | Assign(_v, _e) -> (
    match q with
    | hd::rest ->
        Hashtbl.add hd ~key:_v ~data: evalExpr _e q; (q, Normal())
  )
  | Expr(e) -> let res = evalExpr e q in print_float res; (q, Normal())
  | Return(e) -> let res = evalExpr e q in (q, FReturn(res))
  | If(e, codeT, codeF) -> 
    let cond = evalExpr e q in
    if(cond>0.0) then
      evalCode codeT q 
    else
      evalCode codeF q
  ;q
  | While -> ()
  | For -> ()
  | FctDef -> ()
  | _ -> q (*ignore *)

and evalWhile (_cond: expr) (_body: statement list) (_q: envQueue) : (envQueue, exitType) =
  (_q, Normal())

and evalFor (_init: statement) (_cond: expr) (_inc: statement) (_body: statement list) (_q: envQueue) : (envQueue, exitType)=
  (_q, Normal())

and evalFunc (name: string) (params: expr list) (q: envQueue): float = 
  let (code: block) = Hashtbl.find_exn functionList name in
    evalCode code q

()

let run (_code: block): unit = 
  let scope = S(Hashtbl.create(module String) :: []) in
   let q, return = evalCode _code scope in (
    match q with
      |hd::[] -> ()
      |_ -> Error.of_string "something went wrong with scopes"
    match return with
      | Normal() -> ()
      | _ -> Error.of_string "something went wrong with returns"
   )



(* ========== Tests ========== *)

let%expect_test "evalNum" = 
  evalExpr (Num 10.0) |>
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
  evalCode p1 []; 
  [%expect {| 1. |}]

(*
    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)
let p2: block = [
  Assign("v", Num(1.0));
  If(
    Op2(">", Var("v"), Num(10.0)), 
    [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
    [For(
        Assign("i", Num(2.0)),
        Op2("<", Var("i"), Num(10.0)),
        Expr(Op1("++a", Var("i"))),
        [
          Assign("v", Op2("*", Var("v"), Var("i")))
        ]
      )]
  );
  Expr(Var("v"))
]

let%expect_test "p1" =
  evalCode p2 []; 
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
          Op2("<", Var("x"), Num(1.0)),
          [Return(Num(1.0))],
          [Return(Op2("+",
                      Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                      Fct("f", [Op2("-", Var("x"), Num(1.0))])
                     ))])
      ]);
    Expr(Fct("f", [Num(3.0)]));
    Expr(Fct("f", [Num(5.0)]));
  ]

let%expect_test "p3" =
  evalCode p3 []; 
  [%expect {| 
        2. 
        5.      
    |}]



