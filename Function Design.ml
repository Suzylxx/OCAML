(* Some example programs used for tests. *)

let e0 = If (I 0, Primop (Plus, [I 1 ; I 2]), I 0)
let e1 = If (Primop (Equals, [I 1 ;I 2]), I 2, I 3)
let e2 = If (Primop (Equals, [I 1 ; I 2]), I 2, B true)
let e3 = Fn ("x", Int, Primop (Plus, [Var "x" ; I 1]))
let e4 = Rec ("f", Arrow (Int, Int),
              Fn ("x",Int, If (Primop (Equals, [Var "x" ; I 0]),
                             I 0,
                             Primop (Plus, [ Apply(Var "f", Primop (Minus, [Var "x"; I 1])) ; Var "x"]))))

let e5 = Let ([Val(e1, "x")], Primop (Plus, [Var "x" ; I 1]))
let e6 = Let ([Valtuple (Tuple [I 1; Primop (Plus, [I 1 ; I 2])], ["x" ; "y"])],
              Primop (Times, [Var "x" ; Var "y"]))
let e7 = Let ([Val(e1, "x") ; Val (Primop (Plus, [Var "x" ; I 1]), "y")],
              Primop (Times, [Var "x" ; Var "y"]))

let fst' = Fn ("t", Product [Int; Int], Let ([Valtuple (Var "t", ["x"; "_"])], Var "x"))
let snd' = Fn ("t", Product [Int; Int], Let ([Valtuple (Var "t", ["_"; "y"])], Var "y"))

let e10 = If (I 0, Primop (Plus, [I 1 ; I 2]), Var "x")
let e11 = If (B true, Primop (Plus, [I 1 ; I 2]), Var "x")

let e12 = Let (
  [
    Val (snd', "snd");
    Val (I 3, "x");
    Val (Primop (Times, [Var "x"; I 2]), "y");
    Val (Tuple [Var "x"; Var "y"], "t")
  ],
  Tuple [B true; Apply (Var "snd", Var "t")]
)

let fact_tr =
  Rec ("fact_tr", Arrow (Product [Int; Int], Int),
       Fn ("args", Product [Int; Int],
           Let ([Valtuple (Var "args", ["acc"; "n"])],
                If (Primop (Equals, [Var "n"; I 0]),
                    Var "acc",
                    Let ([Val (Primop (Times, [Var "acc"; Var "n"]), "acc'");
                          Val (Primop (Minus, [Var "n"; I 1]), "n'");
                          Val (Tuple [Var "acc'"; Var "n'"], "args'")],
                         Apply (Var "fact_tr", Var "args'")))
               )
          )
      )

let fact_tr' = Fn ("n", Int, Apply (fact_tr, (Tuple [I 1; Var "n"])))

let fact =
  Rec ("fact", Arrow (Int, Int),
       Fn ("n", Int,
           If (Primop (Equals, [Var "n"; I 0]),
               I 1,
               Primop (Times,
                       [Var "n";
                        Apply (Var "fact",
                               Primop (Minus, [Var "n"; I 1]))]))
          )
      )

let bool_not =
  Fn ("b", Bool, If (Var "b", B false, B true))

let bool_and =
  Fn ("args", Product [Bool; Bool],
      Let ([Valtuple (Var "args", ["b1"; "b2"])],
           If (Var "b1", Var "b2", B false))
     )

let bool_or =
  Fn ("args", Product [Bool; Bool],
      Let ([Valtuple (Var "args", ["b1"; "b2"])],
           If (Var "b1", B true, Var "b2"))
     )

let bool_xor =
  Fn ("args", Product [Bool; Bool],
      Apply (bool_and,
             Tuple [Apply (bool_or, Var "args");
                    Apply (bool_not,
                           (Apply (bool_and, Var "args")))])
     )

let bool_eq =
  Fn ("args", Product [Bool; Bool],
      Apply (bool_not,
             (Apply (bool_xor, Var "args")))
     )

let leq =
  Fn ("n", Int,
      Fn ("m", Int,
          Apply
            (bool_or,
             Tuple [Primop (Equals, [Var "n"; Var "m"]);
                    Primop (LessThan, [Var "n"; Var "m"])])
         )
     )

let div = (* compute n / m as quotient and remainder by repeated subtraction *)
  Rec ("div", Arrow (Int, Arrow (Int, Product [Int; Int])),
       Fn ("n", Int,
           Fn ("m", Int,
               Let ([Val (Primop (Minus, [Var "n"; Var "m"]), "d")],
                    If (Primop (LessThan, [Var "n"; Var "m"]),
                        (* if the numerator is greater than denominator,
                           we can't subtract anymore, so the quotient is
                           zero and the remainder is n
                        *)
                        Tuple [I 0; Var "n"],
                       (* otherwise, we make a recursive call on d and add
                          one to the quotient.
                        *)
                        Let ([Valtuple (Apply
                                          (Apply (Var "div", Var "d"),
                                           Var "m"),
                                        ["q"; "r"]);
                              Val (Primop (Plus, [Var "q"; I 1]), "q'")],
                             Tuple [Var "q'"; Var "r"])))
              )
          )
      )

let right =
  (* applies a function to the right component of an integer tuple. *)
  Fn ("f", Arrow (Int, Int),
      Fn ("p", Product [Int; Int],
          Let ([Valtuple (Var "p", ["x"; "y"])],
               Tuple [Var "x";
                      Apply (Var "f", Var "y")])
         )
     )

let dup =
  Fn ("x", Int, Tuple [Var "x"; Var "x"])

let tup_eq =
  Fn ("ps", Product [Product [Int; Int]; Product [Int; Int]],
      Let ([Valtuple (Var "ps", ["p"; "q"]);
            Valtuple (Var "p", ["x1"; "y1"]);
            Valtuple (Var "q", ["x2"; "y2"])],
           Apply (bool_and,
                  Tuple [Primop (Equals, [Var "x1"; Var "x2"]);
                         Primop (Equals, [Var "y1"; Var "y2"])]))
     )

(*------------------------------------------------------------------
 * Q: Unused Variables
 *-------------------------------------------------------------------*)

let rec unused_vars e = match e with
  | Var _ | I _ | B _ -> []
  | If (e, e1, e2) ->
      union (unused_vars e, union (unused_vars e1, unused_vars e2))
  | Primop (po, args) ->
      List.fold_right (fun e1 e2 -> union (unused_vars  e1, e2)) args []
  | Apply (e1, e2) -> union (unused_vars e1, unused_vars e2)
  | Fn (x, _, e) ->
      if member x (freeVariables e) then
        unused_vars e
      else
        union ([x], unused_vars e)
  | Rec (x, _, e) ->
      if member x (freeVariables e) then
        unused_vars e
      else
        union ([x], unused_vars e)

  | Tuple exps ->
      List.fold_right (fun e1 e2 -> union (unused_vars e1, e2)) exps []

  | Let ([], e) -> unused_vars e

  | Let (Val (e,x)::decs, e2) ->
      let tail = Let (decs, e2) in
      let unused = union (unused_vars e, unused_vars tail) in
      if member x (freeVariables tail) then
        unused
      else
        union ([x], unused)

  | Let (Valtuple (e,xl)::decs, e2) ->
      let tail = Let (decs, e2) in
      let unused = union (unused_vars e, unused_vars tail) in
      let fvs = freeVariables tail in
      union (List.filter (fun x -> not (member x fvs)) xl, unused)

let unused_vars_tests =
  let f name = Fn (name, Int, Tuple [ I 3; I 3 ]) in
  let id = Fn ("x", Int, Tuple [ Var "x"; Var "x" ]) in
  let tup = Tuple [ f "x"; id; f "y" ] in
  [ ( f "x", ["x"] )
  ; ( id, [] )
  ; ( tup, ["x"; "y"] )
  ; ( Let
        ( [ Val ( f "z", "h" ); Valtuple (tup, ["f"; "g"]) ]
        , Apply ((Var "f"), I 3)
        )
    , [ "h"; "z"; "x"; "y"; "g" ]
    )
  ; ( Let
        ( [ Val ( tup, "t" ) ]
        , Let
            ( [ Valtuple ( Var "t", ["f"; "g"] ) ]
            , Tuple
                [ Apply (Var "f", I 3)
                ; Apply (Var "g", I 4)
                ]
            )
        )
    , [ "x"; "y" ]
    )
  ]

(*------------------------------------------------------------------
 * Q: Substitution
 *-------------------------------------------------------------------*)

(* Substitution
   subst : (exp * name) -> exp -> exp

   subst (e',x) e = [e'/x]e

   subst replaces every occurrence of the variable x
   in the expression e with e'.
*)

let rec substArg s a = List.map (subst s) a

and subst ((e',x) as s) exp =  match exp with
  | Var y ->
     if x = y then e'
     else Var y
  | I n -> I n
  | B b -> B b
  | Primop(po, args) -> Primop(po, substArg s args)
  | If(e, e1, e2) ->
     If(subst s e, subst s e1, subst s e2)
  | Tuple es ->
     Tuple (List.map (subst s) es)
  | Let([], e2) -> Let([], subst s e2)
  | Let(dec1::decs, e2) ->
     let rest = Let(decs, e2) in
     (match dec1 with
      | Val(exp, name) ->
            let (name, rest) =
              if member name (freeVariables e') then
                rename (name, rest)
              else
                (name, rest)
            in
            let exp = subst s exp in
            (* if this val binds the var we're subbing out, then we
               stop the recursion
             *)
            if name = x then
              Let(Val(exp, name) :: decs, e2)
            else
              (match subst s rest with
               | Let(decs, e2) -> Let(Val(exp, name) :: decs, e2)
               | _ -> assert false)

      | Valtuple(exp, names) ->
         let (names', rest) = renameList names e' rest in
         let exp = subst s exp in
         (* if the variable we're subbing out is bound by this val-tuple,
            then we stop the recursion.
          *)
         if member x names then
           Let(Valtuple(exp, names) :: decs, e2)
         else
           (match subst s rest with
            | Let(decs, e2) -> Let(Valtuple(exp, names') :: decs, e2)
            | _ -> assert false))
  | Apply (e1, e2) -> Apply (subst s e1, subst s e2)
  | Fn (y, t, e) ->
     if y = x then
       Fn (y, t, e)
     else
       if member y (freeVariables e') then
         let (y, e1) = rename (y,e) in
         Fn (y, t, subst s e1)
       else
         Fn(y, t, subst s e)
  | Rec (y, t, e) ->
     if y = x then
       Rec (y, t, e)
     else
       if member y (freeVariables e') then
         let (y, e1) = rename (y,e) in
         Rec (y, t, subst s e1)
       else
         Rec (y, t, subst s e)


and substList l e = match l with
| [] -> e
| (x,e')::pairs ->
    subst (x,e') (substList pairs e)

and rename (x, e) =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and renameAll e = match e with
  | ([], e) -> ([], e)
  | (x::xs, e) ->
      let (x', e) = rename (x, e) in
      let (xs', e) = renameAll (xs, e) in
      (x' :: xs', e)

and renameList names e' exp =
  if List.exists (fun name -> member name (freeVariables e')) names then
    renameAll(names, exp)
  else
    (names, exp)

let subst_tests : ((exp * name) * exp, exp) tests =
  [
    (((Var "x", "y"), (* [x/y] *)
      (* let (x, q, z) = (3, y, true) in y *)
      Let ([Valtuple (Tuple [I 3; Var "y"; B true], ["x"; "q"; "z"])],
           Var "y")),

     (* expected: let (x1, q, z) = (3, x, true) in x *)
     Let ([Valtuple (Tuple [I 3; Var "x"; B true], ["x1"; "q"; "z"])],
          Var "x"))
    ;

    (((Var "x", "y"), (* [x/y] *)
      (* let x = y in x *)
      Let ([Val (Var "y", "x")], Var "x")),

     (* expected: let x = x in x *)
     Let ([Val (Var "x", "x")], Var "x"))
    ;

    (((Var "x", "y"), (* [x/y] *)
      (* let (x, y, z) = q in y *)
      Let ([Valtuple (Var "q", ["x"; "y"; "z"])], Var "y")),

     (* expected: let (x, y, z) = q in y *)
     Let ([Valtuple (Var "q", ["x"; "y"; "z"])], Var "y"))
    ;

    (((Let ([Val (Var "x", "y")], Var "y"), "z"), (* [(let y = x in y)/z] *)
      (* let y = z in foo *)
      Let ([Val (Var "z", "y")], Var "foo")),

     (* expected: let y = (let y = x in y) in foo *)
     Let ([Val (Let ([Val (Var "x", "y")],
                     Var "y"),
                "y")],
          Var "foo"))
    ;

    (((Var "x", "z"), (* [x/z] *)
      (* let val x = y
             val z = (x, z)
         in (x, z)
      *)
      Let ([Val (Var "y", "x");
            Val (Tuple [Var "x"; Var "z"], "z")],
           Tuple [Var "x"; Var "z"])),

     (*  expected:
         let val x1 = y
             val z = (x1, x)
         in (x1, z)
     *)
     Let ([Val (Var "y", "x1");
           Val (Tuple [Var "x1"; Var "x"], "z")],
          Tuple [Var "x1"; Var "z"]))
    ;

    (((Tuple [Var "x"; Var "y"], "x"), (* [(x, y)/x] *)
      (* let val y = x
             val (y, x) = x
         in (x, y)
      *)
      Let ([Val (Var "x", "y");
            Valtuple (Var "x", ["y"; "x"])],
           Tuple [Var "x"; Var "y"])),

     (* expected:
        let val y1 = (x, y)
            val (y, x) = (x, y)
        in (x, y)
     *)
     Let
       ([Val (Tuple [Var "x"; Var "y"], "y1");
         Valtuple (Tuple [Var "x"; Var "y"], ["y"; "x"])],
        Tuple [Var "x"; Var "y"]))
    ;

    (((Var "y", "x"), (* [y/x] *)
      (* let val z = x
             val y = x
             val (y, x) = x
         in (x, y, z)
      *)
      Let ([Val (Var "x", "z");
            Val (Var "x", "y");
            Valtuple (Var "x", ["y"; "x"])],
           Tuple [Var "x"; Var "y"; Var "z"])),

     (* expected:
        let val z = y
            val y1 = y
            val (y2, x) = x
        in (x, y2, z)
     *)
     Let ([Val (Var "y", "z");
           Val (Var "y", "y1");
           Valtuple (Var "y", ["y2"; "x"])],
          Tuple [Var "x"; Var "y2"; Var "z"]))
    ;

    (((Tuple [Var "y"; Var "z"], "x"), (* [(y, z)/x] *)
      (* (let y = 3 in x, let z = 3 in x) *)
      Tuple [Let ([Val (I 3, "y")], Var "x");
             Let ([Val (I 3, "z")], Var "x")]),

     (* expected:
        (let y1 = 3 in (y, z), let z1 = 3 in (y, z))
     *)
     Tuple
       [Let ([Val (I 3, "y1")], Tuple [Var "y"; Var "z"]);
        Let ([Val (I 3, "z1")], Tuple [Var "y"; Var "z"])])
    ;

    (((Var "x", "y"), (* [x/y] *)
      (* let val x = (let val y = 3 in y + y)
             val y = x + x
         in y
      *)
      Let ([Val (Let ([Val (I 3, "y")],
                      Primop (Plus, [Var "y"; Var "y"])),
                 "x");
            Val (Primop (Plus, [Var "x"; Var "x"]),
                 "y")],
           Var "y")),

     (* expected:
        let val x = (let val y = 3 in y + y)
            val y = x + x
        in y
     *)
     Let
       ([Val (Let ([Val (I 3, "y")],
                   Primop (Plus, [Var "y"; Var "y"])),
              "x");
         Val (Primop (Plus, [Var "x"; Var "x"]),
              "y")],
        Var "y"))
    ;

    (((Var "x", "y"), (* [x/y] *)
      (* let val (x1, x) = y in y + x1 *)
      Let ([Valtuple (Var "y", ["x1"; "x"])],
           Tuple [Primop (Plus, [Var "y"; Var "x1"])])),

     (* expected:
        let val (x1, x) = x in x + x1
     *)
     Let
       ([Valtuple (Var "x", ["x1"; "x'"])],
        Tuple [Primop (Plus, [Var "x"; Var "x1"])]))
    ;
  ]

(*------------------------------------------------------------------
 * Q: Evaluation
 *-------------------------------------------------------------------*)

let rec evalList (exps : exp list) =
  List.map eval exps

and evalValtuple (e1, xs, decs, e2) =
  match eval e1 with
  | Tuple es ->
    if List.length es = List.length xs then
      eval (substList (List.combine es xs) (Let(decs, e2)))
    else
      raise (Stuck "Tuple binding failure (length mismatch)")

  | _ -> raise (Stuck "Tuple binding failure")

(* eval : exp -> exp *)
and eval exp = match exp with
  (* Values evaluate to themselves... *)
  | I _ -> exp
  | B _ -> exp

  | Var x -> raise (Stuck ("Free variable (" ^ x ^ ") during evaluation"))

  (* primitive operations +, -, *, <, = *)
  | Primop(po, args) ->
      let argvalues = evalList args in
      (match evalOp(po, argvalues) with
      | None -> raise (Stuck "Bad arguments to primitive operation")
      | Some v -> v)

  | Tuple es -> Tuple (evalList es)

  | Let([], e2) -> eval e2
  | Let(dec1::decs, e2) ->
      (match dec1 with
      | Val(e1, x) ->
         let v1 = eval e1 in
         eval (subst (v1, x) (Let(decs, e2)))
      | Valtuple(e1, xs) -> evalValtuple (e1, xs, decs, e2))

  | If(e, e1, e2) ->
       (match eval e with
       | B true -> eval e1
       | B false -> eval e2
       | _ -> raise (Stuck "Left term of application is not an Fn"))

  | Fn _ -> exp
  | Rec (f, _, e) -> eval (subst (exp, f) e)
  | Apply (e1, e2) ->
       (match eval e1 with
       | Fn(x,_,e) -> eval (subst (e2,x) e)
       | _ -> raise (Stuck "Left term of application is not an Fn"))

let eval_tests : (exp, exp) tests =
  [
    (Apply (fact_tr', I 3), I 6);
    (Apply (fact_tr', I 5), I 120);
    (Apply (fact_tr', I 8), I 40320);
    (Apply (bool_or, Tuple [B true; B false]), B true);
    (Apply (bool_and, Tuple [B true; B false]), B false);
    (Apply (bool_xor, Tuple [B true; B false]), B true);
    (Apply (Apply (div, I 16), I 5), Tuple [I 3; I 1]);
    (Apply (Apply (div, I 2), I 5), Tuple [I 0; I 2]);
    (Apply (Apply (div, I 10), I 2), Tuple [I 5; I 0]);
    (Apply (Apply (leq, I 3), I 5), B true);
    (Apply (Apply (leq, I 3), I 3), B true);
    (Apply (Apply (leq, I 3), I 1), B false);
    (Apply (tup_eq, Tuple [Apply (Apply (right, fact), Apply (dup, I 3));
                           Apply (Apply (right, fact_tr'), Apply (dup, I 3))]),
     B true)
    ;
    (Apply (tup_eq, Tuple [Apply (Apply (right, fact), Apply (dup, I 5));
                           Apply (Apply (right, fact_tr'), Apply (dup, I 5))]),
     B true)
    ;
    (Apply (tup_eq, Tuple [Apply (Apply (right, fact), Apply (dup, I 8));
                           Apply (Apply (right, fact_tr'), Apply (dup, I 8))]),
     B true)
  ]

(*------------------------------------------------------------------
 * Type Inference
 *-------------------------------------------------------------------*)

let rec infer ctx e = match e with
  | I _ -> Int
  | B _ -> Bool
  | Primop (po, exps) ->
     let (domain, range) =  primopType po in
     let rec check exps ts = match exps , ts with
       | [] , [] -> range
       | e::es , t::ts ->
          let t' = infer ctx e in
          if t' = t then check es ts
          else type_mismatch t t'
     in
       check exps domain

  | Tuple es -> Product (List.map (infer ctx) es)

  | Apply (e1, e2) ->
    (match infer ctx e1 with
     | Arrow(t2, t) ->
        let t2' = infer ctx e2 in
        if t2 = t2' then t
        else type_mismatch t2 t2'
    | t' -> raise (TypeError ("Application expected arrow type" ^ "\n Found type " ^ string_of_tp t')))

  | Var x -> (try lookup x ctx with NotFound -> raise (TypeError ("Found free variable")))

  | Rec (f, t, e) ->
     let ctx' = extend ctx (f, t) in
     let t'   = infer ctx' e in
     if t' = t then t
     else type_mismatch t t'

  | Fn (x,t,e) -> Arrow(t, infer (extend ctx (x,t)) e)
  | Let ([], e) -> infer ctx e
  | Let (dec::decs, e) ->
     let ctx' = infer_dec ctx dec  in
     infer ctx' (Let(decs, e))
  | If (e, e1, e2) ->
     (match infer ctx e with
      | Bool -> let t1 = infer ctx e1 in
                let t2 = infer ctx e2 in
                if t1 = t2 then t1
                else type_mismatch t1 t2
      | t -> type_mismatch Bool  t)

and infer_dec ctx dec = match dec with
  | Val (e, x) -> extend ctx (x, infer ctx e)
  | Valtuple (e, nl) ->
     (match infer ctx e with
     | Product tl ->
        if List.length tl = List.length nl then
          List.fold_left2 (fun ctx n t -> (n,t)::ctx) ctx nl tl
        else
          raise (TypeError ("Expected type " ^ string_of_tp (Product tl)))
     | t' -> raise (TypeError ("Expected product type" ^ "\n Found type " ^ string_of_tp t'))
     )

let infer_tests =
  [ ( ([], Tuple [ e1; e1 ]),
      Product [ Int; Int ]
    )
  ; ( ([], tup_eq),
      Arrow (Product [Product [Int; Int]; Product [Int; Int]],
             Bool)
    )
  ; ( ([], dup),
      Arrow (Int, Product [Int; Int]))
  ; ( ([], fact_tr),
      Arrow (Product [Int; Int], Int)
    )
  ; ( ([], fact),
      Arrow (Int, Int)
    )
  ; ( ([], Tuple [ bool_not; bool_not ]),
      Product [Arrow (Bool, Bool); Arrow (Bool, Bool)]
    )
  ; ( ([], bool_and),
      Arrow (Product [Bool; Bool], Bool)
    )
  ; ( ([], bool_or),
      Arrow (Product [Bool; Bool], Bool)
    )
  ; ( ([], bool_xor),
      Arrow (Product [Bool; Bool], Bool)
    )
  ; ( ([], bool_eq),
      Arrow (Product [Bool; Bool], Bool)
    )
  ; ( ([], leq),
      Arrow (Int, Arrow (Int, Bool))
    )
  ; ( ([], div),
      Arrow (Int, Arrow (Int, Product [Int; Int]))
    )
  ; ( ([], right),
      Arrow (Arrow (Int, Int),
             Arrow (Product [Int; Int], Product [Int; Int]))
    )
  ; ( ([], fst'),
      Arrow (Product [Int; Int], Int)
    )
  ; ( ([], snd'),
      Arrow (Product [Int; Int], Int)
    )
  ]
