(* Question 1: unify *)
let rec unify (t1 : tp) (t2 : tp) : unit =
  match t1, t2 with
  (* unifying identical concrete types does nothing *)
  | Int, Int
  | Bool, Bool -> ()
  (* For type constructors, recursively unify the parts *)
  | Arrow (t1, t1'), Arrow (t2, t2') ->
      (unify t1 t2; unify t1' t2')
  | Product tl1, Product tl2 ->
      if List.length tl1 = List.length tl2 then
        List.iter2 unify tl1 tl2
      else
        unif_error UnifProductMismatch
  | TVar a, _ -> unifyVar a t2
  | _, TVar b -> unifyVar b t1
  (* All other cases are mismatched types. *)
  | _, _ -> unif_error @@ UnifMismatch (t1, t2)

(* Unify a variable with a type *)
and unifyVar a t = match !a with
  | Some t1' -> unify t1' t
  | None ->
      match t with
      | TVar b -> (match !b with
          | None -> if a != b then a := Some (TVar b)
          | Some t2' -> unifyVar a t2')
      | _ ->
          if occurs a t then unif_error UnifOccursCheckFails
          else a := Some t

(*------------------------------------------------------------------
 * Type Inference
 *-------------------------------------------------------------------*)

(* Question 2: infer *)
let rec infer (ctx : context) (e : exp) : tp = match e with
  | I _ -> Int
  | B _ -> Bool
  | Primop (po, exps) ->
     let (domain, range) =  primopType po in
     let rec check exps ts = match exps , ts with
       | [] , [] -> range
       | e::es , t::ts ->
          let t' = infer ctx e in
          unify t' t;
          check es ts
       | _, _ -> type_error InvalidPrimop
     in
       check exps domain

  | Tuple es -> Product (List.map (infer ctx) es)

  | Apply (e1, e2) ->
     let t1 = infer ctx e1 in
     let t2 = infer ctx e2 in
     let t = TVar (ref None) in
     unify (Arrow(t2, t)) t1;
     t

  | Var x ->
     (match lookup x ctx with
      | Some r -> r
      | None -> type_error @@ FreeVariable x)

  | Rec (f, e) ->
     let t = TVar (ref None) in
     let ctx' = extend ctx (f, t) in
     let t' = infer ctx' e in
     unify t' t;
     t

  | Fn (x, e) ->
     let t = TVar (ref None) in
     Arrow(t, infer (extend ctx (x, t)) e)

  | Let ([], e) -> infer ctx e

  | Let (dec::decs, e) ->
     let ctx' = infer_dec ctx dec in
     infer ctx' (Let (decs, e))

  | If (e, e1, e2) ->
     let t = infer ctx e in
     unify t Bool;
     let t1 = infer ctx e1 in
     let t2 = infer ctx e2 in
     unify t1 t2;
     t1

and infer_dec ctx dec = match dec with
  | Val (e, x) -> extend ctx (x, infer ctx e)
  | Valtuple (e, nl) ->
     let l = List.length nl in
     let t = infer ctx e in
     let vars = tabulate (fun _ -> TVar (ref None)) l in
     let expected = Product vars in
     unify expected t;
     extend_list ctx (List.combine nl vars)
