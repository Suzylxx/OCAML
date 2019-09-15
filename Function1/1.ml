(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 1 *)
(* Helper function: given two expressions, we add or multiply
 them     *)
(* ————————————————————–—————————————————————————————————————————————– *)
let add x y = let p = ref (get_value x + get_value y) in Plus (p, x, y)
let mult x y = let p = ref (get_value x * get_value y) in Times (p, x, y)


(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 2                                                        *)
(* compute_column m f = c

 Given a spreadsheet m and a function f, compute the i-th value in
 the result column c by using the i-th value from each column in m.

 Example:
 m = [ [a1 ; a2 ; a3 ; a4] ;
       [b1 ; b2 ; b3 ; b4] ;
       [c1 ; c2 ; c3 ; c4] ]

To compute the 2nd value in the new column, we call f with
[a2 ; b2 ; c2]

 Generic type of compute_column:
   'a list list -> ('a list -> 'b) -> 'b list

 If it helps, you can think of the specific case where we have a
 spreadsheet containing expressions, i.e.
 compute_column: exp list list -> (exp list -> exp) -> exp list

 Use List.map to your advantage!

 Carefully design the condition when you stop.
 *)
(* ————————————————————–—————————————————————————————————————————————– *)


let rec is_empty m = List.for_all (fun r -> r = [] ) m

let rec compute_column m p =
  if is_empty m then []
  else
    let r  = List.map (function x::xs -> x) m in
    let m' = List.map (function x::xs -> xs) m in
    snd p r :: compute_column m' p


(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 3 *)
(* Implement a function update which given an expression will re-
 compute the values stored at each node. This function will be used
 after we have updated a given number.

 update  : exp -> unit

 *)
(* ————————————————————–—————————————————————————————————————————————– *)
let update_cell (loc, v) = (loc := v)

let rec update e = match e with
  | Num r -> ()
  | Plus (r, e1, e2) ->
     (update e1 ; update e2;
let v = (get_value e1) + (get_value e2) in
if !r = v then () else r := v)
  | Times (r, e1, e2) ->
     (update e1 ; update e2;
let v = (get_value e1) * (get_value e2) in
if !r = v then () else (r := v))


(* Fully update a whole spreadsheet *)
let update_exp e = update e
let update_column c = List.fold_left (fun _ e -> update e) () c
let update_sheet m = List.fold_left  (fun _ c -> update_column c) () m

(* EXTRA FUN:
 Our implementation traverses the whole expression and even worse
 the whole spreadsheet, if one number cell is being updated.

 If you are looking for a nice programming problem, think
 about how to update only those values which are parent nodes to the
 Number being updated. You might need to choose a different
 representation for expressions.

 *)
(* ————————————————————–—————————————————————————————————————————————– *)
