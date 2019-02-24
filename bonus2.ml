(* Higher-Order Functions – Additional Practice *)

(* Question 1.1: 10 points *)
let rec compose l = match l with
  | [] -> fun x -> x
  | f::fs -> let c = compose fs  in
             fun x -> f (c x)

(* One line solution using List.fold_right *)
let rec compose' l =
  (* g is the accumulator of the fold, so we just compose f on the
   * left to build up the bigger composite function *)
  List.fold_right (fun f g x -> f (g x)) l (fun x -> x)

(* Why does compose' work?
   Theorem: compose' composes all functions in l from left to right.
   Proof by induction:
   Base case l = [].
   > fold_right (fun f g x -> f (g x)) [] (fun x -> x) = fun x -> x
   by definition.
   The composition of zero functions is the identity function
   `fun x -> x`, so this is good.
   (Just like how a sum of no numbers is 0.)

   Step case l = f :: fs.
   Recall that
   > fold_right op (x :: xs) e = op x (fold_right xs e),
   so
   > fold_right (fun f g x -> f (g x)) (f::fs) e
   > = (fun f g x -> f (g x)) f (fold_right fs e)
   > = fun x -> f (fold_right fs e x)
   (last step is by partially applying the tree-argument function
   `fun f g x -> ...` to two arguments `f` and `fold_right fs e`.)

   Next, by induction hypothesis, `fold_right fs e` computes the
   left-to-right composition of all functions in `fs`.
   Let's call this composite function `k`.
   Then we get
   > = fun x -> f (k x) --- (1)
   In general, the definition of composing f onto g is
   > f . g = fun x -> f (g x)
   (Where . (dot) represents the composition operator.)
   So continuing from (1) we get:
   > = f . k
   by definition of function composition.
   This is the left-to-right composition of all functions in f::fs,
   as required.
 *)
   

(* shortest solution *)
(*  Note that l must remain as parameter to compose, while x is optional : 
    otherwise, passing the first list to compose will fix the input
    type forever.
    This is a limitation of the OCaml type system called the "value restriction".
    https://stackoverflow.com/questions/22507448/the-value-restriction
 *)
let compose'' l (* x *) = 
  List.fold_right (@@) l (* x *)

(* Why does compose'' work?
   Proof by sloppy induction:
   Recall that fold_right op (f::fs) e = op f (fold_right fs e)
   So substituting @@ for op and using infix notation we get:
   op f (fold_right fs e) = f @@ fold_right fs e
   Recall that f @@ x = f x, so substituting we get:
   f @@ fold_right fs e = f (fold_right fs e)
   So this fold will generate precisely
   f1 (f2 (f3 ... (fk e)))
 *)

(* Q 1.2: 10 points *)
let rec replicate n x =
  if n <= 0 then
    []
  else
    x :: replicate (n - 1) x

let rec replicate' n =
  if n <= 0 then
    fun x -> []
  else
    let f = replicate' (n-1) in
    fun x -> x :: f x
         
(* There's an operational difference between replicate and replicate'.
   replicate *needs* x to be specified before it can compute anything,
   whereas replicate' just needs n in order to build up a function
   that replicates a still unknown x n times.
   The key is that all the braching structure and recursion is
   happening *outside* of the abstraction over x in replicate'.
 *)
                    
(* Q 1.3: 5 points

   Using compose implement the function repeat which takes a function f : 'a -> 'a and repeatedly applies f to an input x. The runction repeat should therefore return a function that still expects an input of type 'a and returns f(f....(x)).

  Hint: Use replicate to build a list [f ; f ; .... ; f] first.
 *)

let repeat n f = compose (replicate n f)

(* Question 2 - Unfolding *)

(* This is the function unfold, take some time to compare it it fold.
   If fold_left and fold_right take lists to generate a value, this
   function takes a value that will generate a list. The relation
   between folds and unfolds is the beginning of a wonderful tale in
   computer science. But let's not get ahead of ourselves.


   Unfold takes a function that from a seed value it generates a new
   element of the list, and a the seed for the next element, another
   function to stop the generation, and an initial seed.
*)

let evens max = unfold (fun b -> b, b + 2) (fun b -> max <= b) 0

(* Q2.2: Return the Fibonacci sequence up-to max *)
let fib max =
  unfold
    (fun (n1, n2) -> n1, (n2, n1+n2))
    (fun (x, _) -> x >= max)
    (1, 1)
  (* The idea is to make the seed hold the current number as well as
     the *next* number (rather than the previous).
   *)

(* Q2.3: Return the list of rows of the Pascal triangle that are shorter than max *)
let pascal max =
  unfold
    (fun r ->
      let next = List.map2 (+) ([0] @ r) (r @ [0]) in
      r, next)
    (fun l -> List.length l > max)
    [1]

(* Q2.4 *)               

(* Implement zip with a single call to unfold *)
let zip l1 l2 =
  unfold (fun (x::xs, y::ys) -> ((x,y), (xs, ys)))
         (fun (l1, l2) -> l1 = [] || l2 = []) (l1, l2)
