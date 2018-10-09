exception NotImplemented

(* -------------------------------------------------------------*)
(* QUESTION:  Tries                                             *)
(* -------------------------------------------------------------*)

(* A trie is an n-ary tree *)

type 'a trie = Node of 'a * ('a trie) list | Empty

(* -------------------------------------------------------------*)
(* Example trie list                                            *)
(* -------------------------------------------------------------*)

let t =
 [Node
     ('b',
      [Node ('r' , [Node ('e' , [Node ('e' , [Empty])])]) ;
       Node ('e' , [Empty ;
		    Node ('e' , [Empty ; Node ('f', [Empty; Node ('y', [Empty])]) ;
				 Node ('r',[Empty])]) ;
		    Node ('a' , [Node ('r', [Empty; Node ('d' , [Empty])])])])])]

(* -------------------------------------------------------------*)
(* Implementation of tabulate                                   *)
(* tabulate f n returns [f 0; f 1; ...; f (n - 1)]              *)
(* -------------------------------------------------------------*)

let rec tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n - 1) ((f n) :: acc)
  in
  tab (n - 1) []

(* -------------------------------------------------------------*)
(* TRIE HELPER FUNCTIONS                                        *)
(* -------------------------------------------------------------*)

(* Creates a tree containing only l *)
(* unroll : 'a list -> 'a trie list *)
let rec unroll l = match l with
  | []     -> [Empty]
  | x :: t -> [Node(x, unroll t)]

(* Returns true if l contains an Empty node, false otherwise *)
(* contains_empty : 'a trie list -> bool *)
let rec contains_empty l = match l with
  | x :: xs -> x = Empty || contains_empty xs
  | []      -> false

(* Examples of functions that could be used for q5 predicates *)
type labeled_pred = string * (int -> bool)

let is_even : labeled_pred = ("Is even", fun n -> (n mod 2) = 0)
let mult_3 : labeled_pred = ("Is a multiple of 3", fun n -> (n mod 3) = 0)
let is_odd : labeled_pred = ("Is odd", fun n -> (n mod 2) <> 0)
let more_than_4 : labeled_pred = ("Is larger than 4", fun n -> n > 4)
let is_pow_of_2 : labeled_pred =
  let rec f = function
  | 0 -> false
  | 1 -> true
  | n -> if n mod 2 <> 0 then false else f (n / 2)
  in
  ("Is a power of 2", f)
