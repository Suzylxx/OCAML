(* -------------------------------------------------------------*)
(* QUESTION 1 : String manipulation  [20 points]                *)
(* -------------------------------------------------------------*)

(* string_explode : string -> char list *)
let string_explode s = tabulate (fun x -> String.get s x) (String.length s)

(* string_implode : char list -> string *)
let string_implode l = List.fold_right (fun l acc -> Char.escaped l ^ acc) l ""

(* -------------------------------------------------------------*)
(* QUESTION 2 : Insert a string into a dictionary  [20 points]  *)
(* -------------------------------------------------------------*)

(* Insert a word into a dictionary. Duplicate inserts are allowed *)

let  insert s t =
  (* ins: char list * char trie list -> char trie list *)
  let rec ins l t = match l, t with
    | [], tr -> [Empty] @ tr 
    | l, [] -> (unroll l) 
    | l, [Empty] -> (unroll l) @ [Empty]
    | l, (Empty::tr) -> (Empty::(ins l tr))
    | a::a', Node(tr,tr')::tr'' -> 
        if a = tr then Node(tr, ins a' tr') :: tr''
        else Node(tr, tr') :: (ins l tr'')
  in
  ins (string_explode s) t

(* -------------------------------------------------------------*)
(* QUESTION 3 : Look up a string in a dictionary   [20 points]  *)
(* -------------------------------------------------------------*)

(* Look up a word in a dictionary *)

let lookup s t =
  (* lkp : char list -> char trie list -> bool *)
  let rec lkp l t =match l, t with 
    | [], [] -> false
    | [], [Empty] -> true 
    | [], tr -> contains_empty tr
    | l, [] -> false 
    | l, [Empty] -> false
    | l, (Empty::tr) -> lkp l tr
    | a::a', Node(tr,tr')::tr'' ->
        if a = tr then lkp a' tr'
        else lkp l tr''
  in
  lkp (string_explode s) t

(* -------------------------------------------------------------*)
(* QUESTION 4 : Find all strings in a dictionary   [OPTIONAL]   *)
(* -------------------------------------------------------------*)

(* Find all strings which share the prefix p *)

    (*let find_all prefix t =
  (* find_all' : char list -> char trie list -> char list list *)
       let rec find_all' l t =
         raise NotImplemented
       in
       raise NotImplemented
*)
(* -------------------------------------------------------------*)
(* QUESTION 5 :  Logic functions   [OPTIONAL]                   *)
(* -------------------------------------------------------------*)

(* eval: labeled_pred -> labeled_pred -> int -> int -> bool *)
    (*let eval (_, (p : int -> bool)) (_, (q : int -> bool)) (n : int) =
       raise NotImplemented*)
