(* -------------------------------------------------------------*)
(* QUESTION 1 : Let's have cake!                                *)
(* -------------------------------------------------------------*)

(* allergy_free : ingredient list -> cupcake list -> cupcake list *)
let rec allergy_free allergens cupcakes = match allergens, cupcakes with
  |_, [] -> []
  |[], _ -> cupcakes 
  |i::i', [c] -> 
      allergy_free i' (List.filter (fun (Cupcake(_,_,_,ing)) -> 
          List.for_all (fun al -> al != i) ing) cupcakes)
  |i::i', c::c' ->
      allergy_free (i') (List.filter (fun (Cupcake(_,_,_,ing)) -> 
          List.for_all (fun al -> al != i) ing) cupcakes)
  |i::i', c::c' -> 
      List.filter (fun (Cupcake(_,_,_,ing)) -> 
          List.exists (fun al -> al = i) ing) cupcakes
  
(* -------------------------------------------------------------*)
(* QUESTION 2 : Generic Tree Traversals                         *)
(* -------------------------------------------------------------*)

      (*(map_tree : ('a -> 'b) -> 'a tree -> 'b tree *)
let rec map_tree f t = match t with
  | Empty -> Empty
  | Node(n, tr, tr') -> Node(f n, map_tree f tr, map_tree f tr')

(* delete_data : ('a * 'b) tree -> 'a tree *)
let delete_data t = 
  let del (x,y) = match (x,y) with
    | (_,_) -> x
  in
  map_tree del t
  
(* fold_tree : ('a * 'b ' * 'b -> 'b) -> 'b -> 'a tree -> 'b *)
let rec fold_tree f e t = match t with
  | Empty -> e
  | Node(n, tr, tr') -> f(n, fold_tree f e tr, fold_tree f e tr')
  
      (* size : 'a tree -> int *)
let size tr = 
  fold_tree (fun (n, tr, tr') -> 1 + tr + tr') 0 tr

    (*reflect : 'a tree -> 'a tree *)
let reflect tr =
  fold_tree (fun (n, tr, tr') -> Node(n, tr', tr)) (Empty) tr

(* inorder : 'a tree -> 'a list *)
let inorder tr =
  fold_tree (fun (n, tr, tr') -> tr @ [n] @ tr') [] tr
      
    
        
