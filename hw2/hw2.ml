(* --------------------------------------------------------------------*)
(* QUESTION 1: House of Cards                                          *)
(* --------------------------------------------------------------------*)

(* Q1: Comparing cards *)
(* Comparing two ranks *)
let dom_rank (r1 : rank) (r2 : rank) = match r1, r2 with
  | Ace, _ -> true
  | King,  (King|Queen|Jack|Ten|Nine|Eight|Seven|Six) -> true 
  | Queen, (Queen|Jack|Ten|Nine|Eight|Seven|Six) -> true
  | Jack,  (Jack|Ten|Nine|Eight|Seven|Six) -> true
  | Ten,   (Ten|Nine|Eight|Seven|Six) -> true
  | Nine,  (Nine|Eight|Seven|Six) -> true 
  | Eight, (Eight|Seven|Six) -> true
  | Seven, (Seven|Six) -> true
  | r1, r2 -> r1 = r2

(* Comparing two cards (r1, s1) and (r2, s2) *)
let dom_card (c1 : card) (c2 : card) = match c1, c2 with
  | (r1, s1), (r2, s2) ->
      if s1 = s2 then dom_rank r1 r2
      else dom_suit s1 s2
  
(* Q2: Insertion Sort – Sorting cards in a hand *)
let rec insert (c : card) (h : hand) : hand = match c, h with
  | c, Empty -> Hand (c, h)
  | c, Hand (c2, h2) -> 
      if dom_card c c2 = true then Hand(c, Hand(c2, h2))
      else Hand(c2, insert c h2)

let rec sort (h : hand) : hand = match h with
  | Empty -> h
  | Hand (c1, h1) ->
      insert c1 (sort h1)

(* Q3: Generating a deck of cards *)
let generate_deck (suits : suit list) (ranks : rank list) : card list = 
  match suits,ranks with
  | [], [] -> [] 
  | [], b::b' -> []
  | s, r ->
      let rec generateD s r : card list = match s, r with
        | [], [] -> []
        | [], b::b' -> []
        | a::a', b::b' -> (b, a) :: (generateD s b')
        | a::a', [] -> generateD a' ranks
      in
      generateD suits ranks
      

(* Q4: Shuffling a deck of cards *)
let rec split (deck : card list ) (n: int): (card*card list)  = match deck with
  | [] -> raise Domain
  | [c] -> (c,[])
  | h :: t ->
      let rec append d n' acc = match d, n' with
        | [],n' -> raise Domain
        | h :: t, 0 -> (h, acc @ t)
        | h :: t, n' -> (append (t) (n'-1) (acc @ [h]))
      in 
      append deck n []
  
let shuffle (deck : card list) : card list = match deck with
  | [] -> []
  | [c] -> [c]
  | h :: t -> 
      let size = List.length deck in
      let rec select deck n = match split deck (Random.int n) with
        | c,[] -> [c]
        | c,d -> c :: (select d (n-1))
      in
      select deck size
                    
                    
        

(* --------------------------------------------------------------------*)
(* QUESTION 2: Sparse Representation of Binary Numbers                 *)
(* ------------------------------------------------------------------- *)

(* Q1: Incrementing a sparse binary number *)
let inc (ws : nat ) : nat = match ws with
  | 1 :: ws' -> 
      let rec check (list:nat) : nat = match list with
        | [] -> [1]
        | h :: [] -> [2*h]
        | h :: (t :: t') -> 
            if (2*h = t) then check (t :: t')
            else (2*h) :: (t :: t')
      in
      check ws 
  | [] -> [1] 
  | ws -> 1 :: ws
  
(* Q2: Decrementing a sparse binary number *)
let dec (ws : nat) : nat = match ws with
  | [1] -> []
  | [2] -> [1]
  | a :: ws' ->
      let rec check (list:nat) :nat = match list with
        | 1 :: t -> t
        | 2 :: t -> 1 :: t
        | h :: t ->
            if (h/2 = 2) then 1 :: ((h/2) :: t)
            else check ((h/2) :: ((h/2) :: t))
        | [] -> raise Domain
      in
      check ws 
  | [] -> raise Domain
  
(* Q3: Adding sparse binary numbers *)
let rec add (m : nat) (n : nat) : nat  = match m, n with
  | [], [] -> []
  | m, [] -> m
  | [], n-> n 
  | a, b ->
      if dec b != [] then add (inc a) (dec b) 
      else inc a
  
(* Q4: Converting to integer - tail recursively *)
let rec toInt (n : nat) (acc : int) : int = match n, acc with
  | [] , acc -> 0
  | h :: t, acc -> 
      if (t = []) then (acc+h)
      else toInt t (acc+h)

let sbinToInt (n : nat) : int =
  toInt n 0
