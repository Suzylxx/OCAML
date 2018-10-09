(* The code here will be added to the top of your code automatically.
   You do NOT need to copy it into your code.
*)

exception Domain
exception NotImplemented

type suit = Clubs | Spades | Hearts | Diamonds

type rank =  Six | Seven | Eight | Nine | Ten |
             Jack | Queen | King | Ace

type card = rank * suit

type hand = Empty | Hand of card * hand

(* dom_suit : suit -> suit -> bool

   dom_suit s1 s2 = true iff suit s1 beats or is equal to suit s2
                    relative to the ordering S > H > D > C
   Invariants: none
   Effects: none
*)

let dom_suit s1 s2 = match s1, s2 with
  | Spades, _        -> true
  | Hearts, Diamonds -> true
  | Hearts, Clubs    -> true
  | Diamonds, Clubs  -> true
  | s1, s2           -> s1 = s2

type nat = int list (* increasing list of weights, each a power of two *)
