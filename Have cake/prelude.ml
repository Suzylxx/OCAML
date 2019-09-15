exception NotImplemented

(* Question 1 : Let's have cake! *)

type price = float
type weight = float
type calories = int
type ingredient = Nuts | Gluten | Soy | Dairy

type cupcake = Cupcake of price * weight * calories * ingredient list

let c1 = Cupcake (2.5, 80.3, 250, [Dairy; Nuts])
let c2 = Cupcake (2.75, 90.5, 275, [Dairy; Soy])
let c3 = Cupcake (3.05, 100.4, 303, [Dairy; Gluten; Nuts])
let c4 = Cupcake (3.25, 120.4, 330, [Dairy; Gluten ])

let cupcakes = [c1 ; c2 ; c3 ; c4]

(* Question 2 : Generic Tree Traversals *)

type 'a tree = Node of 'a * 'a tree * 'a tree | Empty
