(* ---------------------------------------------------- *)

(* Processing finite objects lazily is also useful;
   it corresponds to demand driving compution.
*)
(* ---------------------------------------------------- *)
(* We define next a lazy list; this list is possibly
   finite; this is accomplished by a mutual recursive
   datatype.

   'a lazy_list defines a lazy list; we can observe the 
   head and its tail. For the tail we have two options:
   we have reached the end of the list indicated by the 
   constructor None or we have not reached the end 
   indicated by the constructor Some and we expose
   another lazy list of which we can observe the head and the tail.  

*)

(* ---------------------------------------------------- *)         
(* Q1 *)

(* 
   val take : int -> 'a lazy_list -> 'a list 
*)
let rec take n s = match n with
  | 0 -> []
  | n -> s.hd :: take' (n-1) (force s.tl)
and take' n xs = match xs with
  | None -> []
  | Some x -> take n x

(* val map : ('a -> 'b) -> 'a lazy_list -> 'b lazy_list
*)
let rec map f s = 
  {hd = f (s.hd);
   tl = Susp (fun () -> map' f (force s.tl))
  }
and map' f xs = match xs with
  | None -> None
  | Some xs -> Some (map f xs)

(* 
  val append : 'a lazy_list -> ('a lazy_list) option susp -> 'a lazy_list
*)
let rec append s1 s2 = match (force s2) with
  | None -> s1
  | Some s -> match s1 with
    | {hd; tl} -> match (force tl) with
      | None -> {hd = s1.hd; tl = s2}
      | Some tl -> {hd = s1.hd; tl = Susp (fun () -> Some (append tl s2))}
  
(* ---------------------------------------------------- *)
(* val interleave : 'a -> 'a list -> 'a list lazy_list *)
let rec interleave x l = match l with
  | [] -> {hd = [x]; tl = Susp(fun () -> None)}
  | h::t -> {hd=x::l; 
             tl=Susp(fun () -> Some (map(fun y-> h::y) (interleave x t)))}
  
(* ---------------------------------------------------- *)
(* val flatten : 'a lazy_list lazy_list -> 'a lazy_list = <fun>
*)
let rec flatten s = match s with
  |{hd; tl} -> match force hd.tl with
    |None -> (match force tl with
        |None -> {hd=hd.hd; tl=Susp(fun () -> None)}
        |Some l -> {hd=hd.hd; tl=Susp(fun () -> Some(flatten l))})
    |Some l -> (match force tl with
        |None -> {hd=hd.hd; tl=hd.tl}
        |Some l' -> {hd=hd.hd; 
                     tl=Susp(fun () -> Some(append l (Susp(fun () -> Some(flatten l')))))})

                                                  
(* ---------------------------------------------------- *)
(* Permute *)
let rec permute l = match l with
  |[] -> {hd=[]; tl=Susp(fun () -> None)}
  |h::t -> match force ((permute t).tl) with
    |None ->flatten {hd=interleave h t; tl = Susp(fun () -> None)}
    |Some l' ->flatten {hd=interleave h t; 
                        tl = Susp(fun () -> Some(map (interleave h) l'))}

(* ---------------------------------------------------- *)         
(* Q2 *)
                   
let rec hailstones n = match (n mod 2) with
  |0 -> {hd=n; tl=Susp(fun () -> Some(hailstones (n/2)))}
  |1 -> {hd=n; tl=Susp(fun () -> Some(hailstones (3*n+1)))}
        
        
