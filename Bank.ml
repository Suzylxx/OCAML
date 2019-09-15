(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account p =
  let p' = ref p in
  let balance = ref 0 in
  let count = ref 0 in
  {update_passwd = (fun old_p new_p -> 
       if old_p = !p' then (count := 0; p' := new_p) 
       else (count := !count +1; raise wrong_pass));
   retrieve = (fun pass money -> 
       if !count >= 3 then raise too_many_attempts
       else if pass = !p' then
         (if money <= !balance then (count := 0; balance := !balance-money)
          else raise no_money)
       else (count := !count+1; raise wrong_pass));
   deposit = (fun pass money -> 
       if !count >= 3 then raise too_many_attempts
       else if pass = !p' then (count := 0; balance := !balance + money)
       else (count := !count +1; raise wrong_pass)); 
   print_balance = (fun pass -> 
       if !count >= 3 then raise too_many_attempts
       else if pass = !p' then (count := 0; !balance)
       else (count := !count +1; raise wrong_pass))} 
;;


(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec catalan_I n =
  let count = ref 0 in
  {num_rec = !count;
   result = 
     let rec catalan n = match n with
       | 0 -> (count := !count + 1; 1)
       | 1 -> (count := !count + 1; 1)
       | n -> (count := !count + 1; sum (fun i -> catalan i * catalan (n - 1 - i))) (n - 1)
     in
     catalan n}
;;
(* Q 2.2 : Memoization with a global store *)

let rec catalan_memo n =
  let rec catalan n = 
    let find = Hashtbl.find_opt store n in
    match find with
    | None -> 
        (let v =reccat catalan n in
         Hashtbl.add store n v; v)
    | Some v -> v
  in
  catalan n 
;;

(* Q 2.3 : General memoization function *)

let memo f stats =
  let store = Hashtbl.create 1000 in
  let rec g = (fun n -> match Hashtbl.find_opt store n with
      | None -> (let r =f g n in
                 stats.entries := !(stats.entries) +1; Hashtbl.add store n r; r)
      | Some v -> (stats.lkp := !(stats.lkp) +1; v)
    )
  in 
  g
;;

(* Q 2.4 : Using memo to efficiently compute the Hofstadter Sequence Q *)

let hofstadter_Q n =
  let stats = {entries = ref 0; lkp = ref 0} in
  let rec sequence f i = match i with
    | 0 -> f 0
    | i -> f i + sequence f (i-1)
  in
  let seq f= function
    | 1|2 -> 1
    | n -> sequence (fun i -> f(n-f(n-1))+f(n-f(n-2))) (n-1)
  in
  let v = memo seq stats in
  (v, stats)

;;
