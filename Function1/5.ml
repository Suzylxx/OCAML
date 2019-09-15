let rec parseExp toklist sc =
  parseSExp
    toklist
    (fun toklist' exp -> match toklist' with
       | SEMICOLON :: toklist'' -> sc toklist'' exp
       | _ -> raise (Error "Expected a single semicolon"))

and parseSExp toklist sc =
  parsePExp toklist (fun toklist' exp -> match toklist' with
      | PLUS :: toklist'' -> parseSExp toklist'' 
                               (fun sc' exp' -> sc sc' (Sum(exp, exp')))
      | SUB :: toklist'' -> parseSExp toklist'' 
                              (fun sc' exp' -> sc sc' (Minus(exp,exp')))
      | _ -> sc toklist' exp
    )

and parsePExp toklist sc =
  parseAtom toklist (fun toklist' exp -> match toklist' with
      | TIMES :: toklist'' -> parsePExp toklist''
                                (fun sc' exp' -> sc sc' (Prod(exp, exp')))
      | DIV :: toklist'' -> parsePExp toklist''
                              (fun sc' exp' -> sc sc' (Div(exp, exp')))
      | _ -> sc toklist' exp
    )

and parseAtom toklist sc = match toklist with
  | INT h::t -> sc t (Int h)
  | LPAREN :: t -> 
      parseSExp t (fun t' exp -> match t' with
          | RPAREN :: t'' -> sc t'' exp
        )

(* parse : string -> exp *)
let parse string =
  parseExp
    (lex string)
    (fun s e -> match s with
       | [] -> e
       | _ -> raise (Error "Incomplete expression"))

(* eval : string -> int *)
let eval e = eval' (parse e)
    
    


