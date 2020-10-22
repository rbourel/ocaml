let rec pow n = if(n = 0) then 1 else 2 * pow (n-1)

let _ = pow 20

let rec f n = match n with
  | 0 -> 0
  | 1 -> 1
  | n when n mod 2 = 0 -> f (n/2)
  | n  -> f (n/2) + f(n/2 + 1)

exception ListeVide of string 

let pop l = match l with
  | [] -> raise (ListeVide "Liste Vide")
  | x :: l1 -> (x,l1)

let  l1 = [1;2;3;4;5]

let _ = pop l1

let push v l = l @ [v]
let rec push2 v l = match l with
  | [] -> [v]
  | x :: l1 -> x :: push2 v l1

let _ = push2 6 l1

type 'a file = {file_out : 'a list; file_in:'a list}

let is_empty l = match l with
  | {file_out = []; file_in = []} -> true
  | _ -> false

let push' v l = match l with
  | {file_out=ll;file_in = l1} -> {file_out = ll; file_in = v::l1}
 
let pop' l = if(is_empty l) then raise (ListeVide "Liste Vide")
                 else match l with
                      | {file_out= ll;file_in=l1} -> (1,{file_out = ll;file_in = l1})
                     

(* TEST *)
(* doit retourner (1,{file_out = [2;3]; file_in = []}) *)
let _ =
  let f1 = push' 1 in
  let f2 = push' 2 f1 in
  let f3 = push' 3 f2 in
  pop' f3
(* END TEST *)                                                                    
