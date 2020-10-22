let rec longueur l = match l with
  | [] -> 0
  | x :: li -> 1 + longueur li

(* TEST *)
(* Ceci doit retourner 3 *)
let res = longueur [1;2;3]
(* END TEST *)

let rec appartient e l = match l with
  | [] -> false
  | x :: li -> if x = e then true else appartient e li

(* TEST *)
(* Ceci doit retourner false *)
let res = appartient 4 [1;2;3]
(* END TEST *)

let  rec rang e l = if appartient e l then match l with
    | [] -> 0
    | x :: li -> if(x = e) then 1 else 1 + rang e li
    else -1
  
(* TEST *)
(* Ceci doit retourner 2 *)
let res = rang 2 [3;2;1]
(* END TEST *)

let rec concatl l1 l2 = match l1 with
  | [] -> l2
  | x :: li -> x :: concatl li l2

(* TEST *)
(* Ceci doit retourner [1;2;3;4;5;6] *)
let res = concatl [1;2;3] [4;5;6]
(* END TEST *)

let rec debliste l n = match l, n with
  |[],0 -> []
  | li,0 -> []
  |[],ni -> []
  | (x::li), ni -> x :: debliste li (n-1) 

(* TEST *)
(* Ceci doit retourner [1; 2; 3] *)
let res = debliste [1;2;3;4;5;6;7] 3
(* END TEST *)

let rec finliste l n = if longueur l < n then l else match (l,longueur l - n) with
  | li, 0 -> li
  | (x :: li),ni -> finliste li n
  | [], ni -> []
  
  

(* TEST *)
(* Ceci doit retourner [5; 6; 7] *)
let res = finliste [1;2;3;4;5;6;7] 3
(* END TEST *)

let rec remplace x y l = match l with
  | [] -> []
  | e :: li -> if(e = x) then y :: remplace x y li else e :: remplace x y li

(* TEST *)
(* Ceci doit retourner [1; 42; 3; 42; 5] *)
let res = remplace 2 42 [1;2;3;2;5]
(* END TEST *)

let rec entete l l1 = match l,l1 with
  |[],l10 -> true
  |l0,[] -> false
  |(x :: l0),(y :: l10) -> if(x != y) then false else  entete l0 l10
                      

(* TEST *)
(* Ceci doit retourner true *)
let res = entete [1;2;3] [1;2;3;2;5]
(* END TEST *)

let rec sousliste l l1 = if longueur l > longueur l1 then false else match l,l1 with
  |(x :: l0),l1 -> if(entete l0 l1) then true else sousliste l0 l1
  |[],l1 -> false 
(* TEST *)
(* Ceci doit retourner true *)
let res = sousliste [1;2;3] [1;2;3;2;5]
(* END TEST *)

let oter l l1 = if(entete l l1) then finliste l1 (longueur l1 - longueur l) else l1

(* TEST *)
 (* Ceci doit retourner [2; 5] *)
 let res = oter [1;2;3] [1;2;3;2;5]
(* END TEST *)

 let rec remplacel l1 l2 l = match l with
   | x :: l0 -> if (entete l1 l) then concatl l2 (remplacel l1 l2 (oter l1 l))
                else x :: remplacel l1 l2 l0
   |[] -> []
     

(* TEST *)
(* Ceci doit retourner [4; 5; 6; 2; 5; 6; 2; 1; 3; 8] *)
let res = remplacel [1;2;1] [5;6] [4;1;2;1;2;1;2;1;2;1;3;8]
(* END TEST *)

let rec supprimel l1 l = if (l1 = []) then l else match l with
  | x :: l0 -> if (entete l1 l) then  (supprimel l1 (oter l1 l))
               else x :: supprimel l1 l0
  |[] -> []
(* TEST *)
(* Ceci doit retourner [4; 2; 1; 3; 8] *)
let res = supprimel [1;2] [4;1;2;1;2;1;3;8]

(* Ceci doit retourner [1; 2; 3] *)
let res = supprimel [] [1;2;3]
(* END TEST *)

let maxl l = failwith "à faire, si vous avez le temps"

(* TEST *)
(* Ceci doit retourner 5 *)
let res = maxl [-1; 3; -4; 2; -3; 4; -2; 3; -1]
(* END TEST *)
