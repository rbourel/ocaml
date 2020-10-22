type 'a narbr =  Noeud of 'a * 'a narbr list

let feuille v = Noeud(v,[])
let noeud v l = Noeud(v,l)
let valeur a = match a with
  | Noeud(x,_) -> x
                           
let sous_arbres a = match a with
  | Noeud(_,l) -> l

let a1 = feuille 4
let a2 = noeud 3 [a1; a1]

(* TEST *)
(* Doivent retourner true *)
let _ = valeur a1 = 4
let _ = valeur a2 = 3
let _ = sous_arbres a1 = []
let _ = sous_arbres a2 = [a1;a1]
(* END TEST *)

let rec compterN f l = match l with
  | [] -> 0
  | x :: l1 -> f x + compterN f l1
             
let rec compter a = match a with
   | Noeud(_,[]) -> 1
  | Noeud(x,arb :: l1) -> compter(arb) + compterN compter l1 
  
  
(* TEST *)
(* doit retourner 2 *)
let _ = compter a2
(* END TEST *)  


let max a b = if a > b then a else b 
  
  
let rec pluslongue a = match a with
  | Noeud(_,[]) -> 1
  |Noeud(x,arb :: l1) -> max (compter arb) (compterN compter l1)

let a3 = noeud 8 [a1; a2; a1]
(* TEST *)
(* doit retourner 3 *)
let _ = pluslongue a3
(* END TEST *)

let rec app f l = match l with
  | [] -> []
  | x :: l1 -> f x @ app f l1
    
let listsa a = match a with
  | Noeud(_,[]) -> []
  | Noeud(c,l1) -> app listsa l1 @ [a] 

let f4 = feuille 4
let f10 = feuille 10
let f12 = feuille 12
let f13 = feuille 13
let f20 = feuille 20
let f21 = feuille 21
let n7 = noeud 7 [ f10; f12; f13 ]
let n3 = noeud 3 [ f4; n7; f20]
let n5 = noeud 5 [ n3; f21 ]

let f22 = feuille 22
let n14 = noeud 7 [ f10; f12; f13 ]
let n15 = noeud 3 [ f4; n14; f20]
let n17 = noeud 5 [ n15; f21 ]

(* TEST *)
 let _ = listsa n5
(* Ceci doit retourner true *)
let _ =
  List.sort compare (listsa n5) = List.sort compare [f4; f10; f12; f13; f20; f21; n7; n3; n5]
(* END TEST *)
  

let rec appV f l = match l with
  | [] -> []
  | x :: l1 -> f x @  appV f l1
  
let rec listbr a = match a with
  | Noeud(v,[]) -> [[v]]
  | Noeud(v,l) -> let res = List.fold_left (fun acc elt -> acc @ listbr elt) [] l in
                   List.map (fun l -> v :: l) res 

(* TEST *)
let _ = listbr n5
(* doit retourner true *)
let _ =
  let res = [
    [5; 3; 4];
    [5; 3; 7; 10];
    [5; 3; 7; 12];
    [5; 3; 7; 13];
    [5; 3; 20];
    [5; 21]
  ] in
  List.sort compare (listbr n5) =
  List.sort compare res
(* END TEST *)

let rec egalR f l1 l2 = match l1,l2 with
  | [], [] -> true
  | [], _ -> false
  | _ , [] -> false
  | (v1 :: l3), (v2 :: l4) -> if(f v1 != f v2) then false else egalR f l3 l4 

                   
let egal a b = match a,b with
  | Noeud(v,[]),Noeud(v2,[]) -> v = v2
  | Noeud(v1,l1), Noeud(v2,l2) -> v1 = v2 && egalR sous_arbres l1 l2

(* TEST *)
(* doit retourner true *)
let _ = egal n5 n5
(* END TEST *)

let rec remplaceR f l = match l with
  | [] -> []
  | x :: l1 -> f x :: remplaceR f l1
             
let rec remplace a1 a2 a = match a with
  | Noeud(v,[]) -> if(egal a1 a) then a2 else a
  | Noeud(v,l1) -> if(egal a a1) then a2 else remplaceR remplace l1

let n42 = noeud 42 [feuille 2048]
let res = noeud 5 [(noeud 3 [f4; n42; f20]); f21]

(* TEST *)
(* ceci doit retourner true *)
let _ = (remplace n7 n42 n5) = res
(* END TEST *)
