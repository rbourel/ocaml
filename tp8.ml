type coul = Coeur | Trefle | Pique | Carreau
type haut = Sept | Huit | Neuf | Dix | Valet | Dame | Roi | As
type carte = Carte of haut * coul

let coul c = match c with
  | Carte (h,co) -> co
let haut c = match c with
  | Carte (h,co) -> h

exception NbImp of string 

let haut_of_int i = match i with
  | 7 -> Sept
  | 8 -> Huit
  | 9 -> Neuf
  | 10 -> Dix
  | 11 -> Valet
  | 12 -> Dame
  | 13 -> Roi
  | 14 -> As
  | _ -> raise (NbImp "Nombre Impossible")
                                                      

(* TEST *)
(* doit retourner Dame *)
let _ = haut_of_int 8
(* END TEST *)


exception NonCouleur of string
                      
let coul_of_string s = match s with
  | "Coeur" -> Coeur
  | "Trefle" -> Trefle
  |"Pique" -> Pique
  | "Carreau" -> Carreau
  | _ -> raise (NonCouleur "Ce n'est pas une couleur")

(* TEST *)
(* doit retourner Pique *)
let _ = coul_of_string "Pique"
(* END TEST *)

let carte i s = Carte (haut_of_int i, coul_of_string s)
(* TEST *)
(* ces tests doivent retourner true *)
let _ = (haut (carte 8 "Trefle")) = Huit
let _ = (coul (carte 14 "Trefle")) = Trefle
(* END TEST *)

 let string_of_haut hau = match hau with
   |Sept -> "7"
   |Huit -> "8"
   |Neuf -> "9"
   |Dix -> "10"
   |Valet ->"Valet"
   |Dame -> "Dame"
   |Roi -> "Roi"
   |As -> "As"

let string_of_coul cou = match cou with
  |Coeur -> "Coeur"
  |Trefle -> "Trefle"
  |Pique -> "Pique"
  |Carreau -> "Carreau"
       
let string_of_carte c = match c with
  | Carte (ha,co) -> (string_of_haut ha)^" de "^(string_of_coul co)

(* TEST *)
(* ceci doit retourner la chaîne "Valet de Pique" *)
let res = string_of_carte (carte 11 "Pique")

(* ceci doit retourner la chaîne "9 de Trefle" *)
let res = string_of_carte (carte 9 "Trefle")
(* END TEST *)

let int_to_coul i = match i with
  | 0 -> Coeur
  | 1 -> Trefle
  | 2 -> Pique
  | 3 -> Carreau
  | _ -> assert false

let createRandom a = Random.int a
                   
let random_carte () =  Carte (haut_of_int (createRandom 8  + 7), int_to_coul( createRandom 4))

let _ = random_carte ()

let rec appartient e l = match l with
  | [] -> false
  | x :: li -> if x = e then true else appartient e li
             
let rec ajtcarte l = let c =  random_carte() in if (appartient c l) then ajtcarte l else c :: l

(* TEST *)
(* ceci doit retourner true *)
let res =
  let l1 = ajtcarte [] in
  let l2 = ajtcarte l1 in
  match l1,l2 with
  | [c],[c1; c2] -> c = c2 && c1 <> c2
  | _ -> false
(* END TEST *)

let rec faitjeu n = match n with
  | 0 -> []
  | _ -> let l = faitjeu (n-1) in ajtcarte l

 let _ = faitjeu 20

       
 let rec regardeColOrHau c l = match c,l with
   | c,[] -> []
   | Carte(ha,co) , (Carte(hax,cox) :: l1) -> if(c = Carte(hax,cox)) then regardeColOrHau c l1
                                              else if(co = cox || ha = hax) then [c]@[Carte(hax,cox)]@regardeColOrHau c l1 else []
                  
let dessus l = match l with
  | [c] -> c
  | x :: l1 -> x
  | [] -> random_carte () (* Inch ça va pas jusque là *)

exception ErreurFormat of string
        
let rec reduc l = match l with
  | x :: y :: z :: l1 -> if haut(dessus x) = haut(dessus z) || haut (dessus x) = haut( dessus z)
                         then let l2 = y@x in l2::z::l1
                         else (x :: (reduc (y::z::l1)))
  | y :: z :: [] -> l
  | _ -> raise (ErreurFormat "La liste ne contient qu'une colonne")

let p1 = [carte 14 "Trefle";  carte 10 "Coeur" ]
let p2 = [carte 7 "Pique";    carte 11 "Carreau" ]
let p3 = [carte 14 "Carreau"; carte 8 "Pique" ]
let p4 = [carte 7 "Carreau";  carte 10 "Trefle" ]

let p'1 = p2@p1

(* TEST *)
(* ceci doit retourner true *)
let _ = (reduc [p1; p2; p3; p4]) = [p'1; p3; p4]
(* END TEST *)
                      
let rec reussite l = if reduc l = l then l else reussite  (reduc l)

let p''1 = p3@p'1
(* TEST *)
(* ceci doit retourner true *)
let res = (reussite [p1; p2; p3; p4]) = [p''1; p4]
(* END TEST *)

(* Copiez la ligne suivante (avec le #) dans le toplevel (fenêtre du bas) et
   tapez Entrée
#load "graphics.cma";;
*)
open Graphics

let b = white
let n = black
let r = red

let carr   = [| [|b;b;b;b;b;r;b;b;b;b;b|];
                [|b;b;b;b;r;r;r;b;b;b;b|];
                [|b;b;b;r;r;r;r;r;b;b;b|];
                [|b;b;r;r;r;r;r;r;r;b;b|];
                [|b;r;r;r;r;r;r;r;r;r;b|];
                [|b;b;r;r;r;r;r;r;r;b;b|];
                [|b;b;b;r;r;r;r;r;b;b;b|];
                [|b;b;b;b;r;r;r;b;b;b;b|];
                [|b;b;b;b;b;r;b;b;b;b;b|] |]

let tref   = [| [|b;b;b;b;b;n;n;b;b;b;b|];
                [|b;b;b;b;n;n;n;n;b;b;b|];
                [|b;b;b;b;n;n;n;n;b;b;b|];
                [|b;b;n;n;b;n;n;b;n;n;b|];
                [|b;n;n;n;n;n;n;n;n;n;n|];
                [|b;n;n;n;n;n;n;n;n;n;n|];
                [|b;b;n;n;b;n;n;b;n;n;b|];
                [|b;b;b;b;b;n;n;b;b;b;b|];
                [|b;b;b;b;n;n;n;n;b;b;b|] |]

let coeu   = [| [|b;b;r;r;b;b;b;r;r;b;b|];
                [|b;r;r;r;r;b;r;r;r;r;b|];
                [|b;r;r;r;r;r;r;r;r;r;b|];
                [|b;r;r;r;r;r;r;r;r;r;b|];
                [|b;b;r;r;r;r;r;r;r;b;b|];
                [|b;b;b;r;r;r;r;r;b;b;b|];
                [|b;b;b;b;r;r;r;b;b;b;b|];
                [|b;b;b;b;b;r;b;b;b;b;b|];
                [|b;b;b;b;b;r;b;b;b;b;b|] |]


let piqu   = [| [|b;b;b;b;b;n;b;b;b;b;b|];
                [|b;b;b;b;b;n;b;b;b;b;b|];
                [|b;b;b;b;n;n;n;b;b;b;b|];
                [|b;b;n;n;n;n;n;n;n;b;b|];
                [|b;n;n;n;n;n;n;n;n;n;b|];
                [|b;n;n;n;n;n;n;n;n;n;b|];
                [|b;n;n;n;b;n;b;n;n;n;b|];
                [|b;b;b;b;b;n;b;b;b;b;b|];
                [|b;b;b;b;n;n;n;b;b;b;b|] |]

let draw_haut h = match h with
  | Sept -> draw_string " 7"
  | Huit -> draw_string " 8"
  | Neuf -> draw_string " 9"
  | Dix -> draw_string "10"
  | Valet -> draw_string " V"
  | Dame -> draw_string " D"
  | Roi -> draw_string " R"
  | As -> draw_string " A"

let draw_coul c l coul = match coul with
  | Carreau -> draw_image (make_image carr) c (l+2)
  | Trefle -> draw_image (make_image tref) c (l+2)
  | Coeur -> draw_image (make_image coeu) c (l+2)
  | Pique -> draw_image (make_image piqu) c (l+2)

let draw_carte ca =
  let (c,l) = current_point() in
  draw_haut (haut ca); draw_coul (c+12) l (coul ca); moveto c (l+14)

let rec app f l = match l with
  | x :: [] -> f x
  | x :: l1 -> f x ; app f l1
  | _ -> assert false
               
let rec draw_pile l = let (x,y) = current_point() in
                      let rec aux l = match l with
  | [] -> ()
  | carte :: reste -> aux reste ; draw_carte carte
                      in
                      aux l ; moveto (x+30) y 
(* TEST *)
let () = Graphics.open_graph ""
let _ = draw_pile p''1
let () = Graphics.close_graph ()
(* END TEST *)

let rec draw_jeu j = let _ = moveto 5 5 in match j with
  | [] -> ()
  | pile :: reste -> draw_jeu reste ; draw_pile pile

let draw_reussite () = let _ = Graphics.open_graph "" in
                       let jeu = faitjeu 32 in
                       let jeu1 = faitjeu 6 in
                       let jeu2 = faitjeu 6 in
                       let jeu3 = faitjeu 6 in
                       let jeuf  = jeu::jeu1::jeu2::(jeu3::[]) in
                       draw_jeu (reduc jeuf)
(* TEST *)
                       
let res = draw_reussite ()
let () = Graphics.close_graph ()
(* END TEST *)
