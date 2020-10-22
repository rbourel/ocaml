(* remplacer par votre type *)
type 'a arbin = Feuille of 'a |  Noeud of 'a * 'a arbin * 'a arbin

let feuille v =  Feuille v
let noeud v g d = Noeud (v,g, d)

let rec compter a = match a with
  | Feuille v -> 1
  | Noeud (v,g,d) ->  compter g + compter d

(* TEST *)
(* ceci doit retourner 3 *)
let arbre_test = noeud 12 (feuille 5) (noeud 7 (feuille 6) (feuille 8))
let _ = compter arbre_test
(* END TEST *)

let rec to_list a = match a with
  | Feuille v -> [v]
  |Noeud (v,g,d) -> to_list g@[v]@to_list d

(* TEST *)
(* ceci doit retourner [5; 12; 6; 7; 8] *)
let _ = to_list arbre_test
(* END TEST *)

let rec inserer v a = match a with
  | Feuille _ -> Noeud(v,Feuille "Nil",Feuille "Nil")
  | Noeud(va,g,d) -> if(v < va) then Noeud(va,inserer v g,d)
                     else Noeud(va,g,inserer v d)
(*  | _ ->  Noeud(v,Feuille "Nil",Feuille "Nil") *)
     

let rec constr l = match l with
  | [] -> Feuille "Nil"
  | x :: l1 ->  inserer x (constr  l1)
                     

let l = ["celeri";"orge";"mais";"ble";"tomate"; "soja"; "poisson"]
(* TEST *)
(* Ceci doit retourner true *)
let _ = List.filter (fun s -> s <> "Nil") (to_list (constr l)) = List.sort compare l
(* END TEST *)

type coord = int * int
type 'a arbinp = (coord * 'a) arbin
let d = 5
let e = 4

let rec compteE a =  match a with
  | Feuille _ -> 0
  |Noeud(v,g,d)-> 1 + compteE g + compteE d

(* let placer a = let rec placeXY a x y = match a with
  | Feuille v -> Feuille((x+e,y+d),v, x+e)
  | Noeud (v,g,dr) -> let (gauche, posG) = placeXY  g (x+d) y in let (droite, posD) = placeXY dr (x+d) (posG + e) in
                                                         (Noeud((gauche),(posG+e,x+d)), v),(droite,posD) in let a,_ = placeXY a 0 0 in a *)

let t =
  noeud 'a'
    (feuille 'j')
    (noeud 'b'
       (noeud 'c'
          (noeud 'd' (feuille 'w') (feuille 'k'))
          (feuille 'z'))
       (feuille 'y'))

(* Pour tester *)
let res = placer t

(* TEST *)
(* Ceci doit retourner true *)
let res = (placer t = noeud ((8, 5), 'a')
     (feuille ((4, 10), 'j'))
     (noeud ((32, 10), 'b')
        (noeud ((24, 15), 'c')
           (noeud ((16, 20), 'd') (feuille ((12, 25), 'w')) (feuille ((20, 25), 'k')))
           (feuille ((28, 20), 'z')))
        (feuille ((36, 15), 'y'))))
(* END TEST *)
