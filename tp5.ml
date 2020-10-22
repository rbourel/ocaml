let rec supp v l = match l with
  | [] -> []
  | x :: l1 -> if(x < v) then supp v l1 else x :: supp v l1

let rec keep v l = match l with
  | [] -> []
  | x :: l1 -> if(x <  v) then x :: keep v l1 else  keep v l1

let  split v l =  (keep v l , supp v l)

(* TEST *)
(* doit retourner [-12; 1; 3], [12; 27; 7; 8; 6; 12; 42] *)
let resupp = keep 4 [12; 27; -12; 7; 8; 1; 3; 6; 12; 42]
let res1,res2 = split 4 [12; 27; -12; 7; 8; 1; 3; 6; 12; 42]
(* END TEST *)

let rec concatl l1 l2 = match l1 with
  | [] -> l2
  | x :: li -> x :: concatl li l2
             
let rec qs l = match l with
  | [] -> []
  | x  :: l1 -> concatl (keep x (qs l1)) (x :: supp x (qs l1))

(* TEST *)
(* doit retourner [-12; 1; 3; 4; 6; 7; 8; 12; 12; 27; 42] *)
let res = qs [4; 12; 27; -12; 7; 8; 1; 3; 6; 12; 42]
(* END TEST *)

let rec longueur l = match l with
  | [] -> 0
  | x :: li -> 1 + longueur li
             
let rec kieme k l = if(k < 1) then assert false else match qs l with
  | [] -> assert false
  | x :: l1 -> if(k = 1) then x else kieme (k-1) l1
             
(* TEST *)
(* doit retourner 8 *)
let res = kieme 7 [4; 12; 27; -12; 7; 8; 1; 3; 6; 12; 42]
(* END TEST *)

             
let rec jqastable x f = if f(x) <> x then jqastable (f x) f else f x

(* TEST *)
(* doit retourner 1 *)
let res = jqastable 13 (fun x -> if (x = 1) then 1 else if (x mod 2 = 1) then 3 * x + 1 else x / 2)
(* END TEST *)

let rec unebulle l = match l with
  | [] -> []
  | x :: [] -> l
  | x :: y :: l1 -> if( x < y) then x :: unebulle (y :: l1) else y :: unebulle (x ::l1)

(* TEST *)
(* doit retourner [4; 12; -12; 27; 7; 8; 1; 3; 6; 27; 12; 42] *)
let res = unebulle [4; 12; 27; -12; 7; 8; 1; 3; 6; 42; 12]
(* END TEST *)

let tribulle l = jqastable l unebulle
                                                            

(* TEST *)
(* doit retourner [-12; 1; 3; 4; 6; 7; 8; 12; 12; 27; 42] *)
let res = tribulle [4; 12; 27; -12; 7; 8; 1; 3; 6; 12; 42]
(* END TEST *)

let rec merge ll = match ll with
  | [] -> []
  | l1 :: l2 -> concatl l1 (merge l2)

(* TEST *)
(* doit retourner [1;2;3;5] *)
let res = merge [[1];[2;3];[5]]
(* END TEST *)

let rec create f k = match k with
  | 0 -> []
  | n -> create f (n-1)@[f n]

(* TEST *)
(* doit retourner [2; 3; 4; 5] *)
let res = create (fun x -> x+1) 4
(* END TEST *)

let rec insert j ll = match ll with
  | [] -> []
  | x :: l2 -> concatl [j] x ::  (insert j l2)

(* TEST *)
(* doit retourner [[1;3;5];[1;7;3;9];[1];[1;6]]*)
let res = insert 1 [[3;5];[7;3;9];[];[6]]
(* END TEST *)

let partition n =
  let rec aux n k =
    match n,k with
    | (0,0) -> [[]]
    | (n,0) -> []
    | (n,k) when n = k -> aux n n
    | (n,k) -> merge insert j aux((n-j) j) :: aux (n-1) k 

(* TEST *)
(* doit retourner une liste contenant [5], [4;1], [3;2], [3;1;1], [2;2;1],
   [2;1;1;1], [1;1;1;1;1] dans un ordre arbitraire *)
let res = partition 5
(* END TEST *)
