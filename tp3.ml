(* TP3. Bourel Rodrigue Cartron Yugo*)
#load "graphics.cma";;
(* Remplacer unit par votre définition de type *)
type matrix ={a:float; b:float; c:float; d:float}
type vector ={x:float;y:float}

let mkm a b c d = {a=a;b=b;c=c;d=d}
let mkv x y = {x=x;y=y}

let vtop v =( v.x,v.y)

(* TEST *)
(* doit retourner (1.;2.) *)
let res = vtop (mkv 1. 2.)
(* END TEST *)

let mtop m = ((m.a,m.b),(m.c,m.d))

(* TEST *)
(* doit retourner ((1.;2.);(3.;4.)) *)
let res = mtop (mkm 1. 2. 3. 4.)
(* END TEST *)

(* Evaluate me *)
let m1 = mkm 0.0 0.0 0.172 0.496
let m2 = mkm 0.076 0.3122 0.257 0.204
let m3 = mkm 0.821 (-0.028) 0.030 0.845
let m4 = mkm 0.024 (-0.356) 0.323 0.074
let v1 = mkv 0.496 (-0.091)
let v2 = mkv 0.494 0.133
let v3 = mkv 0.088 0.176
let v4 = mkv 0.470 0.260

let mt m v = mkv ((m.a*.v.x)+.(m.b*.v.y))((m.c*.v.x)+.(m.d*.v.y))

(* TEST *)
(* Le test suivant doit retourner true *)

let res = (mt (mkm 0. 1. (-1.) 0.) (mkv 3. 4.)) = (mkv 4. (-3.))
(* END TEST *)

let sv v1 v2 = mkv (v1.x+.v2.x)(v1.y+.v2.y)

(* TEST *)
(* Le test suivant doit retourner true *)
let res = (sv (mkv 1. 2.) (mkv 3. 4.)) = (mkv 4. 6.)
(* END TEST *)

let genapplin m  =  fun v -> mt m v 

(* TEST *)
(* Le test suivant doit retourner true *)
let res = (genapplin (mkm 0. 1. 2. 3.) (mkv 2. 3.)) = (mkv 3. 13.)
(* END TEST *)

let gentraffine m v = (fun n -> sv ( genapplin m n) v)

(* TEST *)
(* Le test suivant doit retourner true *)
let res = (gentraffine (mkm 1. 2. 3. (-1.)) (mkv 1. 2.) (mkv 2. 3.)) = (mkv 9. 5.)
(* END TEST *)

(*crée 4 fonctions affines*)
let les4tr = (gentraffine m1 v1, gentraffine m2 v2, gentraffine m3 v3, gentraffine m4 v4)
                                                                         
let elemrang (a,b,c,d) = match Random.int 4 with
  |0->a
  |1->b
  |2->c
  |3->d
  | _ -> a

(* Le canevas pour traff est volontairement omis *)
let traff () = elemrang les4tr

let suite n =
let rec suite_inte i v =
  let () =  v in
  if i < n then suite_inte (i+1) (traff () v)
     else () in
     suite_inte 0 (mkv 0.5 0.0)


let identity f x = f x ; x

let print_suite f n =
let rec suite_interne k v =
  let () = f v in
  if k < n then suite_interne (k+1) (traff () v)
     else () in
     suite_interne 0 (mkv 0.5 0.0)
                      
            
(* TEST *)
(* Cela devrait imprimer ceci :
   (0.50,0.00)
   (0.50,-0.01)
   (0.50,0.19)
   (0.49,0.35)
   (0.50,0.17)
   (0.49,0.33)
*)
let res =
  let () = Random.init 0 in
  let f v = 
    let (x,y) = vtop v in 
    Printf.printf "(%.2f,%.2f)\n" x y in 
  print_suite f 5
(* END TEST *)
       

let run n = Graphics.open_graph "";
            let () = print_suite (fun v -> Graphics.plot (int_of_float(v.x*.500.)) (int_of_float((v.y*.500.)))) n in
              match Graphics.wait_next_event with
              |keypressed -> Graphics.close_graph ()
              |_-> ()                      

(* TEST *)
(* je vous laisse la surprise *)
                  
let res = run 1000000
(* END TEST *)

