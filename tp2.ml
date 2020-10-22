(* TP2. Mettez vos noms ici. *)

(* Compléter *)
let rec pair n = if(n = 0) then true else impair(pred n)
and impair n = if (n=0) then false  else pair(pred n);;

(* TEST *)
let res = pair 12   (* true *)
let res = impair 12 (* false *)
(* END TEST *)

(* Compléter *)
let rec sigma (a,b) = if(a > b)then 0 else b + sigma(a,pred b)

(* TEST *)
let res = sigma (-2,4) (* 7 *)
(* END TEST *)

(* Compléter *)
let rec sigma2 f (a,b) = if (a > b) then 0 else f b + sigma2 f (a, pred b)

(* TEST *)
let res = sigma2 (fun x -> 2 * x) (-2,4) (* 14 *)
(* END TEST *)

(* Compléter *)
let rec sigma3 f (a,b,i) (fc,acci) = if a > b then acci else sigma3 f (a + i,b,i) (fc, fc (f a) acci)  

(* TEST *)
let res = sigma3 (fun x -> 2 * x) (2,6,2) ((fun v acc -> v + acc),0) (* 24 *)
(* END TEST *)

(* TEST *)
let res = sigma3 (fun x -> x * x) (0,10,2) ((fun x acc -> x :: acc), []) (* [100; 64; 36; 16; 4; 0] *)
(* END TEST *)

(* Compléter *)
let rec sigma4 f (p,fi) (fc,acci) a = if p a  then acci else sigma4 f (p,fi) (fc, fc (f a) acci) (fi a)

(* TEST *)
let res = sigma4 (fun x -> 2 * x) ((fun v -> v > 6),(fun v -> v + 2)) ((fun v acc -> v + acc), 0) 2 (* 24 *)
(* END TEST *)

(* Compléter *)
let cum f (a,b,dx) (fc,vi) = sigma4 f  ((fun v -> v > b),(fun v -> v +. dx)) (fc,vi) a

(* TEST *)
let res = cum (fun x -> 2. *. x) (0.2,0.7,0.2) ((fun v acc -> v +. acc),0.) (* 2.4 *)
(* END TEST *)

(* Compléter *)
let integre f (a,b,dx) = cum f (a,b,dx)((fun v acc -> v *. dx +. acc ),0.)

(* TEST *)
let res = integre (fun x -> 1. /. x) (1., 2., 0.001) (* 0.693897243059956925 *)
(* END TEST *)

(* Compléter *)
let rec maxi f (a,b) p = if (b-.a) < p then f a else
                           if f a > f a+. (b -. a)/. 2.  then maxi f (a, a+. (b -. a)/. 2.) p
                           else maxi f (a+. (b-.a)/.2., b) p 

(* TEST *)
let res = maxi (fun x -> 1. -. x *. x) (0.,2.) 0.0001 (* 1. *)
               (* END TEST *)

