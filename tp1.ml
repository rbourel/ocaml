let mul2 x = 2 * x

let vabs x = if(x < 0) then  -x else x 

let test1 x = if(x>11 && x<29) then true else false

let test2 x = if(x==2 || x==5 || x==9 || x==53) then true else false

let test3 (x,y) = if(x == 12) then true else false

let bissext x = if(x mod 4 == 0 && (x mod 100 != 0 || x mod 400 == 0)) then true else false
let proj1 (x,y,z) = x

let proj23 (x,y,z) = (y,z)

let incrpaire (x,y) = (x+1,y+1)

let appliquepaire f (g, d) = (f g, f d)

let incrpaire2 (a,b) =  appliquepaire (fun x ->x+1) (a,b)

let rapport (f,g)(x) = (f x)/.(g x)

let mytan a = rapport (sin,cos) a

let premier n =
  let rec aux i =
    if (i=n) then true
    else if n mod i = 0 then  false
    else aux (i+1)
  in
  if n =1 then false else aux 2

                            
