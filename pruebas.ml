(*************************************)
(*** PROBABILITIES                 ***)
(*************************************)


type 'a st = S of 'a

type _ prob =
| B :  'a -> 'a prob
  | C :  'a * 'b * 'c -> ('a * 'b * 'c) prob


type _ pchoice =
  | Diff :  'a st * 'b st -> ('a st * 'b st * 'c) pchoice
  | Same :  'a st * 'a st -> ('a st * 'a st * 'b) pchoice
 

let rec left : type a b c. (a st * b st * c) pchoice -> a st = function
  | Diff (x, _) -> x
  | Same (x, _) -> x

let rec right : type a b c. (a st * b st * c) pchoice -> b st = function 
  | Diff (_, x) -> x
  | Same (_, x) -> x


let pick f g x =
  if Random.bool() then
   f (left x)
  else
   g (right x)

let f x =
  match x with
  | S y -> print_int y

let h x =
  match x with
  | S y -> print_int (y+1)


let g x =
  match x with
  | S y -> print_string y
