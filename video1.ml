(* As duas seguintes funções são iguais *)

let add (x:int) (y: int) : int =
  x + y

let add' x y =
  x + y

(* Se eu quiser definir tipos específicos faço *)

let addFloats (x:float) (y: float) : float =
  x +. y

(* Funções recursivas *)

let rec fib x = 
  if x = 0 then 0 else
    if x < 3 then 1 else fib(x-1)+fib(x-2)


(* Exercise 1 - Write a function named circle_area
it will take an argument 'r' (radius) and returns 
the area of the circle *)

let circle_arena (r: float) : float = 
  r *. r *. 3.14 

(* Mesma coisa *)

let circle_arena' r = 
  r *. r *. 3.14 


(* Exercise 2 - Write a function named power
that takes in a power 'n' and a float 'x' and returns x ^ n *)

let rec power (n: int) (x: float) : float = 
  if n = 0 then 1.0 else x *. power(n-1) x

(* Exercise 3 - Write a function named gcd
that computesthe greatest common divisor of two positive integers *)

let rec gcd (a: int) (b: int) : int= 
  if a = 0 then b else
    if b = 0 then a else gcd b (a mod b)