(*Building a list:
[] is the empty list
:: "cons" operator - assigns two elements together
    2::[] will return a int list = [3]
    3::[2;3;4] will return an int list = [3,2,3,4]
[e1;e2;e3] - directly makes a list
*)


(* Ex1: Write an OCaml function that returns true if 
  all elements of the list are true *)
let rec all (lst: bool list) : bool =
  match lst with
  | [] -> true
  | x::rest -> x && all rest


(* Ex2: Write a function even2ways that checks if an integer
list only contains even values and has an even number
of elements
*)
let even x = x mod 2 = 0

let rec even2ways (lst: int list) : bool = 
  match lst with
  | [] -> true
  | x::[] -> false (*only one element*)
  | x1::x2::rest -> even x1 && even x2 && even2ways rest

(* Example using _ 
_ essentially means that we don't care what the value
is inside of the list
We use this when we match with element, but we don't
do anything with the knowledge of the value that
is inside of that element *)



(*Ex3: is_empty: Write a function that returns true if
  the list is empty, and false otherwise *)
let is_empty (lst: 'a list) : bool =
  match lst with
  | [] -> true
  | _::_ -> false


(* Ex4: Write an OCaml function named head that returns
the front element of the list *)

let head (lst: 'a list) : 'a = 
  match lst with
  | x::_ -> x
  | [] -> raise (Invalid_argument "head")


(* Exercise1 : Write a function that adds up all the numbers
in an integer list *)

let rec add (lst: int list) : int = 
  match lst with
  | [] -> 0
  | x::rest -> x + add rest

(* Exercise2: Write a function that finds the smallest element
in the list. *)

let rec smallest (lst: int list) : int = 
  match lst with
  | [] -> raise (Invalid_argument "minimum")
  | x::[] -> x 
  | x::rest -> if x < smallest rest then x else smallest rest

(* Exercise3: Write a function that appends two lists into one *)
let rec append (lst1: 'a list) (lst2: 'a list): 'a list = 
  match lst1 with
  | [] -> lst2
  | x::[] -> x :: lst2 
  | x::rest -> x :: append rest lst2

(* Exercise4: Write a function that determines if a value is
in a list or not *)

let rec isIn (lst: 'a list) (value: int): bool = 
  match lst with
  | [] -> false
  | x::rest -> if x != value then isIn rest value else true