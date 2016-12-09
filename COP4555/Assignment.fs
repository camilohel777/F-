//Camilo Rivera
//Fernando Acosta
//Question 1
let rec cartesian (xs ,ys) =
  match xs,ys with
  |(_,[]) -> []
  |([],_) -> []
  |( x::xs , ys )-> (List.map(fun y -> x,y) ys) @ cartesian (xs,ys)
  
//Question 2
//We used a defined function called List.collect defined in
https://msdn.microsoft.com/en-us/library/ee370406.aspx
let rec powerset =
  function
  | [] -> [[]]
  | x::xs -> List.collect (fun subSet -> [subSet; x::subSet]) (powerset xs)
  
//Question 3
let rec transpose M =
  match M with
  | x::xs ->
    match x with  //List.map List.head M is the first list of M which is going to be applied a transpose of the tail of M to each element
    | y::ys -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []
  | _ -> []
  
//Question 4
let rec sort = function
  | [] -> []
  | [x] -> [x]
  | x1::x2::xs -> if x1 <= x2 then x1 :: sort (x2::xs)
                              else x2 :: sort (x1::xs)
                              
(* Step 1:The first base case in this recursive function covers one of the base case
   problems which is when the list is empty. It returns an empty list.
  The second base case is also correct because it covers the possibility
  when the list consists of only one element. The sorted list is just the
  list itself with the single element.
  
  Step 2:The non-base case fails when there is a list and length is greater than 2
  because it only compares two elements at a time so it ignores a possibly
  larger element which maybe two or more spaces farther back.
  
  Step 3:There is no circumstance because the list gets smaller by 1 than the input
  every
  time the recursive call is made until the base case is met.
*)

//Question 5
let rec merge = function
  | ([], ys) -> ys
  | (xs, []) -> xs
  | (x::xs, y::ys) -> if x < y then x :: merge (xs, y::ys)
                                else y :: merge (x::xs, ys)
                                
let rec split = function
  | [] -> ([], [])
  | [a] -> ([a], [])
  | a::b::cs -> let (M,N) = split cs
                (a::M, b::N)
                
let rec mergesort = function
  | [] -> []
  | [x] -> [x] //3.This is the needed case to fix the bug
  | L -> let (M, N) = split L
merge (mergesort M, mergesort N)

(* 1.Analyze the code
    Step 1:The base case covers the circumstance when the list is empty which is correct.
    
    Step 2:Under the assumption that split and merge are working correctly,
    the non-base case call should perform correctly.
    
    Step 3:The input always gets smaller as the recursive call is made. The list
    is split in half every time the recursive call is made.
    
    2. The bug is that "val mergesort : _arg1:'a list -> 'b list when 'b : comparison" is
    trying to output a different list when there is only one value in the list and nothing to split it into. 
    This creates an error which requires a second base case for merge sort in which the list consists only one element.
*)

//Question 6
let curry f a b = f(a,b)
//The type is ('a * 'b -> 'c) -> a:'a -> b:'b -> 'c

let uncurry f (a,b) = f a b
//The type is ('a -> 'b -> 'c) -> a:'a * b:'b -> 'c
