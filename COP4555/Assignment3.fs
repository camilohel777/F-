// Fernando Acosta
// Camilo Rivera

// Problem 1
let rec inner xs = function
| [] -> 0
| y::ys -> List.head xs * y + inner (List.tail xs) ys

// Problem 2
//Original transpose in midterm sample 1 solution by Geoffrey Smith
let rec transpose = function
| [] -> failwith "Cannot transpose a 0-by-n matrix"
| []::xs -> []
| xs -> List.map List.head xs::transpose(List.map List.tail xs)

let multiply (mA, mB)=
  let rec innerMult xs ys =
    match xs with
    | [] -> []
    | x::xs -> List.map (fun f -> inner f x) ys :: innerMult xs ys
innerMult mA (transpose mB)


// Problem 3
let flatten1 xs = List.fold (@) [] xs
let flatten2 xs = List.foldBack (@) xs []

(*
List.fold (@) [] [[1;2];[];[3]]
=> List.fold (@) ([] @ [1;2]) [[];[3]]
=> List.fold (@) (([] @ [1;2]) @ [])) [[3]]
=> List.fold (@) ((([] @ [1;2]) @ []) @ [3]) []
=> ((([] @ [1;2]) @ []) @ [3])
=> [1;2;3]

List.foldBack (@) [[1;2];[];[3]] []
=> [1;2] @ (List.foldBack (@) [[];[3]] [])
=> [1;2] @ ([] @ (List.foldBack (@) [[3]] []) )
=> [1;2] @ ([] @ ([3] @ (List.foldBack (@) [] []) ))
=> [1;2] @ ([] @ ([3] @ [])
=> [1;2] @ ([] @ ([3]) )
=> [1;2] @ ([3])
=> [1;2;3]

When a larger list is in the front of the list flatten1 is slower than flatten2
because it uses the larger list to append to every other list. When the larger list
is in the back of flatten2 then it will be slower than flatten1 since it will
then use the larger list to append to every other list.

Worst case for both is O(n^2)

To find out this we used the following example:

When the larger list is in the front:

flatten1 [[1 .. 500000];[];[3];[4;5;6]];;
Real: 00:00:00.151, CPU: 00:00:00.171, GC gen0: 5, gen1: 4, gen2: 1

flatten2 [[1 .. 500000];[];[3];[4;5;6]];;
Real: 00:00:00.111, CPU: 00:00:00.109, GC gen0: 3, gen1: 3, gen2: 0

And then when the larger list is in the back:

flatten1 [[1;3];[];[3];[4;5;6];[1 .. 500000]];;
Real: 00:00:00.058, CPU: 00:00:00.062, GC gen0: 1, gen1: 1, gen2: 0

flatten2 [[1;3];[];[3];[4;5;6];[1 .. 500000]];;
Real: 00:00:00.076, CPU: 00:00:00.109, GC gen0: 2, gen1: 2, gen2: 1
*)


// Problem 4
let twice f = (fun x -> f (f x))
let successor n = n+1;;

(*
By approaching this problem experimentally, the answer to
(twice (twice (twice (twice successor)))) 0
Is equal to 16, where the formula is 2^k.
And when you evaluate the expression:
twice twice twice twice successor 0
The result is 65536, which with a simple experimentation we figured out
that 65536 is equal to 2^(2^(2^2)). That is the same amount of twices in
the input.
As we recalled from previous courses (Theory of Algorithms specifically),
we knew that the formula for this type of problem is Tetration.
Which is "n copies of a combined by exponentiation, right-to-left".
Source: https://en.wikipedia.org/wiki/Tetration
The formula is:
^k(2), where k is the number of twices. k is the Tetration
Which is = 2^(2^..(^2)). And the number of 2's equals the amount of
twices.
*)


// Problem 5
let rec map f (Cons(x,xsf)) = Cons(f x, fun() -> map f (xsf()));;

// How to use it:
val map : f:('a -> 'b) -> 'a stream -> 'b stream

> let rec upfrom n = Cons(n, fun () -> upfrom(n+1));;
val upfrom : n:int -> int stream

> let rec take n (Cons(x, xsf)) =
- if n = 0 then []
- else x :: take (n-1) (xsf());;
val take : n:int -> 'a stream -> 'a list

> let nats = upfrom 0;;
val nats : int stream = Cons (0,<fun:upfrom@37>)

> map (fun x -> x*5) nats;;
val it : int stream = Cons (0,<fun:map@36>)

> take 5 it;;
val it : int list = [0; 5; 10; 15; 20]


// Problem 6
let rec evaluate = function
  | Num n -> Some n
  | Neg e -> match evaluate e with
              | Some n -> Some (-n)
              | _ -> None
  | Sum (a,b) -> match (evaluate a, evaluate b) with
                  | Some n, Some m -> Some (n+m)
                  | _ -> None
  | Diff (a,b) -> match (evaluate a, evaluate b) with
                  | Some n, Some m -> Some (n-m)
                  | _ -> None
  | Prod(a, b) -> match (evaluate a, evaluate b) with
                  | Some n, Some m -> Some (n*m)
                  | _ -> None
  | Quot (a, b) -> match (evaluate a, evaluate b) with
                  | Some n, Some 0 -> None
                  | Some n, Some m -> Some (n/m)
                  | _ -> None;;
