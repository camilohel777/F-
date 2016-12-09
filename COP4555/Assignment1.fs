// Team Members: Camilo Rivera
// Fernando Acosta
// Question 1
let rec gcd = function
| (a,0) -> a
| (a,b) -> gcd (b, a % b)

let simplify (a,b) =
    let x = gcd (a,b)
    (a/x, b/x)
    
let (.+) (a,b) (c,d) = simplify (a*d + c*b, b*d)

let (.*) (a,b) (c,d) = simplify (a*c, b*d)

// Question 2
let revlists xs = List.map List.rev xs;;

// Question 3
let rec interleave (xs, ys) =
    match (xs, ys) with
    | (xs,[]) -> xs
    | ([],ys) -> ys
    | (x::xs, y::ys) -> x::y::interleave (xs, ys)
    
// Question 4
let rec gencut (n, xs) =
    match (n, xs) with
    | (0, xs) -> ([], xs)
    | (n, []) -> ([], [])
    | (n, x::xs) ->
        let gentemp = gencut(n-1, xs)
        (x::fst gentemp, snd gentemp)
        
let cut xs = gencut ((List.length xs)/2, xs)

// Question 5
let shuffle xs = interleave (cut (xs))

// Question 6
let rec countaux (deck, target, n) =
    let deck = shuffle deck
    if deck = target then n else countaux(deck, target, n+1)
    
let countshuffles n =
    let xs = [1..n]
    countaux(xs, xs, 1)
