// Fernando Acosta
// Camilo Rivera

// This sets F# to read from whatever directory contains this source file.
System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__;;

#load "parser.fsx"

// This lets us refer to "Parser.Parse.parsefile" simply as "parsefile",
// and to constructors like "Parser.Parse.APP" simply as "APP".
open Parser.Parse

let rec subst e x t =
  match e with
  | NUM n -> NUM n
  | BOOL b -> BOOL b
  | ID n -> if n = x then t
                     else ID n
  | SUCC -> SUCC
  | PRED -> PRED
  | ISZERO -> ISZERO
  | IF (b, e1, e2) -> IF (subst b x t, subst e1 x t, subst e2 x t)
  | APP (e1, e2) -> APP (subst e1 x t, subst e2 x t)
  | FUN (x1, e) -> if x = x1 then FUN (x1, e)
                             else FUN (x1, subst e x t)
  | REC (x1, e) -> if x = x1 then REC (x1, e)
                             else REC (x1, subst e x t)
  | _ -> ERROR (sprintf "Cannot do substitution")
  
// Here I show you a little bit of the implementation of interp. Note how ERRORs
// are propagated, how rule (6) is implemented, and how stuck evaluations
// are reported using F#'s sprintf function to create good error messages.
let rec interp = function
| ERROR s -> ERROR s
| NUM n -> NUM n // Rule (1)
| BOOL b -> BOOL b // Rule (2)
| ID x -> ID x
| SUCC -> SUCC // Rule (3)
| PRED -> PRED // Rule (3)
| ISZERO -> ISZERO // Rule (3)
| FUN (x, e) -> FUN (x, e) // Rule (9)
| REC (x, e) -> REC (x, e)
| IF (b, e1, e2) ->
    match (interp b, e1, e2) with
    | (ERROR s, _, _) -> ERROR s
    | (_, ERROR s, _) -> ERROR s
    | (_, _, ERROR s) -> ERROR s
    | (BOOL b, e1, e2) ->
        match b with
        | true -> interp e1 // Rule (4)
        | false -> interp e2 // Rule (5)
    | (b, e1, e2) -> ERROR (sprintf "'if' needs boolean expression, not '%A'" b)
| APP (e1, e2) ->
    match (interp e1, interp e2) with
    | (ERROR s, _) -> ERROR s // ERRORs are propagated
    | (_, ERROR s) -> ERROR s
    | (SUCC, NUM n) -> NUM (n + 1) // Rule (6)
    | (SUCC, v) -> ERROR (sprintf "'succ' needs int argument, not '%A'" v)
    | (PRED, NUM n) -> // Rule (7)
        match n with
        | 0 -> NUM 0
        | n -> NUM (n - 1)
    | (PRED, v) -> ERROR (sprintf "'pred' needs int argument, not '%A'" v)
    | (ISZERO, NUM n) -> // Rule (8)
        match n with
        | 0 -> BOOL true
        | n -> BOOL false
| (ISZERO, v) -> ERROR (sprintf "'iszero' needs int argument, not '%A'" v)
| (FUN (x, e), t) -> interp (subst e x t) // Rule (10)
| (REC (x, e), t) -> interp (APP (subst e x (REC (x, e)), t)) // Rule (11)
| (h1, h2)        -> APP (h1, h2)

// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp
