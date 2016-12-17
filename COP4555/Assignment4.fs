//By Camilo Rivera and Fernando Acosta
//Problem 1

(*

The function makeMonitoredFun accepts a function which accepts a value of some type
and returns a value of the same type. It does not allow for generic types which is
why the above does not work. List.rev may work with a list of any type and is consistent
with the types.
let msqrt = makeMonitoredFun sqrt;;
This works and is consistent because it will always deal with floats and no other type.
let mrev = makeMonitoredFun List.rev
mrev is bound to a curried function call, which will generate a Value Restriction error.
F# does not give mrev a polymorphic type and List.rev is not a syntactic value. F# does not
know what type mrev should be because the list can be of any type.
let mrev = fun x -> (makeMonitoredFun List.rev) x
mrev now has a polymorphiic type so it is now bound to a function expresssion which is syntactic.
This problem is solved BUT we now encounter a new problem. With this new definition for mrev
we no longer get the accumulated number of times that the function is called because of the
eta expansion. The function fun x -> (makeMonitoredFun List.rev) x is now being evaluated every
in every function call. It will count call of mrev as a function evaluation so it will not
keep track.
*)

//Problem 2
(*
  E -> E+T | E-T | T
  T -> T*X | T/X | X
  X -> F^X | F
  F -> i | (E)
*)

//Problem 3
(*
Two different parses with the same expression creates ambiguity:

"if a then if b then begin print b end else begin print c end"

Tree #1:
When the else belongs to the first "if" c is print if a and not b
if a then
/ \
if b then else begin
/ \
begin print b print c
/ \
end end

Tree #2:
When the else belongs to the second "if" c is printed if not a
if a then
|
if b then
/ \
begin print b else begin print c
/ \
end end
*)


//Problem 4 code used from lecture "Notes on Programming Language Syntax" by Geoffrey Smith
// lookahead token

  int tok = nextToken();
  void advance() {tok = nextToken();}

// used whenever a specific token t must appear next
  void eat(int t) {if (tok == t) advance(); else error("Invalid Syntax");}
  void S() {
    switch (tok) {
      case IF: advance(); E(); eat(THEN); S();
        if(tok contains ELSE)
          eat(ELSE); S();
        break;
      case BEGIN: advance(); S(); L(); break;
      case PRINT: advance(); E(); break;
      default: error("Invalid token");
    }
  }

  void L() {
    switch (tok) {
      case END: advance(); break;
      case SEMICOLON: advance(); S(); L(); break;
      default: error("Invalid token inside BEGIN");
    }
  }
  void E() {
    eat(ID);
  }
  void main() {
    S();
    if (tok == EOF) accept(); else error("No EOF read");
  }
