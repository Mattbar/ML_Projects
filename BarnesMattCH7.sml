(* 
Exercise 1
Give a BNF grammar for the language ML.

<pattern> ::= <singular> | <tuple>
<singluar> ::= <list> | <name> | <constant> | <cons>
<tuple> ::= ( <tuple-list> )
<tuple-list> ::= <pattern> , <tuple-list> | <pattern>
<list> ::= [ <singular> ] | []
<cons> ::= <name> :: <cons-list> | <constant> :: <cons-list>
<cons-list> <name> | <constant> | <cons> 
*)


(* Exercise 2
Define a function member of type ''a * ''a list -> bool so that member(e, L) is true if and only if e is an element of the list L. *)
fun member (i, []) = false
    | member (e, x::xs) = if e = x then true
      else member(e, xs);
member(2,[1,2,3,4]);
member(5,[1,2,3,4]);

(* Exercise 3
Define a function less of type int * int list -> int list so that less(e,L) is a list of all the integers in L that are less than e. *)

fun less (e, []) = []
    | less(e, x::xs) = if x < e then x :: less(e, xs)
      else less(e,xs);
less(5,[1,3,5,6,8]);
less(5,[5,7,9]);

(* Exercise 4
Define a function repeats of type ''a list -> bool so that
repeats (L) is true if and only if the list L has two equal
elements next to each other *)

fun repeats ([]) = false
    | repeats(x::nil) = false
    | repeats(x::xs) = if x = hd(xs) then true 
    else repeats(xs);

repeats([1,2,3,3,4,5]);
repeats([1,2,3,2,4,5]);

(* Exercise 5
Represent a polynomial using a list of its (real) coefficients, starting
with the constant coefficient and only going as high as necessary.  *)

fun pow (x, 0) = 1.0
    | pow (x, exp) = x * pow(x, exp - 1);

fun elms ([], a, count) = 0.0
  | elms (x::xs, a, count) = (x * pow(a, count)) + elms(xs, a, count + 1);

fun eval ([], r) = 0.0 
  | eval (x::xs, r) = x + elms(xs, r, 1);

eval([1.0,5.0,3.0],2.0);

(* Exercise 6
Write a quicksort function of type int list -> int list  *)

fun part(pivot, []) = (nil, [])
    | part(pivot, x::xs) = 
        let
            val (small, big) = part(pivot, xs) 
        in
            if x < pivot 
            then (x::small, big)
            else (small, x::big)
        end;

fun quickie ([]) = []
    | quickie[x] = [x]
    | quickie(x::xs) =
    let
      val (small, big) = part(x,xs)
    in
      quickie(small) @ [x] @ quickie(big)
    end;

quickie([1,4,2,7,11,8,5]);
(* 
Exercise 7
Make another version of your quicksort function, but this time of type 'a list * ('a * 'a -> bool) -> 'a list. The second parameters should be a function that performs the role of the < comparison in your original function.  *)

fun part_round_two(pivot, nil, compFun) = (nil, nil)
    | part_round_two(pivot, x::xs, compFun) = 
        let
            val (small, big) = part_round_two(pivot, xs, compFun) 
        in
            if compFun(x,pivot)
            then (x::small, big)
            else (small, x::big)
        end;

fun quickie_round_two([], compFun)  = []
    | quickie_round_two([x], compFun) = [x]
    | quickie_round_two(x::xs, compFun) =
    let
      val (small, big) = part_round_two(x,xs, compFun)
    in
      quickie_round_two(small, compFun) @ [x] @ quickie_round_two(big, compFun)
    end;

fun compFun(a,b) = a < b;

quickie_round_two([1,4,2,7,11,8,5], compFun);
