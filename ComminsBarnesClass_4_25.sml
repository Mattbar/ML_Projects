(* write a function to test whether an element is a member of a set *)
fun member(nil, i) = false
    | member(x::xs, i) = 
        if i = x then true
        else member(xs, i);

val M1 = member([1,2,3,4,5], 3);
val M2 = member([1,2,3,4,5], 7);
val M3 = member([], 7);
(* wtire a function to constuct the unuin of two sets *)
fun union(nil,nil) = nil
    | union(nil, l2) = l2
    | union(l1, nil) = l1
    | union (l1, x::l2) = 
        if member(l1, x) then union(l1, l2)
        else  [x]@union(l1, l2);

val U1 = union(nil,nil);
val U2 = union([1],nil);
val U3 = union(nil,[1]);
val U4 = union([1,2,3],[2,3,4]);
(* write a fintion to construct the intersection of two sets *)

fun intersection(nil, nil) = nil
    | intersection(nil, L2) = nil
    | intersection(L1, nil) = nil
    | intersection(L1, x::L2) =
        if member(L1, x) then [x]@intersection(L1, L2) 
        else intersection(L1, L2);

val I1 = intersection(nil, nil);
val I2 = intersection([1], nil);
val I3 = intersection(nil, [1]);
val I4 = intersection([1,3,5], [2,4,6]);
val I5 = intersection([1,2,3,4], [2,4,6]);

(* add 1 to every elm in a list *)

fun add1(nil) = nil
    | add1(L) = map(fn x=> x+1) L;

val A1 = add1([1,2,3,4]);

fun TF(nil) = nil
    |TF(L) = map(fn x => if x mod 2 = 0 then true else false) L;

val F1 = TF([1,2,3,4]);
