(* Excercise 1  write  fun il2rl of type int list -> real list*)
fun il2rl(x) = map real x;

val I2R = il2rl([1,2,3,4,5]);

(* Excercise 2 write fun ordlist of type char list -> int list *)
fun ordlist(x) = map ord x;

val Olist = ordlist([#"A",#"B",#"C"])

(* Excercise 3 write fun squarelist of type int list -> int list*)
fun squarelist(x) = map(fn y => y * y) x;

val Slist = squarelist([1,2,3,4]);

(* Excercise 4 fun multpairs of type (int *int ) list -> int list*)
fun multpairs(x) = map(fn (y,z) => y * z) x;

val Mpairs = multpairs([(1,2),(3,4)]);

(* Excercise 5 fun inclist of type int list -> int -> int list *)
fun inclist(L, m) = map(fn x => x + m)L

val Ilist = inclist([1,2,3,4], 10);

(* Excercise 6 fun sqsum of type int list -> int  *)
fun sqsum(x) = foldr(op +) 0 (map(fn x => x*x)x);

val SQsum = sqsum([1,2,3,4]);

(* Excercise 7 fun bor of type bool list -> bool *)
fun bor(x) = 
    let 
        fun OE (x,y) = x orelse y
    in
        foldr(op OE) false x
    end;

val Ebor = bor([]);
val Fbor = bor([false, false, true,true,false]); 

(* Excercise 8  fun band of type bool list -> bool*)
fun band x = 
    let
        fun AA (a,b) = a andalso b
    in
        foldr(op AA) true x
    end;

val Eband = band([]);
val Fband = band([false, false, true,true,false]);

(* Excercise 9 fun bxor of type bool list -> bool*)
fun bxor(x) =
    let
        fun xor (x,y) = (x orelse y) andalso not(x andalso y)
    in
        foldr(op xor) false x
    end;

val Ebxor = bxor([]);
val Tbxor = bxor([false,true,true,true,false]);
val Fbxor = bxor([false, true, false, true]);

(* Excercise 10 dupList of type a list -> a list *)

fun dupList([]) = [] 
    | dupList(x) = foldr(fn (a,b)=> a::a::b)[]x;

val Dlist = dupList([1,2,3]);

(* Excercise 11 fun myLength of type a list -> int *)
fun myLength([]) = 0
    | myLength(x) = 1 + myLength(tl x);
fun betterMylength(L) = foldl(fn(_,y)=> y+1)0 L;

val Len = myLength([1,2,3,4,5]);
val Blen = betterMylength([1,2,3,4,5]);
val emptLen = myLength([]);

(* Excercise 12 fun il2absrl of type int list -> real list*)
fun il2absrl([]) = []
    | il2absrl(x) = map(fn y => abs(real y))x;   
     
val IL = il2absrl([~1,2,4,~5]);
(* Excercise 13 fun truecount of type bool list -> int *)
fun truecount([]) = 0
    | truecount(x :: xs) = if x = true then 1 + truecount(xs) 
      else truecount(xs);

val TC = truecount([true,true,false,true]);
val NOTRUE = truecount([false, false, false]);

(* Excercise 14 maxpairs of type (int * int)list -> int list*)
fun maxpairs(x) = map(fn (y,z) => if y > z then y
      else z )x;

val MP = maxpairs([(1,3),(4,2),(~3,~4)]);
(* Excercise 15 fun myimplode of type char list -> string*)
fun myimplode (L) = foldr (op ^) "" (map str L);

val Mplod = myimplode([#"I", #"a", #"t", #"b", #"a", #"L"]);
(* Excercise 16 fun iconcat of type `a list list -> `a list*)
fun iconcat([]) = []
    | iconcat(x ::xs) = x@iconcat(xs);

val ICON = iconcat([[1,2],[3,4,5,6],[7]]);
(* Excercise 17  fun max of type int list -> int*)
fun max (x)= foldl(fn(a,b) => if a < b then b else a)(hd x) (tl x);
    (* map(fn y => if y > x then y::x)x; *)

val MX = max([1,5,3,9,3]);
(* Excercise 18 *)
fun min (x) = foldl(fn(a,b) => if a < b then a else b)(hd x)(tl x);

val MN = min([5,3,9,3]); *)
(* Excercise 19 fun member of type a * a list -> bool*)
(* fun member(e,L) =
    let
        val TFL = map(fn y => if y = e then true else false)L;
    in
        bor(TFL)
    end; *)

fun member(e, L) = foldr(fn(a,b) => b orelse a=e)false L;

val MEM = member(3,[1,4,5,3,7]);
(* Excercise 20 fun append of type a list -> a list -> a list*)

fun append (L1, L2) = foldr (op ::) L2 L1 ;

val L3 = append([1,2,3,4],[5,6,7,8]);

(* fun il2rk x = map(real)x; = fn int list -> real list
    fun ordList x = map ord x; = fn char list -> int list
    fun squarList x = map(fn a=> a*a)x; = fn int list -> int list
    fun multipairs L = map(fn(a,b) => a*b)L = fn (int * int) list -> int list
    fun inclist L x = map(fn a => a+x)L = fn int list * int -> int list
    fun g x a = 2 + a = 
 *)

