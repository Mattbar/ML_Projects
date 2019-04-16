(* Exercise 1 *)
fun cube(i : int) =
    i * i * i

val numberCubed = cube(5)

(* Exercise 2 *)
fun cuber(r : real) =
    r * r * r
   
val realNumberCubed = 
    cuber(0.0)

(* Exercise 3 *)

fun fourth [] = 0 
    | fourth(a::b::c::d::rest) = d

val f = fourth([1,2,3,4,5,6,7,8])

(* Exercise 4 *)
fun min3(a,b,c) =
    if a <= b andalso a <= c then a 
    else 
    if b <= a andalso b <= c then b 
    else c

val m = min3(5,23,3)

(* Exercise 5 *)
fun red3(a,b,c) = 
    (a,c)

val r = red3(1,2,4)

(* Exercise 6 *)
fun third [] = #"-"
    | third (a::b::c::d) = c

fun thirds string = third(explode string)

val t = thirds("abcs")

(* Exercise 7 *)
fun cycle1 [] = []
    | cycle1(head::rest) = rest@[head]

val c = cycle1([1,2,3,4])

(* Exercise 8 *)
fun sort3(a,b,c :real) =
    if a <= b andalso a<=c andalso b<=c then (a,b,c) 
    else
    if a <= b andalso a<=c andalso c<=b then (a,c,b) 
    else
    if b <= a andalso b<=c andalso a<=c then (b,a,c) 
    else
    if b <= a andalso b<=c andalso c<=a then (b,c,a) 
    else
    if c <= a andalso c<=b andalso b<=a then (c,b,a) 
    else
    (c,a,b)

val s = sort3(22.4,75.3,50.2)

(* Exercise 9 *)
fun del3(a::b::c::rest) = [a,b]@rest

val d = del3([1,2,3,4,5,6,7,8])

(* Exercise 10 *)
fun sqsum(number) =
    if number = 0 then 0
    else
    number * number + sqsum(number -1)

val s = sqsum(5)

(* Exercise 11 *)
fun cycle ([], a) = []
    | cycle(first, a) = if a <= 0 then first
    else cycle(cycle1(first), a-1)

val c = cycle([1,2,3,4,5,6],3)

(* Exercise 12 *)
fun pow(a:real, b:int) =
    if b <= 0 then 1.0
    else
    a*(pow(a,b-1))

val p = pow(5.0,5)

(* Exercise 13 *)
fun maxhelper([], n) = n
    | maxhelper((head::tail), n) = 
        if head > n then maxhelper(tail,head)
        else maxhelper(tail, n)

fun max x = maxhelper(tl x, hd x)

val maxxx = max([1,2,3,4,5,6,55,7,8,9])
