fun increasing([]) = []
    | increasing(x::xs) = foldr(fn(y,z) => if (hd xs) < x then y::z else (hd xs) :: z)[] xs;

val I = increasing([2,1,4,3,2,7,6,5,9]);
