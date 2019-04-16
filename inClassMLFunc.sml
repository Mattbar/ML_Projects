(* write a function  that subtracts the resl #'s *)
    fun sub (a,b :real) = a-b
    val s = sub(3.0, 2.0)
(* Write a function that computes the negative of a real value *)
    fun neg a:real =
        ~a
    val n = neg(5.0)
(* Write a function that computes the sum of the elements of of a lit of ints  *)

    fun sum(list) =
        if null(list) then 0 
        else
        (hd list) + sum(tl list)

    val s = sum([1,2,4])
(* Write a function that computes the length of a list *)
    fun listLength(list) =
        if null(list) then 0
        else
        1 + listLength(tl list)

    val l = listLength([1,2,3,4])
(* Write a function that reverses the order of the elements in a list *)
    fun rev [] = []
        | rev(list) = rev(tl list) @ [hd list]
   
   val r = rev[1,2,3,4]

