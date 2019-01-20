(* This is a comment *)
val z = 34; (* int *)

val abs_of_z = if z < 0 then 0 - z else z;

val abs_of_z_simpler = abs z;

val a = 6 div 3

val b = 6.0 / 3.0

fun pow(x : int, y : int) =
	if y = 0 
	then 1 
	else x * pow(x, y - 1)

fun cube(x : int) = 
	pow(x, 3)

fun swap (pr: int * bool) = 
	(#2 pr, #1 pr)

fun sum_two_pairs (pr1 : int * int, pr2 : int * int) =
	(#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

fun div_mod (x : int, y : int) = 
	(x div y, x mod y)

fun sort_pair(pr: int * int) = 
	if (#1 pr) < (#2 pr) 
	then pr 
	else (#2 pr, #1 pr)

val l = [1,2,3]

(*cons*)
val l0 = 0::l

(*null l *)
(*hd l*)
(*tl l*)
(*t l*)

fun sum_list(xs : int list) =
	if null xs
	then 0
	else hd xs + sum_list(tl xs)

fun countdown (x : int) =
	if x = 0
	then []
	else x :: countdown(x-1)

fun append (xs : int list, ys : int list) =
	if null xs
	then ys
	else (hd xs) :: append((tl xs), ys)

fun sum_pair_list (xs : (int * int) list) =
	if null xs
	then 0
	else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)

