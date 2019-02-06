1*)
val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)))

(*2*)
fun longest_string1 strs = foldl (fn (x, y) => if String.size x > String.size y then x else y) "" strs

(*3*)
fun longest_string2 strs = foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" strs

(*4*)
fun longest_string_helper f =
	foldl (fn (x, y) => if f (String.size x, String.size y) then x else y) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(*5*)
val longest_capitalized = longest_string1 o only_capitals

(*6*)
val rev_string = String.implode o rev o String.explode

(*7*)
exception NoAnswer

fun first_answer f xs =
	case xs of
		[] => raise NoAnswer
	  | x::rest => case f x of
					  NONE => first_answer f rest
	  				| SOME y => y

(*8*)
fun all_answers f xs =
	let fun acc (xs, ans) =
		case xs of
			[] => SOME ans
		  | x::rest => case f x of
		  				   NONE => NONE
		  				 | SOME y => acc (rest, y @ ans)
	in
		acc(xs, [])
	end
