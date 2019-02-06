(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(*1*)
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

(*9*)
val count_wildcards = g (fn _ => 1) (fn _ => 0)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size
fun count_some_var (x, p) = g (fn _ => 0) (fn s => if s = x then 1 else 0) p

(*10*)
val check_pat =
	let
		fun get_variables p =
			case p of
				Variable s => [s]
			  | TupleP ps => foldl (fn (p, ans) => get_variables(p) @ ans) [] ps
			  | ConstructorP (_, p) => get_variables p
			  | _ => []
		fun distinct ps =
			case ps of
				[] => true
			  | head::rest => distinct(rest) andalso (not (List.exists (fn x => head = x) rest))
	in
		distinct o get_variables
	end

(*11*)
fun match (v, p) =
	case (v, p) of
		(_, Wildcard)       => SOME []
	  | (_, Variable(s))    => SOME [(s, v)]
	  | (Unit, UnitP)       => SOME []
	  | (Const x, ConstP y) => if x = y then SOME [] else NONE
	  | (Tuple vs, TupleP ps) => if length vs = length ps then all_answers match (ListPair.zip(vs, ps)) else NONE
	  | (Constructor (s2, v), ConstructorP (s1, p)) => if s1 = s2 then match(v, p) else NONE
	  | _                   => NONE

(*12*)
fun first_match v ps =
	SOME (first_answer (fn p => match(v, p)) ps)
	handle NoAnswer => NONE