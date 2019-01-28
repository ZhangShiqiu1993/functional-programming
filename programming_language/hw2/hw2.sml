(*question 1*)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*1 a*)
fun all_except_option(str, strs) =
	case strs of
		[] => NONE
	  | s::strs' => 
	  		if same_string(str, s) 
  			then SOME (strs')
  			else case all_except_option(str, strs') of
  					NONE => NONE
  				  | SOME (lst) => SOME (s::lst)

(*1 b*)
fun get_substitutions1(substitutions, s) =
	case substitutions of
		[] => []
	  | sub::subs => case all_except_option(s, sub) of
			  		   NONE => get_substitutions1(subs, s)
			  	     | SOME (lst) => lst @ get_substitutions1(subs, s)
	 		

(*1 c*)
fun get_substitutions2(substitutions, s) =
	let
		fun aux(substitutions, ans) =
			case substitutions of
				  [] => ans
				| sub::subs => case all_except_option(s, sub) of
								 NONE => aux(subs, ans)
							   | SOME (lst) => aux(subs, ans @ lst)
	in
		aux(substitutions, [])
	end


(*1 d*)
fun similar_names(substitutions, {first=f, middle=m, last=l}) =
	let
		fun similar_name(substitutions) =
			case substitutions of
				[] => []
			  | sub :: subs => {first=sub, middle=m, last=l} :: similar_name(subs)
	in
		{first=f, middle=m, last=l} :: similar_name(get_substitutions2(substitutions, f))
	end

(*question 2*)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(*2 a*)
fun card_color c =
	case c of
		(Clubs, _) => Black
	  | (Spades, _) => Black
	  | _ => Red

(*2 b*)
fun card_value c =
	case c of
		(_, Num i) => i
	  | (_, Ace) => 11
	  | _ => 10
