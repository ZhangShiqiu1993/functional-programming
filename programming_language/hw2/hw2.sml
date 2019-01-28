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
