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
	  | sub::subs =>
	 		let val tl_ans = get_substitutions1(subs, s)
	 		in case all_except_option(s, sub) of
		  		  NONE => tl_ans
		  	    | SOME (lst) => lst @ tl_ans
	 		end
