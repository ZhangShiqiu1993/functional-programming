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
