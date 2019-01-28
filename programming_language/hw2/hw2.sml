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

(*2 c*)
fun remove_card (card_list, c, e) =
	case card_list of
		[] => raise e
	  | head::rest => if head = c then rest else head::remove_card(rest, c, e)

(*2 d*)
fun all_same_color card_list = 
	case card_list of
		[] => true
	  | [_] => true
	  | head::neck::rest => card_color(head) = card_color(neck) andalso all_same_color(neck::rest)

(*2 e*)
fun sum_cards card_list = 
	let fun sum(cards, ans) =
			case cards of
				[] => ans
			  | head::rest => sum(rest, ans + card_value(head))
	in
		sum(card_list, 0)
	end

(*2 f*)
fun score (held_cards, goal) =
	let
		val sum = sum_cards(held_cards)
		val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
	in
		if all_same_color(held_cards) then preliminary_score div 2 else preliminary_score
	end

(*2 g*)
fun officiate (card_list, does, goal)=
	let
		fun run(held_cards, does, card_list) =
			case does of
				[] => score (held_cards, goal)
			  | Discard c::rest_does => run(remove_card(held_cards, c, IllegalMove), rest_does, card_list)
			  | Draw::rest_does =>
		      		case card_list of
		      			[] => score (held_cards, goal)
		      		  | head_card::rest_cards =>
		      		  		let val held_more_card = head_card::held_cards
		      		  		in
		      		  			if sum_cards(held_more_card) > goal 
		      		  			then score (held_more_card, goal) 
		      		  			else run(held_more_card, rest_does, rest_cards)
		      		  		end
	in
		run([], does, card_list)
	end
