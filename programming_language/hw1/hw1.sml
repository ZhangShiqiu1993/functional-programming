(*1*)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
	let
		val compare_year = #1 date1 = #1 date2
		val compare_month = #2 date1 = #2 date2
	in
		if not compare_year
		then #1 date1 < #1 date2
		else if not compare_month
		then #2 date1 < #2 date2
		else #3 date1 < #3 date2
	end

(*2*)
fun number_in_month (dates_list : (int * int * int) list, month: int) =
	let 
		fun in_month(date : int * int * int) =
			if #2 date = month
			then 1
			else 0
	in
		if null dates_list
		then 0
		else in_month(hd dates_list) + number_in_month(tl dates_list, month)
	end
	