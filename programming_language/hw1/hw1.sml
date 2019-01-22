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

(*3*)
fun number_in_months (dates_list: (int * int * int) list, months: int list) =
	if null months
	then 0
	else number_in_month(dates_list, hd months) + number_in_months(dates_list, tl months)

(*4*)
fun dates_in_month (dates_list: (int * int * int) list, month: int) = 
	if null dates_list
	then [] 
	else 
		let
			val tl_ans = dates_in_month (tl dates_list, month)
		in
			if #2 (hd dates_list) = month
			then hd dates_list :: tl_ans
			else tl_ans
		end

(*5*)
fun dates_in_months (dates_list: (int * int * int) list, months: int list) = 
	if null months
	then []
	else dates_in_month(dates_list, hd months) @ dates_in_months(dates_list, tl months)

(*6*)
fun get_nth(strs : string list, n : int) =
	if n = 1
	then hd strs
	else get_nth(tl strs, n-1)

(*7*)
fun date_to_string(date : int * int * int) =
	let
		val months_name = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in
		get_nth(months_name, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end
