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

(*8*)
fun number_before_reaching_sum (sum : int, xs : int list) =
	let
		fun reach_sum (target : int, xs : int list, sum : int, count : int) =
			let
				val next = sum + (hd xs)
			in
				if sum < target andalso target <= next
				then count
				else reach_sum(target, tl xs, next, count + 1)
			end
	in
		reach_sum(sum, xs, 0, 0)
	end

(*9*)
fun what_month(day : int) =
	let
		val days_list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		number_before_reaching_sum(day, days_list) + 1
	end

(*10*)
fun month_range(day1 : int, day2 : int) =
	if day2 < day1
	then []
	else what_month day1 :: month_range(day1 + 1, day2)

(*11*)
fun oldest (dates : (int * int * int) list) =
	if null dates 
	then NONE
	else
		let 
			fun oldest_nonempty (dates : (int * int * int) list) =
				if null (tl dates)
				then hd dates 
				else 
					let
						val tl_ans = oldest_nonempty(tl dates)
					in
						if is_older(hd dates, tl_ans) then hd dates else tl_ans
					end
		in
			SOME (oldest_nonempty dates)
		end

(*12*)
fun remove_duplicate(xs : int list) =
	let
		fun in_list (x : int, xs : int list) =
			if null xs
			then false
			else if (hd xs) = x
			then true
			else in_list(x, tl xs)
	in
		if null xs 
		then []
		else
			let
				val tl_ans = remove_duplicate(tl xs)
			in
				if in_list(hd xs, tl_ans)
				then tl_ans
				else (hd xs) :: tl_ans
			end
	end
		