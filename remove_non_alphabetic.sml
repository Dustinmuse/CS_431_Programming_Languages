(*
    rev_helper = function that does the reversing
    head = head of input_list
    tail = tail of input_list
    reversed_list = reversed list of input_list

    Description: Takes a inputted list and extracts the head each time it is ran (recursively)
                 and pushes the head to a new list (empty at the start) then returns the
                 reversed list when the inputted list is empty.
*)
fun my_reverse input_list =
	let
		fun rev_helper ([], reversed_list) = reversed_list
		|   rev_helper (head::tail, reversed_list) = rev_helper (tail, head::reversed_list)
	in
		rev_helper (input_list, [])
	end;


(*
    helper = function that checks to make sure each character in the inputted list is alphabetic
    head = head of input_list
    tail = tail of input_list
    alphabetic_list = list of alphabetic characters
    
    Description: Takes a inputted list and extracts the head each time it is ran (recursively)
                 and pushes the head to a new list (empty at the start) then reverses the 
                 alphabetic list when the inputted list is empty and returns it.

*)
fun removeNonAlphabetic input_list =
    let
        fun helper ([], alphabetic_list) = my_reverse(alphabetic_list)
        | helper (head::tail, alphabetic_list) =
            if (head >= #"a" andalso head <= #"z") orelse
               (head >= #"A" andalso head <= #"Z") then
                helper (tail, head::alphabetic_list)
            else helper (tail, alphabetic_list)
    in
        helper (input_list, [])
    end;
