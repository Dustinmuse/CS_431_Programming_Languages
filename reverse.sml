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
	end
