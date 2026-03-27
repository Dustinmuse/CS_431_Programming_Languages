(*
    Name: Dustin Muse & J.D. Otis
    Date: 04/03/2026
    Purpose: Checks if a given string is a palindrome by removing non-alphabetic characters, converting to lowercase, and comparing to a reversed version of itself.
*)


(*
    rev_helper = function that does the reversing
    head = head of input_list
    tail = tail of input_list
    reversed_list = reversed list of input_list

    Description: Takes a inputted list and extracts the head each time it is ran (recursively)
                 and pushes the head to a new list (empty at the start) then returns the
                 reversed list when the inputted list is empty.

    Base Case: if the inputted list is empty, return reversed_list (which is a empty list)

    Recursive Case: if the inputted list is not empty, extract the head from the inputted list
                    then attach it to the reversed_list (which is empty at the start) and
                    call the function rev_helper on the tail of the inputted list.
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

    Base Case: if the inputted list is empty, return my_reverse of alphabetic_list 
               (which is a empty list)

    Recursive Case: if the inputted list is not empty, extract the head from the inputted list
                    then checks if the ASCII of the head if is between 97 (a) and 122 (z)
                    or 65 (A) and 90 (Z). If this is true then it takes the head (character)
                    and attaches it to the alphabetic_list (which is empty at the start) and
                    calls the function helper on the tail of the inputted list.
                    If it is not true then it just calls the function helper on the tail of
                    the inputted list and does not attach the head of the inputted list to
                    alphabetic_list.

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



(*
    head = head of inputted list
    tail = tail of inputted list
    
    Description: Takes a list of alphabetic characters (assuming no numbers or symbols),
                 extracts the head and checks if it is a capital letter. If it is, it adds 32
                 to its ASCII value, converting it into a lowercase letter. If not, it is left
                 alone. Then, it appends it to the result of calling the function on the
                 inputted list's tail.

    Base Case: if the inputted list is empty, return an empty list

    Recursive Case: if the inputted list is not empty, check if the ASCII value of the inputted
                    lists head falls between #"A" and #"Z" (inclusive), indicating it is a capital
                    letter. If it is, convert it to lowercase by adding 32 to its ASCII value and
                    then attach it to the result of calling the function on the tail of the inputted
                    list. If it is not a capital letter, just attach it to the result of calling the
                    function on the tail of the inputted list.
*)
fun changeToLowercase nil = nil
 |  changeToLowercase (head::tail) =
    if (head >= #"A" andalso head <= #"Z")
    then chr( ord(head) + 32 ) :: changeToLowercase tail
    else head :: changeToLowercase tail;



(*
    head1/2 = head of each list
    tail1/2 = tail of each list

    Description: Compares two lists and returns true only when they are the same length
                 and every element at each position is equal.

    Base Case 1: Both lists are empty, so they match exactly. Return true.
    Base Case 2: The second list is empty, so the length of lists differ. Return false.
    Base Case 3: The first list is empty, so the length of lists differ. Return false.

    Recursive Case: Take the head of the 1st inputted list and the head of the 2nd inputted list
                    (which is the reversed version of the 1st inputted list) and extract the head of both lists
                    and compare them. If they are equal, call the function compare_list on the tail of both lists.
                    If they are not equal, return false.
*) 
fun compare_list (nil, nil) = true
 |  compare_list (_, nil) = false
 |  compare_list (nil, _) = false
 |  compare_list (head1::tail1, head2::tail2) = 
        if head1 = head2
        then compare_list (tail1, tail2)
        else false;



(* 
    word = inputted word
    chars = list of characters in the word
    letters = list of only the alphabetic letters of chars
    lower = list of lowercase letters from the list letters
    rev = reversed list of lower

    Description: Takes a string input then calls the function explode on the word. Then it
                 calls the function removeNonAlphabetic on chars. Then it calls the
                 function changeToLowercase on letters. Then it creates a reverse of the
                 inputted string by using the function my_reverse on lower. Then it
                 calls the function compare_list on lower and rev. It returns true
                 if it is a palindrome and false if it is not.
*)
fun is_palindrome word =
	let
		val chars = explode word
		val letters = removeNonAlphabetic chars
		val lower = changeToLowercase letters
		val rev = my_reverse lower
	in
		compare_list (lower, rev)
	end;
