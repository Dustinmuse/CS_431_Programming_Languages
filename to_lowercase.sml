(*
    head = head of inputted list
    tail = tail of inputted list
    
    Description: Takes a list of alphabetic characters (assuming no numbers or symbols),
                 extracts the head and checks if it is a capital letter. If it is, it adds 32
                 to its ASCII value, converting it into a lowercase letter. If not, it is left
                 alone. Then, it appends it to the result of calling the function on the
                 inputted list's tail.
*)
fun to_lowercase nil = nil
 |  to_lowercase (head::tail) =
    if (head >= #"A" andalso head <= #"Z")
    then chr( ord(head) + 32 ) :: to_lowercase tail
    else head :: to_lowercase tail;