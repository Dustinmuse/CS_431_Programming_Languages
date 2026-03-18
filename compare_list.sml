(*
    head1/2 = head of each list
    tail1/2 = tail of each list

    Description: Compares two lists for equality by recursively checking if their heads match.
                 Returns true if both lists are empty, false if one is empty and the other isn't,
                 and recursively compares the tails if both lists have matching heads.
*)
fun compare_list (nil, nil) = true
 |  compare_list (_, nil) = false
 |  compare_list (nil, _) = false
 |  compare_list (head1::tail1, head2::tail2) = 
        if head1 = head2
        then compare_list (tail1, tail2)
        else false;