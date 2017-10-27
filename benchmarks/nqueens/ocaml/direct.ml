let rec range ?(start=0) len =
    if start >= len
    then []
    else start :: (range len ~start:(start+1))

let rec okay i c = function
  | [] -> true
  | x::xs -> c <> x && (c-x) <> i && (c-x) <> -i && okay (i+1) c xs
                   
let rec enum_nqueens n i l =
  if i == n then
    [l]
  else
    List.fold_left (fun k c ->
                     if okay 1 c l then k @ enum_nqueens n (i+1) (c :: l) else k)
                   []
      (range n)
      ;;




let n = int_of_string (Array.get Sys.argv 1) in
print_int (List.length (enum_nqueens n 0 []))

