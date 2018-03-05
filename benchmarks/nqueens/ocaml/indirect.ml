let n = int_of_string Sys.argv.(1)

let range =
  let rec range i =
    if i = n then []
    else i :: range (i + 1)
  in range 0

let rec okay i c = function
  | [] -> true
  | x::xs -> c <> x && (c-x) <> i && (c-x) <> -i && okay (i+1) c xs

let rec enum_nqueens i l =
  if i = n then
    [l]
  else
    List.fold_left
      (fun k c -> if okay 1 c l then k @ enum_nqueens (i+1) (c :: l) else k)
      [] range

let solutions = enum_nqueens 0 []

let () =
  print_int (List.length solutions);
  print_newline ()
