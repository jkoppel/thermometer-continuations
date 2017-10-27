open List;;

let (br_idx : int list ref) = ref [];;
let pos = ref 0;;

exception Empty;;

let rec decr = function
  | (0::ns) -> decr ns
  | (n::ns) -> (n-1)::ns
  | []      -> []

let rec withNondeterminism f =
  let v = try [f()] with Empty -> [] in
  br_idx := decr (!br_idx);
  pos := length (!br_idx);
  match !br_idx with
    [] -> v
  |  _  -> v @ withNondeterminism f

let choose = function
  | [] -> raise Empty
  | xs -> if not (0 = !pos) then
            let idxFromEnd = nth (!br_idx) (!pos - 1) in
            pos := (!pos) - 1;
            nth xs ((length xs - 1) - idxFromEnd)
          else
            (br_idx := (length xs - 1) :: !br_idx;
             hd xs)

let fail () = choose []



let rec range ?(start=0) len =
    if start >= len
    then []
    else start :: (range len ~start:(start+1))

let rec okay i c = function
  | [] -> true
  | x::xs -> c <> x && (c-x) <> i && (c-x) <> -i && okay (i+1) c xs
                   
let rec enum_nqueens n i l =
  if i == n then
    l
  else
    (let c = choose (range n) in
     if okay 1 c l then
       enum_nqueens n (i+1) (c :: l)
     else
       fail ())


;;


let n = int_of_string (Array.get Sys.argv 1) in
print_int (List.length (withNondeterminism (fun () ->enum_nqueens n 0 [])))

