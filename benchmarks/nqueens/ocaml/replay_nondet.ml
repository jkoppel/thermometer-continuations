open List;;

let (br_idx : int list ref) = ref [];;
let pos = ref 0;;

exception Empty;;

let rec decr = function
  | (0::ns) -> decr ns
  | (n::ns) -> (n-1)::ns
  | []      -> []

let withNondeterminism f =
  let rec loop acc f =
    let v = try [f()] with Empty -> [] in
    let acc = v @ acc in
    br_idx := decr (!br_idx);
    pos := length (!br_idx);
    match !br_idx with
    | [] -> List.rev acc
    |  _  -> loop acc f
  in loop [] f

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
    l
  else begin
    let c = choose range in
    if not (okay 1 c l) then fail();
    enum_nqueens (i + 1) (c :: l)
  end

let solutions = withNondeterminism (fun () -> enum_nqueens 0 [])

let () =
  print_int (List.length solutions);
  print_newline ()
