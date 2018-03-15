open List;;

let past = ref []
let future = ref []

exception Empty;;

let push r x = (r := x :: !r)
let pop r = match !r with
  | [] -> None
  | x::xs -> r := xs; Some x

let rec decr = function
  | (0::ns) -> decr ns
  | (n::ns) -> (n-1)::ns
  | []      -> []

let withNondeterminism f =
  let rec loop acc f =
    let v = try [f()] with Empty -> [] in
    let acc = v @ acc in
    assert (!future = []);
    let next = List.rev (decr !past) in
    future := next;
    past := [];
    if next = []
    then List.rev acc
    else loop acc f
  in
  past := [];
  future := [];
  loop [] f

let choose = function
  | [] -> raise Empty
  | xs ->
    match pop future with
    | Some idx ->
      push past idx;
      nth xs ((length xs - 1) - idx)
    | None ->
      let idx = (length xs - 1) in
      push past idx;
      hd xs

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
    let c = choose (List.filter (fun c -> okay 1 c l) range) in
    enum_nqueens (i + 1) (c :: l)
  end

let solutions = withNondeterminism (fun () -> enum_nqueens 0 [])

let () =
  print_int (List.length solutions);
  print_newline ()
