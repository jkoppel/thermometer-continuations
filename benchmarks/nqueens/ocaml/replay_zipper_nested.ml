open List;;

let past = ref []
let future = ref []

let push r x = (r := x :: !r)
let pop r = match !r with
  | [] -> None
  | x::xs -> r := xs; Some x

let nest : (int list * int list) list ref = ref []

let rec decr = function
  | (0::ns) -> decr ns
  | (n::ns) -> (n-1)::ns
  | []      -> []

exception Empty;;

let withNondeterminism f =
  push nest (!past, !future);
  past := []; future := [];
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
  let result = loop [] f in
  begin match pop nest with
    | None -> assert false
    | Some (p, f) ->
      past := p;
      future := f;
      result
  end

let choose = function
  | [||] -> raise Empty
  | xs ->
    match pop future with
    | Some idx ->
      push past idx;
      xs.((Array.length xs - 1) - idx)
    | None ->
      let idx = (Array.length xs - 1) in
      push past idx;
      xs.(0)

let fail () = choose [||]

let n = int_of_string Sys.argv.(1)

let range = Array.init n (fun i -> i)

let rec okay i c = function
  | [] -> true
  | x::xs -> c <> x && (c-x) <> i && (c-x) <> -i && okay (i+1) c xs

let rec enum_nqueens i l =
  if i = n then
    l
  else begin
    let c = choose range in
    (* (* this implementation is nesting-safe: *)
    List.iter ignore (withNondeterminism (fun () -> choose [|(); (); ()|])); *)
    if not (okay 1 c l) then fail();
    enum_nqueens (i + 1) (c :: l)
  end

let solutions = withNondeterminism (fun () -> enum_nqueens 0 [])

let () =
  print_int (List.length solutions);
  print_newline ()
