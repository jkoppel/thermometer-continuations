module type UNIVERSAL = sig
  type u
  val to_u : 'a -> u
  val from_u : u -> 'a
end

module Universal : UNIVERSAL = struct
  type u = U1 | U2 of int
  let to_u = Obj.magic
  let from_u = Obj.magic
end

let (br_idx : Universal.u list list ref) = ref [];;
let pos = ref 0;;

exception Empty;;

let rec decr = function
  | [] :: _ -> assert false
  | ([_]::ns) -> decr ns
  | ((_::xs)::ns) -> xs::ns
  | []      -> []

let withNondeterminism f =
  let rec loop acc f =
    let v = try [f()] with Empty -> [] in
    let acc = v @ acc in
    br_idx := decr !br_idx;
    pos := List.length !br_idx;
    match !br_idx with
    | [] -> List.rev acc
    |  _  -> loop acc f
  in loop [] f

let choose = function
  | [] -> raise Empty
  | xs ->
    if !pos > 0 then begin
      pos := !pos - 1;
      Universal.from_u (List.hd (List.nth !br_idx !pos))
    end else begin
      br_idx := List.map Universal.to_u xs :: !br_idx;
      List.hd xs
    end

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
