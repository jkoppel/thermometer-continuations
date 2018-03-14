effect Choose : 'a list -> 'a

let withNondeterminism m =
  match m () with
  | v -> [v]
  | effect (Choose li) k ->
    (* multicore-ocaml's continuation are single shot;
       they must be explicitly duplicated before multi-shot consumption *)
    let ks = List.map (fun _ -> Obj.clone_continuation k) li in
    let vs = List.map2 (fun k v -> continue k v) ks li in
    List.flatten vs

let choose li = perform (Choose li)
let fail () = perform (Choose [])

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
