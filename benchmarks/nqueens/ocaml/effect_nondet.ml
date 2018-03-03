effect Choose : 'a list -> 'a

let withNonDeterminism m =
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

let rec range ?(start=0) len =
    if start >= len
    then []
    else start :: (range len ~start:(start+1))

let rec okay i c = function
  | [] -> true
  | x::xs -> c <> x && (c-x) <> i && (c-x) <> -i && okay (i+1) c xs

let rec enum_nqueens n i l =
  if i = n then l
  else begin
    let c = choose (range n) in
    if okay 1 c l then
      enum_nqueens n (i+1) (c :: l)
    else
      fail ()
  end

let () =
  let n = int_of_string (Array.get Sys.argv 1) in
  let li = withNonDeterminism (fun () -> enum_nqueens n 0 []) in
  print_int (List.length li);
  print_newline ()
