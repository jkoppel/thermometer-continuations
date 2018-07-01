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

let rs = withNondeterminism (fun () -> let x = choose [1; 2] in
                                       let y = choose [3; 4] in
                                       (x,y))

let rs' = withNondeterminism (fun () -> choose [3;4] * choose [5; 6])

let ts = withNondeterminism (fun () -> let  x = choose [3;4] * choose [5;7] in
                                       if x >= 20 then x
                                       else fail () )

