open List;;

module DynArray = struct
  type 'a t = {
    mutable arr : 'a array;
    mutable len : int;
  }

  let create n init = {
    arr = Array.make (max n 32) init;
    len = n;
  }

  (* size of the underlying storage *)
  let size t = Array.length t.arr

  (* logical length *)
  let length t = t.len

  let get t n =
    if n >= t.len then invalid_arg "DynArray.get";
    t.arr.(n)

  let set t n v =
    if n >= t.len then invalid_arg "DynArray.set";
    t.arr.(n) <- v

  let resize_up t new_len elem =
    if new_len < t.len then invalid_arg "DynArray.resize_up";
    let old_size = Array.length t.arr in
    if new_len <= old_size then begin
      t.len <- new_len;
    end else begin
      let new_size = ref old_size in
      while !new_size < new_len do new_size := !new_size * 2 done;
      let new_arr = Array.make !new_size elem in
      Array.blit t.arr 0 new_arr 0 t.len;
      t.arr <- new_arr;
      t.len <- new_len;
    end

  let resize_down t new_len =
    if new_len > t.len then invalid_arg "DynArray.resize_down";
    let old_size = Array.length t.arr in
    if new_len >= old_size / 2 || old_size <= 32 then begin
      t.len <- new_len
    end else begin
      let new_size = ref old_size in
      while !new_size > new_len * 2 do new_size := !new_size / 2 done;
      let new_arr = Array.sub t.arr 0 (max 32 !new_size) in
      t.len <- new_len;
      t.arr <- new_arr;
    end
end

let br_idx = DynArray.create 0 0;;
let pos = ref 0;;

exception Empty;;

let decr br_idx =
  let len = DynArray.length br_idx in
  if len = 0 then () else begin
    let new_len = ref len in
    while !new_len > 0 && DynArray.get br_idx (!new_len - 1) = 0 do
      decr new_len;
    done;
    DynArray.resize_down br_idx !new_len;
    let next = !new_len - 1 in
    if next >= 0 then
      DynArray.set br_idx next (DynArray.get br_idx next - 1);
  end

let withNondeterminism f =
  let rec loop acc f =
    let v = try [f()] with Empty -> [] in
    let acc = v @ acc in
    decr br_idx;
    pos := 0;
    if DynArray.length br_idx = 0
    then List.rev acc
    else loop acc f
  in loop [] f

let choose xs =
  let len = Array.length xs in
  if len = 0 then raise Empty;
  let br_len = DynArray.length br_idx in
  if !pos < br_len then begin
    let idxFromEnd = DynArray.get br_idx !pos in
    incr pos;
    Array.get xs ((len - 1) - idxFromEnd)
  end else begin
    DynArray.resize_up br_idx (br_len + 1) 0;
    DynArray.set br_idx br_len (len - 1);
    incr pos;
    Array.get xs 0
  end

let fail () = choose [| |]

let n = int_of_string Sys.argv.(1)

let range = Array.init (int_of_string Sys.argv.(1)) (fun i -> i)

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
