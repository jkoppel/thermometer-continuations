type ('b, 'c) cont = ('b -> 'c) -> 'c

module type MONAD = sig
    type 'a m
    val return : 'a -> 'a m

    val bind : 'a m -> ('a -> ('b m, 'c) cont) -> ('a -> ('b m, 'c) cont) -> ('b m, 'c) cont
end


module type UNIVERSAL = sig
  type u
  val to_u : 'a -> u
  val from_u : u -> 'a
end

module Universal : UNIVERSAL = struct
  type u = Obj.t
  let to_u = Obj.repr
  let from_u = Obj.obj
end

let f_to_u f x = Universal.to_u (f (Universal.from_u x))
let f_from_u f x = Universal.from_u (f (Universal.to_u x))

module type RMONAD = sig
  module M : MONAD
  open M
  type ans

  val reflect : 'a m -> 'a
  val reify : (unit -> ans) -> ans m
end

(* push : 'a list ref -> 'a -> () *)
let push st x = (st := x :: !st)

(* pop : 'a list ref x-> 'a option*)
let pop st = match !st with
| [] -> None
| (x::xs) -> (st := xs; Some x)

module Represent (A : sig module M : MONAD;; type ans end) : (RMONAD with module M = A.M and type ans = A.ans) = struct
  type ans = A.ans
  module M = A.M

  open A
  open M

  type stack = Universal.u list
  type rev_stack = stack

  exception Throw of stack
  exception Done of Universal.u

  let past : rev_stack ref = ref []
  let future : stack ref = ref []

  let (cont : (Universal.u -> Universal.u) ref) = ref (fun x -> assert false)

  let reflect m =
    match pop future with
    | Some u ->
      push past u;
      Universal.from_u u
    | None ->
      let st = !past in
      bind m
        (fun x k ->
           cont := f_to_u k;
           push past (Universal.to_u x);
           x)
        (fun x k ->
             cont := f_to_u k;
             raise (Throw ((Universal.to_u x) :: st)))
        (f_from_u (!cont))

  let rec reify_helper f =
    try
      let v = f () in
      f_from_u !cont (return v)
    with
    | (Throw new_past) ->
        past := [];
        future := List.rev new_past;
        reify_helper f

    | (Done x) -> Universal.from_u x


  let reify f =
    cont := (fun x -> raise (Done x));
    past := [];
    future := [];
    reify_helper f
end

module ListMonad : (MONAD with type 'a m = 'a list) = struct
  type 'a m = 'a list

  let return x = [x]

  let rec bind l f k = match l with
    | [] -> k []
    | x::xs ->
      f x @@ fun a ->
      bind xs f @@ fun b ->
      k (a @ b)

  let bind l first f k = match l with
    | [] -> k []
    | x::xs ->
      first x @@ fun a ->
      bind xs f @@ fun b ->
      k (a @ b)
end

module N = Represent(struct
  module M = ListMonad
  type ans = int list
end)

let choose xs = N.reflect xs
let fail () = N.reflect []
let withNondeterminism f = N.reify f

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
