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

module type Answer = sig type ans end

module type CONTROL = sig
    include Answer
    val reset : (unit -> ans) -> ans
    val shift : (('a -> ans) -> ans) -> 'a
end

(* push : 'a list ref -> 'a -> () *)
let push st x = (st := x :: !st)

(* pop : 'a list ref x-> 'a option*)
let pop st = match !st with
| [] -> None
| (x::xs) -> (st := xs; Some x)

module Control (A : Answer) : CONTROL with type ans = A.ans = struct
  include A
  type frame = Ret of Universal.u | Enter
  type stack = frame list
  type reset_stack = ((unit -> ans) * stack * stack) list

  exception Done of ans
  exception MissingFun

  let reset_stack : reset_stack ref = ref []
  let record_stack : stack ref = ref []
  let replay_stack : stack ref = ref []
  let cur_fun : (unit -> ans) ref = ref (fun () -> raise MissingFun)

  let invoke_cont f st =
    push reset_stack (!cur_fun, !record_stack, !replay_stack);
    record_stack := [];
    replay_stack := List.rev st;
    cur_fun := f;

     let res = try f () with Done x -> x in
     match pop reset_stack with
     | None -> assert false
     | Some (future, record, replay) ->
       cur_fun := future;
       record_stack := record;
       replay_stack := replay;
       res

  let reset f = invoke_cont f []

  let shift f = match pop replay_stack with
    | (Some (Ret v)) ->
      push record_stack (Ret v);
      Universal.from_u v
    | Some Enter | None ->
      let st = !record_stack in
      let g = !cur_fun in
      push record_stack Enter;
      let k v = invoke_cont g (Ret (Universal.to_u v) :: st) in
      raise (Done (f k))
end

module Test = struct
  module C = Control(struct type ans = int end)
  open C

  let () = assert (1 + reset (fun () -> 2) = 3)
  let () = assert (1 + reset (fun () -> (2 + shift (fun k -> 3))) = 4)
  let () = assert (1 + reset (fun () -> (2 + shift (fun k -> k 3))) = 6)
  let () = assert (1 + reset (fun () -> (2 + shift (fun k -> k (k 3)))) = 8)
  let () = assert (1 + reset (fun () -> (2 + shift (fun k -> k (k 3)))) = 8)
  let () =
    assert
      (1 + reset (fun () ->
           (2 + reset (fun () ->
                3 + shift (fun k -> k (k 3)))))
       = 1 + (2 + (3 + (3 + 3))))
  let () =
    assert
      (1 + reset (fun () ->
           4 + shift (fun k1 -> 2 + reset (fun () ->
               3 * shift (fun k2 -> k1 (k2 3)))))
       = 1 + 2 + (4 + (3 * 3)))
  let () =
    assert
      (1 + reset (fun () ->
           4 + shift (fun k1 -> 2 + reset (fun () ->
               3 * shift (fun k2 -> k2 (k1 3)))))
       = 1 + (2 + 3 * (4 + 3)))
  let () =
    assert
      (1 + reset (fun () ->
           2 + shift (fun k1 ->
               3 * shift (fun k2 -> k2 (k1 10))))
       = 1 + 3 * (2 + 10))
end

module type MONAD = sig
    type 'a m
    val return : 'a -> 'a m
    val bind : 'a m -> ('a -> 'b m) -> 'b m
end

module type RMONAD = sig
    module M : MONAD
    val reflect : 'a M.m -> 'a
    val reify : (unit -> 'a) -> 'a M.m
end

module Represent (M : MONAD) : RMONAD with module M = M = struct
  module Answer = struct type ans = Universal.u M.m end
  module C = Control(Answer)
  module M = M

  let reflect m = C.shift (fun k -> M.bind m k)
  let reify t = M.bind (C.reset (fun () -> M.return (Universal.to_u (t ()))))
                       (fun v -> M.return (Universal.from_u v))
end

module ListMonad : (MONAD with type 'a m = 'a list) = struct
  type 'a m = 'a list

  let return x = [x]
  let rec bind l f = List.flatten (List.map f l)
end

module N = Represent(ListMonad)


let choose xs = N.reflect xs
let fail () = N.reflect []
let withNondeterminism f = N.reify f

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
