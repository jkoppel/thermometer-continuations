open List


type 'b fcont = {k : 'c. ('b -> 'c) -> 'c}


let runf f x k = (f x).k k
  
module type MONAD = sig
    type 'a m
    val return : 'a -> 'a m

    val bind : 'a m -> ('a -> ('b m) fcont) -> ('b m -> 'c) -> 'c
end

    
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

let f_to_u f x = Universal.to_u (f (Universal.from_u x))
let f_from_u f x = Universal.from_u (f (Universal.to_u x))
    
module type RMONAD = sig
  module M : MONAD
  open M
  type ans
        
  val reflect : 'a m -> 'a
  val reify : (unit -> ans) -> ans m
end
      
module Represent (A : sig module M : MONAD;; type ans end) : (RMONAD with module M = A.M and type ans = A.ans) = struct
  type ans = A.ans
  module M = A.M
        
  open A
  open M

  type stack = Universal.u list

  exception Throw of stack
  exception Done of Universal.u

  let pos = ref 0
  let len = ref 0

  let (stack : stack ref) = ref []
  let (cont : (Universal.u -> Universal.u) ref) = ref (fun x -> raise (Done x))

  let reflect m =
    if !pos < !len then
      (pos := !pos + 1;
       Universal.from_u (nth (!stack) (!len - !pos)))
    else
      let st = !stack in
      bind m
        (fun x -> {k = fun k -> cont := f_to_u k;
                                raise (Throw ((Universal.to_u x) :: st))})
        (f_from_u (!cont))

  let rec reify_helper f =
    try
      (f_from_u (!cont)) (return (f ()))
    with
    | (Throw st) ->
        pos := 0;
        len := length st;
        stack := st;
        reify_helper f
          
    | (Done x) -> Universal.from_u x


  let reify f =
    cont := (fun x -> raise (Done x));
    stack := [];
    reify_helper f
end



module ListMonad : (MONAD with type 'a m = 'a list) = struct
  type 'a m = 'a list

  let return x = [x]

  let rec bind l f k = match l with
    []      -> k []
  | (x::xs) -> runf f x (fun a -> bind xs f
                                    (fun b -> k (a @ b)))
end
    
module N = Represent(struct module M = ListMonad;; type ans = int * int;; end)


let rs = N.reify (fun () -> let x = N.reflect [1; 2] in
                            let y = N.reflect [3; 4] in
                            (x,y))
    

    
module Maybe : (MONAD with type 'a m = 'a option) = struct
  type 'a m = 'a option
  let return x = Some x
  
  let bind m f k = match m with
      None     -> k None
    | (Some x) -> runf f x k                         
end

module M = Represent(struct module M = Maybe;; type ans = int;; end)

let findi p x = try Some (find p x) with Not_found -> None

let exMaybe xs = M.reify (fun () -> let small = M.reflect (findi (fun x -> x < 10) xs) in
                                    let large = M.reflect (findi (fun x -> x > 100) xs) in
                                    small + large)

let exMaybe1 = exMaybe [20; 20; 9]
let exMaybe2 = exMaybe [20; 200; 9]
let exMaybe2 = exMaybe [20; 200; 10]
