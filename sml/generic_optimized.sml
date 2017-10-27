open List

       
signature UNIVERSAL = sig
  type u
  val to_u : 'a -> u
  val from_u : u -> 'a
end

                          
structure Universal : UNIVERSAL = struct
  datatype u = U
  val to_u = Unsafe.cast
  val from_u = Unsafe.cast
end

type 'b fcont = Universal.u  (* forall 'c. ('b -> 'c) -> 'c *)
fun packf (f : ('b -> 'c) -> 'c) = Universal.to_u f
fun runf (f : 'b fcont) : ('b -> 'c) -> 'c = Universal.from_u f

                                                   
signature MONAD = sig
    type 'a m
    val return : 'a -> 'a m
    (* First function to bind may be tail-called; second one may not *)
    val bind : 'a m -> ('a -> ('b m) fcont) -> ('a -> ('b m) fcont) -> ('b m -> 'c) -> 'c 
end
    
(* bind : 'a m -> ('a -> (('b m -> 'c) -> 'c)) -> ('a -> (('b m -> 'c) -> 'c)) -> ('b m -> 'c) -> 'c *)
                      
signature RMONAD = sig
  structure M : MONAD
  type ans
        
  val reflect : 'a M.m -> 'a
  val reify : (unit -> ans) -> ans M.m
end
                       
fun f_to_u   f x = Universal.to_u   (f (Universal.from_u x))
fun f_from_u f x = Universal.from_u (f (Universal.to_u   x))
      
functor Represent (S : sig structure M : MONAD; type ans; end) : RMONAD = struct
  open S;
  open M;

  type stack = Universal.u list

  exception Invoke of stack
  exception Done of Universal.u

  val pos = ref 0
  val len = ref 0

  val (stack : stack ref) = ref []
  val (cont : (Universal.u -> Universal.u) ref) = ref (fn x => raise (Done x))

  fun reflect m =
    if !pos < !len then
      (pos := !pos + 1;
       Universal.from_u (nth (!stack, !len - !pos)))
    else
      let val st = !stack in
          bind m
               (fn x => packf (fn k => (cont := f_to_u k;
                                        stack := (Universal.to_u x) :: st;
                                        x)))
               (fn x => packf (fn k => (cont := f_to_u k;
                                        raise (Invoke ((Universal.to_u x) :: st)))))
               (f_from_u (!cont))
      end

  fun reify_helper f =
    (let val m = return (f ()) in
         f_from_u (!cont) m (* Must evaluate f () before !cont *)
     end)
    handle
      (Invoke st) =>
        (pos := 0;
         len := length st;
         stack := st;
         reify_helper f)
          
    | (Done x) => Universal.from_u x


  fun reify f =
    (cont := (fn x => raise (Done x));
     pos := 0;
     len := 0;
     stack := [];
     reify_helper f)
end



structure ListMonad : MONAD = struct
  type 'a m = 'a list

  fun return x = [x]

  (* Uses f the first time, and f' thereafter *)
  fun bind []      f f' k = k []
    | bind (x::xs) f f' k = runf (f x) (fn a => bind xs f' f'
                                                     (fn b => k (a @ b)))
end
    
structure N = Represent(struct structure M = ListMonad; type ans = int * int; end);


val withNondeterminism = N.reify
fun amb xs = N.reflect xs
fun fail () = amb []
                 

val rs = withNondeterminism (fn () => let val x = amb [1, 2]
                                        val y = amb [3, 4]
                                      in
                                        (x,y)
                                      end);

val rs' = withNondeterminism (fn () => (amb [3,4] * amb [5, 6], 0))

val ts = withNondeterminism (fn () => let val x = amb [3,4] * amb [5,7] in
                                      if x >= 20 then (x, 0)
                                      else fail () end);
    

structure Maybe : MONAD = struct
  type 'a m = 'a option
  fun return x = SOME x
  
  fun bind NONE     f f' k = k NONE
    | bind (SOME x) f f' k = runf (f x) k
end

structure M = Represent(struct structure M = Maybe; type ans = int; end);

fun exMaybe xs = M.reify (fn () => let val small = M.reflect (find (fn x => x < 10) xs)
                                       val large = M.reflect (find (fn x => x > 100) xs) in
                                       small + large
                                   end)

val exMaybe1 = exMaybe [20, 20, 9]
val exMaybe2 = exMaybe [20, 200, 9]
val exMaybe3 = exMaybe [20, 200, 10]

