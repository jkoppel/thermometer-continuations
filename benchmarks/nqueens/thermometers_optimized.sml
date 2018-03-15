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
    val bind : 'a m -> ('a -> ('b m -> 'c) -> 'c) -> ('a -> ('b m) fcont) -> ('b m -> 'c) -> 'c 
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
               (fn x => fn k =>
                  (cont := f_to_u k;
                   stack := (Universal.to_u x) :: st;
                   x))
               (fn x => packf (fn k =>
                   (cont := f_to_u k;
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

  fun bind []      f_first f_rest k = k []
    | bind (x::xs) f_first f_rest k =
      f_first x (fn a =>
      bind_rest xs f_rest (fn b =>
      k (a @ b)))

  and bind_rest []      f k = k []
    | bind_rest (x::xs) f k =
      runf (f x) (fn a =>
      bind_rest xs f (fn b =>
      k (a @ b)))
end
    
structure N = Represent(struct structure M = ListMonad; type ans = int list; end);


val withNondeterminism = N.reify
fun choose xs = N.reflect xs
fun fail () = choose []
                 
val n = valOf (Int.fromString (hd (CommandLine.arguments ())))

fun range start len =
  if start >= len
  then []
  else start :: (range (start+1) len)

val board_range = range 0 n

fun okay i c []      = true
  | okay i c (x::xs) = c <> x andalso (c-x) <> i andalso (c-x) <> (~i) andalso okay (i+1) c xs
                   
fun enum_nqueens i l =
  if i = n then
    l
  else
    let val c = choose (List.filter (fn c => okay 1 c l) board_range) in
      enum_nqueens (i+1) (c :: l)
    end

open Timer
open Time

val timer = startCPUTimer ()
val res = List.length (withNondeterminism (fn () => enum_nqueens 0 []))
val times = let val {usr : time, sys : time } = checkCPUTimer timer in
                (toMilliseconds sys,
                 toMilliseconds usr)
            end 

val _ = (
  print (Int.toString res); print "\n";
  print (LargeInt.toString (#2 times)); print "\n";
  OS.Process.exit(OS.Process.success)
)
