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



structure MaybeMonad : MONAD = struct
  type 'a m = 'a option

  fun return x = SOME x

  fun bind m f f' k = case m of
    NONE     => k NONE
  | (SOME x) => runf (f x) k
end;

structure M = Represent(struct structure M = MaybeMonad; type ans = int end)

fun fail () = M.reflect NONE
fun withFailable f = M.reify f



fun create_list_helper i k n = if i = n then
                                   []
                               else if (i+1) mod k = 0 then
                                   "invalid" :: create_list_helper (i+1) k n
                               else
                                   Int.toString i :: create_list_helper (i+1) k n

local
    fun parse_int_helper 0 s = 0
      | parse_int_helper i s = let val c = String.sub (s, i-1) in
                                   if c < #"0" orelse c > #"9" then
                                       fail ()
                                   else
                                       10 * (parse_int_helper (i-1) s) + (Char.ord c - Char.ord #"0")
                               end
in
  fun parse_int s = parse_int_helper (String.size s) s
end

val MODULUS = 1000000000
                                        
fun parse_sum []      = 0
  | parse_sum (s::ss) = case withFailable (fn () => parse_int s) of
                            SOME x => (x + parse_sum ss) mod MODULUS
                         |  NONE   => parse_sum ss                                          


open Timer
open Time


val (n,k) = let val [arg1, arg2] = CommandLine.arguments () in
                (valOf (Int.fromString arg1), valOf (Int.fromString arg2))
            end

val l = create_list_helper 0 k n
val timer = startCPUTimer ()
val x = parse_sum l
val times = let val {usr : time, sys : time } = checkCPUTimer timer in
                (toMilliseconds sys,
                 toMilliseconds usr)
            end 

val _ = print (Int.toString x ^ "\n")
val _ = (print (LargeInt.toString (#2 times)); OS.Process.exit(OS.Process.success))

                                          



