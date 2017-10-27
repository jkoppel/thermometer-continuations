signature UNIVERSAL = sig
    type u;
    val to_u : 'a -> u;
    val from_u : u -> 'a;
end;

structure Universal : UNIVERSAL = struct
  datatype u = U;
  val to_u = Unsafe.cast;
  val from_u = Unsafe.cast;
end;

signature CONTROL = sig
    type ans
    val reset : (unit -> ans) -> ans
    val shift : (('a -> ans) -> ans) -> 'a
end;

(* push : 'a list ref -> 'a -> () *)
fun push st x = (st := x :: !st)

(* pop : 'a list ref x-> 'a option*)
fun pop st = case !st of
    (x::xs) => (st := xs;
                SOME x)
  | []      => NONE

functor Control (type ans) : CONTROL = struct
  type ans = ans
  datatype frame = RET of Universal.u | ENTER
  type stack = frame list
  type reset_stack = ((unit -> ans) * stack * stack) list


                                                     
  exception Done of ans
  exception MissingFun

  val reset_stack : reset_stack ref = ref []
  val record_stack : stack ref = ref []
  val replay_stack : stack ref = ref []
  val cur_fun : (unit -> ans) ref = ref (fn () => raise MissingFun)


  fun invoke_cont f st =
    (push reset_stack (!cur_fun, !record_stack, !replay_stack);
     record_stack := [];
     replay_stack := rev st;
     cur_fun := f;
     
     let val res = (f () handle (Done x) => x)
         val (SOME (f', rec_stack', rep_stack')) = pop reset_stack in
         cur_fun := f';
         record_stack := rec_stack';
         replay_stack := rep_stack';
         
         res
     end)

  fun reset f = invoke_cont f []

  fun shift f = case pop replay_stack of
    (SOME (RET v)) => (push record_stack (RET v);
                       Universal.from_u v)
  | _     =>
    let val st = !record_stack
        val g = !cur_fun in
        (push record_stack ENTER;
         raise (Done (f (fn v => invoke_cont g (RET (Universal.to_u v) :: st)))))
    end
end;

signature MONAD = sig
    type 'a m
    val return : 'a -> 'a m;
    val bind : 'a m -> ('a -> 'b m) -> 'b m;
end;

signature RMONAD = sig
    structure M : MONAD
    val reflect : 'a M.m -> 'a
    val reify : (unit -> 'a) -> 'a M.m
end;

functor Represent (M : MONAD) : RMONAD = struct
  structure C = Control(type ans = Universal.u M.m)
  structure M = M
                    
  fun reflect m = C.shift (fn k => M.bind m k)
  fun reify t = M.bind (C.reset (fn () => M.return (Universal.to_u (t ()))))
                       (M.return o Universal.from_u)
end;



structure MaybeMonad : MONAD = struct
  type 'a m = 'a option

  fun return x = SOME x

  fun bind m f = case m of
    NONE     => NONE
  | (SOME x) => f x
end;

structure M = Represent(MaybeMonad)

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
  | parse_sum (s::ss) = (parse_int s + parse_sum ss) mod MODULUS
                                          


open Timer
open Time


val (n,k) = let val [arg1, arg2] = CommandLine.arguments () in
                (valOf (Int.fromString arg1), valOf (Int.fromString arg2))
            end

val l = create_list_helper 0 k n
val timer = startCPUTimer ()
val x = withFailable (fn () => parse_sum l)
val times = let val {usr : time, sys : time } = checkCPUTimer timer in
                (toMilliseconds sys,
                 toMilliseconds usr)
            end 

val _ = (print (case x of
                    (SOME y) => Int.toString y 
                 |  NONE     => "None");
         print "\n")
val _ = (print (LargeInt.toString (#2 times)); OS.Process.exit(OS.Process.success))

                                          



