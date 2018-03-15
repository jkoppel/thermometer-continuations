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



structure ListMonad : MONAD = struct
  type 'a m = 'a list

  fun return x = [x]

  fun bind l f = case l of
    []      => []
  | (x::xs) => f x @ bind xs f
end;

structure N = Represent(ListMonad)

fun choose xs = N.reflect xs
fun fail () = choose []
fun withNondeterminism f = N.reify f

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
