open SMLofNJ.Cont;

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

signature ESCAPE =
  sig
    type void
    val coerce : void -> 'a
    val escape : (('1a -> void) -> '1a) -> '1a
  end;

structure Escape : ESCAPE =
  struct
    datatype void = VOID of void
    fun coerce (VOID v) = coerce v
    fun escape f = callcc (fn k=>f (fn x=>throw k x))
  end;

signature CONTROL =
  sig
    type ans
    val reset : (unit -> ans) -> ans
    val shift : (('1a -> ans) -> ans) -> '1a
  end;

functor Control (type ans) : CONTROL =
  struct
    open Escape
    exception MissingReset
    val mk : (ans -> void) ref =
      ref (fn _=>raise MissingReset)

    fun abort x = coerce (!mk x)
    type ans = ans
    fun reset t = escape (fn k=>let val m = !mk in
      mk := (fn r=>(mk := m; k r));
      abort (t ()) end)

    fun shift h = escape (fn k=>abort (h (fn v=>
      reset (fn ()=>coerce (k v)))))
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


fun range start len =
  if start >= len
  then []
  else start :: (range (start+1) len)

fun okay i c []      = true
  | okay i c (x::xs) = c <> x andalso (c-x) <> i andalso (c-x) <> (~i) andalso okay (i+1) c xs
                   
fun enum_nqueens n i l =
  if i = n then
    l
  else
      let val c = choose (range 0 n) in
          if okay 1 c l then enum_nqueens n (i+1) (c :: l) else fail ()
      end

open Timer
open Time

val timer = startCPUTimer ()
val n = valOf (Int.fromString (hd (CommandLine.arguments ())));
val res = List.length (withNondeterminism (fn () => enum_nqueens n 0 []))
val times = let val {usr : time, sys : time } = checkCPUTimer timer in
                (toMilliseconds sys,
                 toMilliseconds usr)
            end 


val _ = (print (LargeInt.toString (#2 times)); OS.Process.exit(OS.Process.success))
