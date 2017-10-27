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

                                          



