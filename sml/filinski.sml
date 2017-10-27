open SMLofNJ.Cont;

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

structure C = Control(type ans = int);

val exC = 1 + C.reset (fn () => 2 * (C.shift (fn k => k (k (5)))));

(* Should return 37 *)
val exNest1 = 1 + C.reset (fn () => 2 + C.shift (fn k =>  3*(C.shift (fn l => l (k 10))) ));
val exNest2 = 1 + C.reset (fn () => 2 + C.shift (fn k =>  3*(C.shift (fn l => l 10)) ));
val exNest3 = 1 + C.reset (fn () => 2 + C.shift (fn k =>  3*(C.shift (fn l => k 10)) ));

val exNest4 =  C.reset (fn () =>
                  C.shift (fn k =>  3*(C.shift (fn l => l (k 10))))
                  * (C.shift (fn k => k 5 + k 6)));