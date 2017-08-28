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

structure C = Control(type ans = int);

val exC = 1 + C.reset (fn () => 2 * (C.shift (fn k => k (k (5)))));

(* Should return 37 *)
val exNest1 = 1 + C.reset (fn () => 2 + C.shift (fn k =>  3*(C.shift (fn l => l (k 10))) ));
val exNest2 = 1 + C.reset (fn () => 2 + C.shift (fn k =>  3*(C.shift (fn l => l 10)) ));
val exNest3 = 1 + C.reset (fn () => 2 + C.shift (fn k =>  3*(C.shift (fn l => k 10)) ));

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

val rs = N.reify (fn () => let val x = N.reflect [1, 2]
                               val y = N.reflect [3, 4] in
                               (x,y)
                           end)


fun amb xs = N.reflect xs
fun fail () = amb []

val rsF = N.reify (fn () => let val x = amb [3,4] * amb [5,7] in
                                 if x >= 20 then x
                                 else fail () end);

functor ContMonad (type ans) : MONAD = struct
  type 'a m = ('a -> ans) -> ans

  fun return x = (fn f => f x)
      
  fun bind m t = (fn k => m (fn v => t v k))
end;

structure C = Represent(ContMonad(type ans = string))

fun callcc f = C.reflect (fn k => k (f k))

val kref : (string -> string) ref = ref (fn x => x)

(* Couldn't think of any good examples using callcc but not state.
 * We cheat a bit by using the built-in assignment, but it's okay because it's idempotent.
 * With a bit more work, we would run this in the StateT Cont monad instead
 *)
val c1 = C.reify (fn () => let val t = callcc (fn k => (kref := k; "b"))
                           in
                               if t = "b" then (!kref) "a" else t ^ t
                           end)
                 (fn x => x)

val c2 = C.reify (fn () => let val t = C.reflect (fn k => k (k "hi")) in
                               t ^ " there"
                           end)
                 (fn x => x)

functor StateMonad (type state) : MONAD = struct
  type 'a m = state -> 'a * state
  fun return x = fn s => (x, s)
  fun bind m f = fn s => let val (x, s') = m s
                         in f x s' end
end;

structure S = Represent (StateMonad (type state = int) )

fun tick () = S.reflect (fn s => ((), s+1))
fun get ()  = S.reflect (fn s => (s, s))
fun put n = S.reflect (fn _ => ((), n))

val exState = #1 (S.reify (fn () => (put 5; tick ();
                                 2 * get ()))
                          0)
                      
(*
val exState = S.reify (fn () => (S.reflect (put 1);
                                 let val x = S.reflect get in
                                     x + 1
                                 end))
                      3
*)
