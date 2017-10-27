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


open List;

functor StateTListMonad(type state) : MONAD = struct
  type 'a m =  state -> (('a * state) list)
  fun return a = (fn s => [(a, s)])
  fun bind m f = (fn s => concat (map (fn (a,s') => f a s')
                                      (m s)))
end;


structure ParserMonad = StateTListMonad(type state = char list);
structure PM = Represent(ParserMonad);


(* This section is translated from the parser in http://www.cse.chalmers.se/~rjmh/OPLSS/Monads%20and%20all%20that%20--%20II.pdf *)

(* bactrackingChoice : (unit -> 'a) -> (unit -> 'a) -> 'a *)
fun backtrackingChoice x y = PM.reflect (fn s => (PM.reify x) s @ (PM.reify y) s)
                                                              
fun get ()  = PM.reflect (fn s => [(s, s)])
fun put n = PM.reflect (fn _ => [((), n)])
fun fail () = PM.reflect (fn _ => [])
fun guard true  = ()
  | guard false = fail ()

fun popc () = case get () of
                  []      => fail ()
                | (c::cs) => (put cs; c)
                      
fun exactly c = let val c' = popc () in
                    (guard (c = c'); c)
                end

fun satisfy pred () = let val c = popc () in
                         (guard (pred c); c)
                     end

                       
fun many p = backtrackingChoice (fn () => some p)
                                (fn () => [])
and some p = p () :: (many p)
                         
fun number () = let val ds = some (satisfy Char.isDigit) in
                    valOf (Int.fromString (String.implode ds))
                end

val MODULUS = 30000

fun expr () = backtrackingChoice (fn () => let val a = term ()
                                               val _ = exactly #"+"
                                               val b = term ()
                                           in
                                               (a + b) mod MODULUS
                                           end)
                                 term
and term () = backtrackingChoice (fn () => let val a = factor ()
                                               val _ = exactly #"*"
                                               val b = factor ()
                                           in
                                               (a * b) mod MODULUS
                                           end)
                                 factor
and factor () = backtrackingChoice number
                                   (fn () => let val _ = exactly #"("
                                                 val a = expr ()
                                                 val _ = exactly #")"
                                             in
                                                 a
                                             end)
                             

                 
fun randBool rng = if Random.randRange (0,1) rng = 0 then false else true
fun randSplit rng n = let val i = Random.randRange (1, n-1) rng in
                          (i, n-i)
                      end

fun randInt rng = Int.toString (Random.randRange (0, 100) rng)
                          
fun mkRandExp rng 1 = randInt rng
  | mkRandExp rng n = if randBool rng then
                          let val (l, r) = randSplit rng n in
                              mkRandTerm rng l ^ "+" ^ mkRandTerm rng r
                          end
                      else
                          mkRandTerm rng n
and mkRandTerm rng 1 = randInt rng
  | mkRandTerm rng n = if randBool rng then
                           let val (l, r) = randSplit rng n in
                               mkRandFactor rng l ^ "*" ^ mkRandFactor rng r
                           end
                       else
                           mkRandFactor rng n
and mkRandFactor rng 1 = randInt rng
  | mkRandFactor rng n = "(" ^ mkRandExp rng n ^ ")"

                                                     
open Timer
open Time


val (n,k) = let val [arg1, arg2] = CommandLine.arguments () in
                (valOf (Int.fromString arg1), valOf (Int.fromString arg2))
            end

val rng = Random.rand (n, k)
val s = mkRandExp rng n
val sl = String.explode s
val timer = startCPUTimer ()
val x = PM.reify (fn () => expr ()) sl
val times = let val {usr : time, sys : time } = checkCPUTimer timer in
                (toMilliseconds sys,
                 toMilliseconds usr)
            end 

(* val _ = print (x ^ "\n")*)

val _ = (print (LargeInt.toString (#2 times)); OS.Process.exit(OS.Process.success))

                                          

