open List;

signature RMONAD = sig
    type 'a m;
    val reflect : 'a m -> 'a;
    val reify : (unit -> 'a) -> 'a m;
end;


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


functor Cont(type answer) = struct
  type 'a m = ('a  -> answer) -> answer;

  type stack = Universal.u list;
  
  exception Throw of stack;

  val (stack : stack ref) = ref [];
  val (queue : stack ref) = ref [];

  fun reify f g =
    (g (f ())) handle (Throw st) =>
               (queue := rev st;
                stack := [];
                reify f g);

  fun callcc f =
    case !queue of
        (v::vs) => (queue := vs;
                    stack := v :: !stack;
                    Universal.from_u v)
      | [] =>  let val k = !stack in
                   f (fn v => raise (Throw ((Universal.to_u v) :: k)))
               end;
end;

structure C = Cont(type answer = string);

val (kref : (string -> string) ref) = ref (fn x => x)

(* Couldn't think of any good examples using callcc but not state.
 * We cheat a bit by using the built-in assignment, but it's okay because it's idempotent.
 * With a bit more work, we would run this in the StateT Cont monad instead
 *)
val rs = C.reify (fn () => let val t = C.callcc (fn k => (kref := k; "b"))
                           in
                               if t = "b" then (!kref) "a" else t ^ t
                           end)
                 (fn x => x);

