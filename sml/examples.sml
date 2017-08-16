val firstTime = ref false

(* amb : 'a * 'a -> 'a *)
fun amb (x1,x2) = if !firstTime then x1 else x2
                    
(* withNondeterminism : (unit -> 'a) -> 'a list *)
fun withNondeterminism f = [(firstTime := true; f ()),
                            (firstTime := false; f ())]

                          
val y = withNondeterminism (fn () => 3 * amb (5,6))
(* val y = [15,18] : int list *)


signature MONAD = sig
    type 'a m;
    val return : 'a -> 'a m;
    val bind :  'a m -> ('a -> 'b m) -> 'b m;
end;

structure ListMonad : MONAD = struct
  type 'a m = 'a list
  fun return x = [x]
  
  fun bind []      f = []
   | bind (x::xs) f = f x @ bind xs f
end;

open ListMonad;

bind [3,4] (fn x =>
bind [5,6] (fn y =>
  return (x * y)))
(* val it = [15,18,20,24] : int list *)
        
