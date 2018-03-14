open List;

val past : int list ref = ref [];
val future : int list ref = ref [];

fun push st x = (st := x :: !st)

fun pop st = case !st of
    (x::xs) => (st := xs; SOME x)
  | []      => NONE


fun decr (0::ns) = decr ns
  | decr (n::ns) = (n-1)::ns
  | decr []      = []

exception Empty;

fun withNondeterminism f =
  let val v = [f()] handle Empty => []
      val next = rev (decr (!past))
  in
    future := next;
    past := [];
    if next = [] then v
    else v @ withNondeterminism f
  end

fun choose xs =
  let val len = Vector.length xs in
    if len = 0 then raise Empty
    else case pop future of
        SOME idx =>
          (push past idx;
           Vector.sub(xs, (len - 1) - idx))
      | NONE =>
        let val idx = len - 1 in
          push past idx;
          Vector.sub(xs, 0)
        end
  end

fun fail () = choose (Vector.fromList [])

val n = valOf (Int.fromString (hd (CommandLine.arguments ())))

val board_range = Vector.tabulate(n, fn i => i)

fun okay i c []      = true
  | okay i c (x::xs) = c <> x andalso (c-x) <> i andalso (c-x) <> (~i) andalso okay (i+1) c xs
                   
fun enum_nqueens i l =
  if i = n then
    l
  else
    let val c = choose board_range in
      if not (okay 1 c l) then fail ()
      else enum_nqueens (i+1) (c :: l)
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
