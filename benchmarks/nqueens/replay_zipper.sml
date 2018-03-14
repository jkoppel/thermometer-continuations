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

fun choose [] = raise Empty
  | choose xs =
      case pop future of
          SOME idx =>
          (push past idx;
           nth (xs, (length xs - 1) - idx))
        | NONE =>
          let val idx = length xs - 1 in
            push past idx;
            hd xs
          end

fun fail () = choose []

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
