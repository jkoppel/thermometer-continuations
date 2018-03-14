fun range start len =
  if start >= len
  then []
  else start :: (range (start+1) len)

val n = valOf (Int.fromString (hd (CommandLine.arguments ())))

val board_range = range 0 n

fun okay i c []      = true
  | okay i c (x::xs) = c <> x andalso (c-x) <> i andalso (c-x) <> (~i) andalso okay (i+1) c xs
                   
fun enum_nqueens i l =
  if i = n then
    [l]
  else
    foldl (fn (c, k) =>
              if okay 1 c l then k @ enum_nqueens (i+1) (c :: l) else k)
          [] board_range

open Timer
open Time

val timer = startCPUTimer ()
val res = List.length (enum_nqueens 0 [])
val times = let val {usr : time, sys : time } = checkCPUTimer timer in
                (toMilliseconds sys,
                 toMilliseconds usr)
            end 

val _ = (
  print (Int.toString res); print "\n";
  print (LargeInt.toString (#2 times)); print "\n";
  OS.Process.exit(OS.Process.success)
)
