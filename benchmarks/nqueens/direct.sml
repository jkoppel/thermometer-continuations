fun range start len =
  if start >= len
  then []
  else start :: (range (start+1) len)

fun okay i c []      = true
  | okay i c (x::xs) = c <> x andalso (c-x) <> i andalso (c-x) <> (~i) andalso okay (i+1) c xs
                   
fun enum_nqueens n i l =
  if i = n then
    [l]
  else
    foldl (fn (c, k) =>
              if okay 1 c l then k @ enum_nqueens n (i+1) (c :: l) else k)
          []
          (range 0 n)


open Timer
open Time

val timer = startCPUTimer ()
val n = valOf (Int.fromString (hd (CommandLine.arguments ())));
val res = List.length (enum_nqueens n 0 [])
val times = let val {usr : time, sys : time } = checkCPUTimer timer in
                (toMilliseconds sys,
                 toMilliseconds usr)
            end 


val _ = (print (LargeInt.toString (#2 times)); OS.Process.exit(OS.Process.success))
