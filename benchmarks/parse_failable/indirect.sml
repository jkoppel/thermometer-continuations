
fun create_list_helper i k n = if i = n then
                                   []
                               else if (i+1) mod k = 0 then
                                   "invalid" :: create_list_helper (i+1) k n
                               else
                                   Int.toString i :: create_list_helper (i+1) k n

local
    fun parse_int_helper 0 s = SOME 0
      | parse_int_helper i s = let val c = String.sub (s, i-1) in
                                   if c < #"0" orelse c > #"9" then
                                       NONE
                                   else
                                       case parse_int_helper (i-1) s of
                                           SOME x => SOME (10 * x + (Char.ord c - Char.ord #"0"))
                                        |  NONE   => NONE
                               end
in
  fun parse_int s = parse_int_helper (String.size s) s
end

val MODULUS = 1000000000
                                        
fun parse_sum []      = SOME 0
  | parse_sum (s::ss) = case parse_int s of
                            SOME x => (case parse_sum ss of
                                           SOME y => SOME ((x + y) mod MODULUS)
                                         | NONE   => NONE)
                         |  NONE   => NONE
                                          


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

val _ = (print (case x of
                    (SOME y) => Int.toString y 
                 |  NONE     => "None");
         print "\n")
val _ = (print (LargeInt.toString (#2 times)); OS.Process.exit(OS.Process.success))

                                          


