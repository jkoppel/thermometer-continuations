open List;

val MODULUS = 30000

fun parseDigits []      = []
  | parseDigits (c::cs) = if Char.isDigit c then
                              concat [ [([c], cs)]
                                     , map (fn (ds, cs') => (c :: ds, cs')) (parseDigits cs)
                                     ]
                              
                          else
                              []

                                  
fun parseNumber cs = map (fn (ds, cs') => (valOf (Int.fromString (String.implode ds)), cs')) (parseDigits cs)


                         
fun parseExpr []      = []
  | parseExpr cs = concat [ concat (map (fn (x,cs') => case cs' of
                                                           (#"+" :: cs'') => map (fn (y, cs''') => ((x + y) mod MODULUS, cs'''))
                                                                                 (parseTerm cs'')
                                                         | _ =>  [])
                                        (parseTerm cs))
                          , parseTerm cs
                          ]
and parseTerm []      = []
  | parseTerm cs = concat [ concat (map (fn (x,cs') => case cs' of
                                                           (#"*" :: cs'') => map (fn (y, cs''') => ((x * y) mod MODULUS, cs'''))
                                                                                 (parseFactor cs'')
                                                         | _ =>  [])
                                        (parseFactor cs))
                          , parseFactor cs
                          ]
and parseFactor [] = []
  | parseFactor cs = concat [ parseNumber cs
                            , case cs of
                                  (#"(" :: cs') => concat (map (fn (x, cs'') => case cs'' of
                                                                                    (#")" :: cs''') => [(x, cs''')]
                                                                                  | _               => [])
                                                               (parseExpr cs'))
                                | _ =>  []
                            ]



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
val x = parseExpr sl
val times = let val {usr : time, sys : time } = checkCPUTimer timer in
                (toMilliseconds sys,
                 toMilliseconds usr)
            end 

(* val _ = print (x ^ "\n")*)

val _ = (print (LargeInt.toString (#2 times)); OS.Process.exit(OS.Process.success))

                                          

