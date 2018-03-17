exception Done of int
exception Impossible
                      
val state : int option ref = ref NONE
val cur_expr : (unit -> int) ref = ref (fn () => raise Impossible)

                
fun shift f = case !state of
                  SOME x => x
                | NONE => let fun k x = (state := SOME x;
                                         (!cur_expr) ())
                              val result = f k
                          in
                              raise (Done result)
                          end

fun reset f = (cur_expr := f;
               (f () handle (Done x) => x))


val r1 = reset (fn () => 2 + 3 * shift (fn k => 5))

val r2 = reset (fn () => 1 + shift (fn k => (k 1) * (k 2) * (k 3)))
