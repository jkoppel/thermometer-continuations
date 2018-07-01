type idx = int * int

fun next_idx (k, len) =
  if k + 1 = len then NONE
  else SOME (k + 1, len)



val past : idx list ref = ref []
val future : idx list ref = ref []

fun next_path [] = []
  | next_path (i :: is) =
      case next_idx i of
        SOME i' => i' :: is
      | NONE => next_path is

(*
fun withNondeterminism f =
  let val v = [f ()] handle Empty => []
      val next_future = List.rev (next_path (!past))
  in
      past := [];
      future := next_future;
      if !future = [] then v
      else v @ withNondeterminism f
end
*)


fun push stack x = (stack := x :: !stack)

val nest : (idx list * idx list) list ref = ref []
exception Impossible

fun withNondeterminism f =
  (* before running, save current !past and !future value *)
 (push nest (!past, !future);
  past := [];
  future := [];
  let val result = loop f [] in
    (* then restore them after the run *)
    case pop nest of
      NONE => raise Impossible
    | SOME (p, f) => (past := p;
                      future := f;
                      result)
  end)
