open List;

val (br_idx : int list ref) = ref [];
val pos = ref 0;

exception Empty;

fun decr (0::ns) = decr ns
  | decr (n::ns) = (n-1)::ns
  | decr []      = []

fun withNondeterminism f =
  let val v = [f()] handle Empty => [] in
      br_idx := decr (!br_idx);
      pos := length (!br_idx);
      case !br_idx of
          [] => v
       |  _  => v @ withNondeterminism f
  end

fun amb [] = raise Empty
  | amb xs = if not (0 = !pos) then
                 let val idxFromEnd = nth (!br_idx, !pos - 1) in
                     (pos := (!pos) - 1;
                      nth (xs, (length xs - 1) - idxFromEnd))
                 end
             else
                 (br_idx := (length xs - 1) :: !br_idx;
                  hd xs)

fun fail () = amb []

val rs = withNondeterminism (fn () => let val x = amb [1, 2]
                                        val y = amb [3, 4]
                                      in
                                        (x,y)
                                      end);

val rs' = withNondeterminism (fn () => amb [3,4] * amb [5, 6])

val ts = withNondeterminism (fn () => let val x = amb [3,4] * amb [5,7] in
                                      if x >= 20 then x
                                      else fail () end);
