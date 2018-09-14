package edu.mit.thermocont;

public class Examples {
    public static void main(String[] args) {
        Control<Integer> c = new ThermoCont<>();


        System.out.println(1 + c.reset(() -> {
           int x = c.shift((k) -> k.apply(k.apply(5)));
           return x * 2;
        }));

        System.out.println(1 + c.reset(() -> {
            int x = c.shift((k) -> {
                int y = c.shift((l) ->
                        l.apply(k.apply(10)));
                return 3 * y;
            });
            return 2 + x;
        }));


        Nondet n = new Nondet();
        System.out.println(
                n.withNondeterminism(() ->
                    n.choose(3, 4) * n.choose(5, 6)));
    }
}
