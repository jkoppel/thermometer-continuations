package edu.mit.thermocont;

import edu.mit.thermocont.monad.App;
import edu.mit.thermocont.monad.Monad;

import java.util.function.Supplier;

public class Represent<F, M extends Monad<F>> implements RMonad<F, M> {
    private Control<App<F, Object>> cont = new ThermoCont<>();
    private Monad<F> m;

    public Represent(Monad<F> m) {
        this.m = m;
    }

    @Override
    public <A> A reflect(App<F, A> x) {
        return cont.shift((k) -> m.bind(x, k));
    }

    @Override
    public <A> App<F, A> reify(Supplier<A> f) {
        return m.bind(cont.reset(() -> m.ret((Object)f.get())),
                (x) -> m.ret((A)x));
    }
}
