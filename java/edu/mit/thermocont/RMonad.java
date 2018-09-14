package edu.mit.thermocont;

import edu.mit.thermocont.monad.App;
import edu.mit.thermocont.monad.Monad;

import java.util.function.Supplier;

public interface RMonad<F, M extends Monad<F>> {
    public <A> A reflect(App<F, A> x);
    public <A> App<F, A> reify(Supplier<A> f);
}
