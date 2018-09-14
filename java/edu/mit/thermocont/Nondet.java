package edu.mit.thermocont;

import edu.mit.thermocont.monad.App;
import edu.mit.thermocont.monad.ListMonad;

import java.util.Arrays;
import java.util.List;
import java.util.function.Supplier;

public class Nondet {
    private RMonad<List, ListMonad> rm = new Represent<>(new ListMonad());

    public <A> A choose(List<A> as) {
        return rm.reflect(App.listToApp(as));
    }

    public <A> A choose(A... as) {
        return choose(Arrays.asList(as));
    }

    public <A> List<A> withNondeterminism(Supplier<A> f) {
        return App.appToList(rm.reify(f));
    }
}
