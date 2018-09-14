package edu.mit.thermocont.monad;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

/*
 * All code in this folder is adapted from the RPGA Framework
 * ("Ridiculously Generic Program Analysis"), an unfinished project
 * of James Koppel that could automatically turn "normally"-written
 * interpreters into compilers and many kinds of analyzers for a language.
 * The suggestion that it be done in Java "for accessibility" turned the project
 * largely into an exercise in how to do higher-order typed programming
 * in Java.
 *
 * James Koppel discovered the first techniques for using replay
 * to do direct-style monads while working on this framework, and
 * eventually generalized them to thermometer continuations.
 */

public class ListMonad implements Monad<List> {
    public <E> App<List, E> ret(E e) {
        return App.listToApp(Collections.singletonList(e));
    }

    public <E,F> App<List,F> bind(App<List, E> m, Function<E, App<List, F>> f) {
        List<F> result = new ArrayList<>();

        for (E e : App.appToList(m)) {
            result.addAll(App.appToList(f.apply(e)));
        }

        return App.listToApp(result);
    }
}
