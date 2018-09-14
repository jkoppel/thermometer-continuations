package edu.mit.thermocont.monad;

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

public interface Monad<M> {
    public <E> App<M, E> ret(E e);
    public <E,F> App<M,F> bind(App<M, E> m, Function<E, App<M, F>> f);
}
