package edu.mit.thermocont.monad;


import java.util.List;

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

public class App<F,X> {
    private Object val;

    protected App(Object val) {
        this.val = val;
    }

    public static <E> App<List, E> listToApp(List<E> l) {
        return new App<>(l);
    }

    public static <E> List<E> appToList(App<List, E> l) {
        return (List<E>)l.val;
    }
}