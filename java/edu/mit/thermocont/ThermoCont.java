package edu.mit.thermocont;

import java.util.Collections;
import java.util.Stack;
import java.util.function.Function;
import java.util.function.Supplier;

/*
 * This implementation is translated directly from the one in the paper.
 * Does more stack-cloning as a result than it ought to; didn't think
 * very hard about this.
 */

public class ThermoCont<Ans> implements Control<Ans> {
    private interface Frame {}
    private static class Return implements Frame {
        Object val;
        public Return(Object val) { this.val = val; }
    }
    private static class Enter implements Frame {}

    private class ResetState {
        private Supplier<Ans> block;
        private Stack<Frame> past;
        private Stack<Frame> future;

        public ResetState(Supplier<Ans> block, Stack<Frame> past, Stack<Frame> future) {
            this.block = block;
            this.past = past;
            this.future = future;
        }
    }

    private static class Done extends RuntimeException {
        private Object ans;

        public Done(Object ans) {
            this.ans = ans;
        }
    }

    private Stack<Frame> past = new Stack<>();
    private Stack<Frame> future = new Stack<>();
    private Stack<ResetState> nest = new Stack<>();

    private Supplier<Ans> curExpr = null;

    private static <E> Stack<E> cloneStack(Stack<E> s) {
        return (Stack<E>)s.clone();
    }

    private Ans runWithFuture(Supplier<Ans> f, Stack<Frame> fFuture) {
        nest.push(new ResetState(curExpr, past, future));
        past = new Stack<>();
        future = cloneStack(fFuture);
        curExpr = f;

        Ans result;
        try {
            result = f.get();
        } catch (Done d) {
            result = (Ans)d.ans;
        }

        ResetState prev = nest.pop();
        curExpr = prev.block;
        past = prev.past;
        future = prev.future;

        return result;
    }


    @Override
    public Ans reset(Supplier<Ans> block) {
        return runWithFuture(block, new Stack<>());
    }

    private <E> E tryPop(Stack<E> s) {
        if (s.empty()) {
            return null;
        } else {
            return s.pop();
        }
    }

    @Override
    public <A> A shift(Function<Function<A, Ans>, Ans> f) {
        Frame fr = tryPop(future);
        if (fr instanceof Return) {
            past.push(fr);
            return (A)((Return) fr).val;
        } else if (fr == null || fr instanceof Enter) {
            Stack<Frame> newFuture = cloneStack(past);
            Collections.reverse(newFuture);
            Supplier<Ans> ourExpr = curExpr;
            Function<A, Ans> k = (v) -> {
                Stack<Frame> fut = cloneStack(newFuture);
                fut.add(0, new Return(v));
                return runWithFuture(ourExpr, fut);
            };
            past.push(new Enter());
            Ans result = f.apply(k);
            throw new Done(result);
        } else {
            throw new IllegalArgumentException();
        }
    }
}
