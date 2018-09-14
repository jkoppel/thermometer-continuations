package edu.mit.thermocont;

import java.util.function.Function;
import java.util.function.Supplier;

public interface Control<Ans> {
    public Ans reset(Supplier<Ans> block);
    public <A> A shift(Function<Function<A, Ans>, Ans> f);
}
