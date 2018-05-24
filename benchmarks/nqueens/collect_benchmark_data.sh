run() {
    printf "begin-benchmark\t$6\n"
    ruby $1 "$2" $3 $4 "$5"
    printf "end-benchmark\n\n"
}

run run_benchmark_time.rb "./ocaml/indirect.native"               10 13 "Indirect (OCaml)"                 IndirOCaml

run run_benchmark.rb      indirect.sml                            10 13 "Indirect (SML/NJ)"                IndirSML

run run_benchmark_time.rb "./ocaml/replay_zipper.native"          10 13 "Replay (OCaml)"                   ReplayOCaml

run run_benchmark.rb      replay.sml                              10 13 "Replay (SML/NJ)"                  ReplaySML

run run_benchmark_time.rb "./ocaml/thermometers_generic.native"   10 13 "Therm. (OCaml)"                   ThermOCaml

run run_benchmark.rb      thermometers.sml                        10 13 "Therm. (SML/NJ)"                  ThermSML

run run_benchmark_time.rb "./ocaml/thermometers_optimized.native" 10 13 "Therm. Opt. (OCaml)"              OptThermOCaml

run run_benchmark.rb      thermometers_optimized.sml              10 13 "Therm. Opt. (SML/NJ)"             OptThermSML

run run_benchmark_time.rb "./ocaml/filinski_delimcc.native"       10 13 "Filinski (Delimcc, OCaml)"        FilOCaml
# note: with the faster-nqueens implementation, Native delimcc beats Bytecode delimcc again

run run_benchmark.rb      filinski_callcc.sml                     10 13 "Filinski (Call/cc, SML/NJ)"       FilSML

run run_benchmark_time.rb "./ocaml/effect.native"                 10 13 "Eff. Handlers (Multicore OCaml)"  EffOCaml

#run run_benchmark_time.rb "sh prolog.sh"                          10 12 "Prolog search (SWI-Prolog)"       SWIProlog
# SWI Prolog is sensibly slower than the GNU Prolog results below, disabled
# profiling shows that most of the time is spent in the arithmetic comparison =/= in okay,
# which may come from having only boxed integers.

run run_benchmark_time.rb "sh gprolog.sh"                         10 13 "Prolog search (GProlog)"          Prolog

if test "$USE_MLTON" = "true"
then
    run run_benchmark_time.rb "./indirect"                        10 13 "Indirect"                         IndirMLton
    run run_benchmark_time.rb "./replay_zipper"                   10 13 "Replay"                           ReplayMLton
    run run_benchmark_time.rb "./filinski_callcc_derived_universal" 10 13 "Finlinski (Call/cc)"            FilMLton
fi
