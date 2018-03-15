run() {
    printf "begin-benchmark\t$5\n"
    ruby $1 "$2" "$3" "$4"
    printf "end-benchmark\n\n"
}

run run_benchmark_time.rb "./ocaml/indirect.native"               12 "Indirect (OCaml)"                 IndirOCaml

run run_benchmark.rb      indirect.sml                            12 "Indirect (SML/NJ)"                IndirSML

run run_benchmark_time.rb "./ocaml/replay_zipper_array.native"    12 "Replay (OCaml)"                   ReplayOCaml

run run_benchmark.rb      replay_zipper_vector.sml                11 "Replay (SML/NJ)"                  ReplaySML
# note: the Vector version timeouts for N=12, so I manually replaced this result with replay_zipper.sml's in the output
# (the result is 307090ms on my machine, about 5m30s)

run run_benchmark_time.rb "./ocaml/thermometers_generic.native"   12 "Therm. (OCaml)"                   ThermOCaml

run run_benchmark.rb      thermometers.sml                        12 "Therm. (SML/NJ)"                  ThermSML

run run_benchmark_time.rb "./ocaml/thermometers_optimized.native" 12 "Therm. Opt. (OCaml)"              OptThermOCaml

run run_benchmark.rb      thermometers_optimized.sml              12 "Therm. Opt. (SML/NJ)"             OptThermSML

run run_benchmark_time.rb "./ocaml/filinski_delimcc.byte"         12 "Filinski (Delimcc, OCaml)"        FilOCaml

run run_benchmark.rb      filinski_callcc.sml                     12 "Filinski (Call/cc, SML/NJ)"       FilSML

run run_benchmark_time.rb "./ocaml/effect.native"                 12 "Eff. Handlers (Multicore OCaml)"  EffOCaml

run run_benchmark_time.rb "sh prolog.sh"                          12 "Prolog search (SWI-Prolog)"       Prolog
