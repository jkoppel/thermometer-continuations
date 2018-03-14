TARGETS=indirect.native \
  replay.native replay_dynarray.native replay_optimized.native \
  replay_zipper.native replay_zipper_array.native \
  thermometers_generic.native thermometers_optimized.native

all:
	@echo "/!\\ the delimcc and effect benchmark are not built by default"
	@echo "/!\\ as they require external runtime/libraries"
	@echo "/!\\ see 'make delimcc', 'make effect'"
	ocamlbuild $(TARGETS)

delimcc:
	@echo "/!\\ the delimcc benchmark requires the 'delimcc' library"
	@ocamlfind query delimcc > /dev/null
	@#delimcc does not work under OCaml 4.06
	@ocamlc -version | grep -v 4.06 > /dev/null \
	  || (echo "unsupported OCaml version $$(ocamlc -version)"; exit 1)
	ocamlbuild -use-ocamlfind -package delimcc filinski_delimcc.byte filinski_delimcc.native
	@echo "/!\\ the bytecode version (.byte) may be faster"
	@echo "/!\\ than the native one (.native), you should test both"

effect:
	@echo "/!\\ the effect-handling benchmark requires the experimental ocaml-multicore runtime"
	@opam switch show | grep multicore > /dev/null \
	  || (echo "/!\\ the current switch ($$(opam switch show)) lacks the multicore runtime; see https://github.com/ocamllabs/ocaml-effects-tutorial#setting-up"; \
	      exit 1)
	ocamlbuild effect_nondet.native

clean:
	ocamlbuild -classic-display -clean
