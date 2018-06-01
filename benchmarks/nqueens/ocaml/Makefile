TARGETS=indirect.native \
  replay.native replay_optimized.native \
  replay_zipper.native replay_zipper_nested.native \
  thermometers_generic.native thermometers_optimized.native

DELIMCC_TARGETS=filinski_delimcc.byte filinski_delimcc.native

EFFECT_TARGETS=effect.native

all:
	@echo "/!\\ the delimcc and effect benchmark are not built by default"
	@echo "/!\\ as they require external runtime/libraries"
	@echo "/!\\ see 'make delimcc', 'make effect'"
	ocamlbuild $(TARGETS)

delimcc:
	@echo "/!\\ the delimcc benchmark requires the 'delimcc' library"
	@ocamlfind query delimcc > /dev/null
	ocamlbuild -use-ocamlfind -package delimcc $(DELIMCC_TARGETS)
	@echo "/!\\ the bytecode version (.byte) may be faster"
	@echo "/!\\ than the native one (.native), you should test both"

effect:
	@echo "/!\\ the effect-handling benchmark requires the experimental ocaml-multicore runtime"
	@opam switch show | grep multicore > /dev/null \
	  || (echo "/!\\ the current switch ($$(opam switch show)) lacks the multicore runtime; see https://github.com/ocamllabs/ocaml-effects-tutorial#setting-up"; \
	      exit 1)
	ocamlbuild $(EFFECT_TARGETS)

clean:
	rm $(TARGETS) $(DELIMCC_TARGETS) $(EFFECT_TARGETS)
	ocamlbuild -classic-display -clean
