OCAMLBUILD=ocamlbuild -use-ocamlfind

all:
	@echo "/!\\ the delimcc benchmark is not build by default"
	@echo "/!\\ as it requires the delimcc library"
	@echo "/!\\ add 'make delimcc' to build it"
	$(OCAMLBUILD) indirect.native replay.native thermometers.native

delimcc:
	@echo "/!\\ the delimcc benchmark requires the 'delimcc' library"
	@ocamlfind query delimcc > /dev/null
	@#delimcc does not work under OCaml 4.06
	@ocamlc -version | grep -v 4.06 > /dev/null \
	  || (echo "unsupported OCaml version $$(ocamlc -version)"; exit 1)
	$(OCAMLBUILD) delimcc.byte delimcc.native
	@echo "/!\\ the bytecode version (.byte) may be faster"
	@echo "/!\\ than the native one (.native), you should test both"

effect:
	@echo "/!\\ the effect-handling benchmark requires the experimental ocaml-multicore runtime"
	opam switch show | grep multicore > /dev/null \
	  || (echo "missing multicore runtime; see https://github.com/ocamllabs/ocaml-effects-tutorial#setting-up"; \
	      exit 1)
	$(OCAMLBUILD) effect_nondet.native

clean:
	$(OCAMLBUILD) -classic-display -clean
