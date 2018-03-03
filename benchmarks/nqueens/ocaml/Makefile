OCAMLBUILD=ocamlbuild -use-ocamlfind

all:
	@echo "/!\\ the delimcc benchmark is not build by default"
	@echo "/!\\ as it requires the delimcc library"
	@echo "/!\\ add 'make delimcc' to build it"
	$(OCAMLBUILD) delim_nondet.native direct.native replay_nondet.native

delimcc:
	@echo "/!\\ the delimcc benchmark requires the 'delimcc' library"
	@ocamlfind query delimcc > /dev/null
	@#delimcc does not work under OCaml 4.06
	@ocamlc -version | grep -v 4.06 > /dev/null \
	  || (echo "unsupported OCaml version $$(ocamlc -version)"; exit 1)
	$(OCAMLBUILD) delimcc_nondet.byte delimcc_nondet.native
	@echo "/!\\ the bytecode version (.byte) may be faster"
	@echo "/!\\ than the native one (.native), you should test both"

clean:
	$(OCAMLBUILD) -classic-display -clean
