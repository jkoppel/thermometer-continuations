FROM ubuntu:18.04

RUN echo 'export USE_MLTON="true"' >> /root/.bashrc

## Install the necessary Ubuntu packages

RUN apt-get update
RUN apt-get install -y --no-install-recommends ruby=1:2.5.1 smlnj=110.79-4 libsmlnj-smlnj=110.79-4 gprolog=1.4.5-4.1
RUN apt-get install -y --no-install-recommends m4=1.4.18-1 git=1:2.17.1-1ubuntu0.1 mlton=20130715-3 vim=2:8.0.1453-1ubuntu1 make=4.1-9.1ubuntu1
RUN apt-get install -y --no-install-recommends aspcud=1:1.9.4-1 opam=1.2.2-6
# Note: aspcud is an optional dependency of opam (a solver for package installation problems)
# that makes its dependency-solving more reliable.


## Install the OCaml switches

# The OCaml software we use are handled through OPAM, the OCaml-specific package manager,
# which allows for more flexible control over versions than using Ubuntu packages,
# and in particular lets us use several different compilers in parallel (the standard
# implementation, and the 'multicore' experimental variant with effect handlers)

# Install a switch for the standard 4.06.1 compiler, and base packages for it
RUN opam init --compiler=4.06.1
RUN opam switch 4.06.1 && eval $(opam config env) \
    && opam install -y ocamlbuild.0.12.0 ocamlfind.1.8.0

# Install delimcc
RUN opam switch 4.06.1 && eval $(opam config env) \
    && opam install -y delimcc.2018.03.16

# Install the experimental 'multicore' compiler in a 4.06.1+multicore switch
RUN opam remote add multicore https://github.com/ocamllabs/multicore-opam.git \
    && opam switch install 4.06.1+multicore
RUN opam switch 4.06.1+multicore && eval $(opam config env) \
    && opam install -y ocamlbuild.0.12.0 ocamlfind.1.8.0


## Copy the host system's benchmark code

# We intentionally delayed the addition of the working directory upto
# this point: whenever the directory state changes on the host system,
# we get a cache miss and every step after this one has to be replayed
# when rebuilding the Docker image. On the other hand,
# environment-setting commands above remain cached.
WORKDIR /thermocont
ADD benchmarks /thermocont/benchmarks
ADD ocaml /thermocont/ocaml
ADD sml /thermocont/sml

## Build the SML benchmarks

RUN cd /thermocont/benchmarks/nqueens \
    && mlton indirect.sml \
    && mlton replay_zipper.sml \
    && mlton filinski_callcc_derived_universal.sml

## Build the standard OCaml benchmarks

RUN cd /thermocont/benchmarks/nqueens/ocaml \
    && opam switch 4.06.1 && eval $(opam config env) \
    && make clean \
    && make all
# Note: "ADD benchmarks /thermocont" may copy over the Docker
# container some build artefacts coming from the host's repository (if
# it was used for testing and not in a clean state). The 'make clean'
# run above makes sure we start from a clean state.


## Build the delimcc-using OCaml benchmarks

RUN cd /thermocont/benchmarks/nqueens/ocaml \
    && opam switch 4.06.1 && eval $(opam config env) \
    && make delimcc


## Build the effect-handler OCaml benchmarks

RUN cd /thermocont/benchmarks/nqueens/ocaml \
    && opam switch 4.06.1+multicore && eval $(opam config env) \
    && make effect
