FROM ubuntu:18.04

WORKDIR /thermocont
ADD . /thermocont

RUN echo 'export USE_MLTON="true"' >> /root/.bashrc

RUN apt-get update \
    && apt-get install -y --no-install-recommends ruby=1:2.5.1 smlnj=110.79-4 libsmlnj-smlnj=110.79-4 ocaml=4.05.0-10ubuntu1 gprolog=1.4.5-4.1 opam=1.2.2-6 ocamlbuild=0.11.0-3build1 \
    && apt-get install -y --no-install-recommends m4=1.4.18-1 git=1:2.17.0-1ubuntu1 mlton=20130715-3 vim=2:8.0.1453-1ubuntu1 make=4.1-9.1ubuntu1 \
    && opam init \
    && opam install -y ocamlfind.1.8.0 \
    && opam install -y delimcc.2017.03.02 \
    && opam remote add multicore https://github.com/ocamllabs/multicore-opam.git \
    && opam switch 4.06.1+multicore \
    && opam switch system
RUN cd /thermocont/benchmarks/nqueens/ocaml \
    && make \
    && eval `opam config env ` \
    && make delimcc \
    && cd .. \
    && mlton indirect.sml \
    && mlton replay_zipper.sml \
    && mlton filinski_callcc_derived_universal.sml

CMD ["bash"]