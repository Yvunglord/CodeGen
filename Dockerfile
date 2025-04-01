FROM ocaml/opam:ubuntu-22.04-ocaml-4.14

WORKDIR /app
COPY . .

RUN opam install ppx_expect alcotest zanuda -y && \
    opam install . --deps-only -y && \
    eval $(opam env)

CMD ["dune", "build"]