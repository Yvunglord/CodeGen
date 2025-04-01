FROM ocaml/opam:ubuntu-22.04-ocaml-4.14
WORKDIR /app
COPY . .
RUN opam install ppx_expect -y
RUN opam install . --deps-only -y
RUN opam install angstrom
RUN opam install zanuda
RUN eval $(opam env)
CMD ["dune", "build"]
