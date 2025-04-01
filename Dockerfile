FROM ocaml/opam:ubuntu-22.04-ocaml-4.14

RUN sudo apt-get update && sudo apt-get install -y \
    gcc \
    make \
    nasm \
    && sudo rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY *.opam ./

RUN opam update && opam install -y \
    ppx_expect \
    alcotest \
    dune \
    && opam install . --deps-only -y

COPY . .

RUN eval $(opam env) && \
    dune build && \
    chmod +x ./main.exe

CMD ["dune", "runtest"]