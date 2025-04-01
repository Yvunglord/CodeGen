FROM ocaml/opam:ubuntu-22.04-ocaml-4.14

RUN sudo apt-get update && \
    sudo apt-get install -y \
    gcc-riscv64-linux-gnu \
    binutils-riscv64-linux-gnu \
    qemu-user \
    && sudo rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . .

RUN opam install ppx_expect alcotest zanuda -y && \
    opam install . --deps-only -y && \
    eval $(opam env)

CMD ["dune", "build"]