## ============================== [ Basis ] ================================= ##

ARG TAG=latest
ARG IMAGE=ocaml/opam2:$TAG

FROM $IMAGE as basis

ENV LANGUAGEDIR=/home/opam/colis-language
ENV PROVERSDIR=/home/opam/provers

ENV Z3="z3-4.8.7"
ENV CVC4="cvc4-1.6"
ENV ALTERGO="alt-ergo.2.3.0"

ARG SWITCH=
RUN [ -z "$SWITCH" ] || opam switch create "$SWITCH"

WORKDIR /home/opam/opam-repository
RUN git pull && opam update

RUN sudo apt-get update

## ======================= [ Basis with dependencies ] ====================== ##

FROM basis as basis-with-deps

RUN sudo apt-get install -qq -yy curl autoconf automake
RUN sudo apt-get install -qq -yy debianutils libgmp-dev m4 perl pkg-config zlib1g-dev

WORKDIR "$LANGUAGEDIR"
COPY colis-language.opam .
RUN sudo chown -R opam .

# Extract pin-depends from opam file and pin them
RUN opam show . -f pin-depends: 2>/dev/null \
  | tr -s '[]"' ' ' \
  | xargs -n2 opam pin -n

RUN opam install . --deps-only --with-test --with-doc

## ============================== [ Builder ] =============================== ##

FROM basis-with-deps as builder

COPY . .
RUN sudo chown -R opam .

RUN eval $(opam env) && make build

## =============================== [ Tester ] =============================== ##

FROM builder as tester

RUN eval $(opam env) && make doc && make test
RUN eval $(opam env) && make install && make uninstall
RUN eval $(opam env) && make clean

## ========================= [ Basis with provers ] ========================= ##

FROM basis as basis-with-provers

WORKDIR "$PROVERSDIR"
RUN sudo chown -R opam .

RUN opam depext -i "$ALTERGO"
RUN eval $(opam env) && cp "$(which alt-ergo)" "$ALTERGO"

RUN curl -sL -o "$CVC4" "http://cvc4.cs.stanford.edu/downloads/builds/x86_64-linux-opt/$CVC4-x86_64-linux-opt"
RUN chmod +x "$CVC4"

RUN curl -sL -o "$Z3.zip" "https://github.com/Z3Prover/z3/releases/download/$Z3/$Z3-x64-ubuntu-16.04.zip"
RUN unzip -q "$Z3.zip"
RUN cp "$Z3"-x64-ubuntu-16.04/bin/z3 "$Z3"

## =============================== [ Prover ] =============================== ##

FROM builder as prover
COPY --from=basis-with-provers "$PROVERSDIR" "$PROVERSDIR"

RUN eval $(opam env) && why3 config --add-prover alt-ergo "$ALTERGO" "$PROVERSDIR"/"$ALTERGO"
RUN eval $(opam env) && why3 config --add-prover cvc4 "$CVC4" "$PROVERSDIR"/"$CVC4"
RUN eval $(opam env) && why3 config --add-prover z3 "$Z3" "$PROVERSDIR"/"$Z3"

WORKDIR "$LANGUAGEDIR"
RUN eval $(opam env) && make replay-proofs
