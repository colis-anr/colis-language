## =============================== [ Basis ] =============================== ##

ARG TAG=latest
ARG IMAGE=ocaml/opam2:$TAG

FROM $IMAGE

MAINTAINER Nicolas Jeannerod

ENV OPAMFILE=colis-language.opam
ARG SWITCH=

RUN [ -z "$SWITCH" ] || opam switch create "$SWITCH"

## ============================ [ Dependencies ] ============================ ##

RUN sudo apt-get update
RUN sudo apt-get install -qq -yy wget autoconf automake
RUN sudo apt-get install -qq -yy debianutils libgmp-dev m4 perl pkg-config zlib1g-dev

WORKDIR /home/opam/colis-language
COPY colis-language.opam .
RUN sudo chown opam colis-language.opam

# The following and the `apt-get install` above could be accomplished by something like
# `opam install --only-deps` if it was supported...

# Extract pin-depends from opam file and pin them
RUN opam show . -f pin-depends: 2>/dev/null \
  | tr -s '[]"' ' ' \
  | xargs -n2 opam pin -n

# Extract dependencies from opam file and install their dependencies
RUN opam install . --deps-only --with-test --with-doc

## ========================= [ Solvers for Why3 ] =========================== ##

# All manually installed provers reside in this directory
WORKDIR /home/opam/provers
RUN sudo chown -R opam .

# Make sure that Why3 is installed
RUN opam depext -i why3
RUN eval $(opam env) \
  && why3 config --detect-provers

# Versions
ENV CVC4="cvc4-1.6"
ENV Z3="z3-4.6.0"
ENV ALTERGO="alt-ergo.2.2.0"

# Install CVC4 as provers/$CVC4
RUN eval $(opam env) \
  && wget -q -O "$CVC4" "http://cvc4.cs.stanford.edu/downloads/builds/x86_64-linux-opt/$CVC4-x86_64-linux-opt" \
  && chmod +x "$CVC4" \
  && why3 config --add-prover cvc4 "$CVC4" "$PWD/$CVC4"

# Install Z3 in provers $Z3
RUN eval $(opam env) \
  && wget -q -O "$Z3.zip" "https://github.com/Z3Prover/z3/releases/download/$Z3/$Z3-x64-ubuntu-16.04.zip" \
  && unzip "$Z3.zip" \
  && mv "$Z3-x64-ubuntu-16.04" "$Z3" \
  && why3 config --add-prover z3 "$Z3" "$PWD/$Z3/bin/z3"

# Install Eprover
RUN eval $(opam env) \
  && wget -q -O E.tgz "http://wwwlehre.dhbw-stuttgart.de/~sschulz/WORK/E_DOWNLOAD/V_2.2/E.tgz" \
  && tar xf E.tgz \
  && ( cd E && ./configure && make ) \
  && why3 config --add-prover eprover "Eprover-2.2" "$PWD/E/PROVER/eprover"

# Register Alt-ergo
RUN eval $(opam env) \
  && opam depext -i "$ALTERGO" \
  && why3 config --add-prover alt-ergo "$ALTERGO" "$(which alt-ergo)"

RUN eval $(opam env) \
  && why3 --list-provers

## =============================== [ Build ] ================================ ##

WORKDIR /home/opam/colis-language
COPY . .
RUN sudo chown -R opam .

RUN eval $(opam env) && make ci
