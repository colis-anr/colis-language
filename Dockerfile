## =============================== [ Basis ] =============================== ##

ARG TAG=latest
ARG IMAGE=ocaml/opam2:$TAG

FROM $IMAGE

MAINTAINER Nicolas Jeannerod

ENV OPAMFILE=colis-language.opam
ARG MAKEARG=
ARG SWITCH=

RUN [ -z "$SWITCH" ] || opam switch create "$SWITCH"

## ============================ [ Dependencies ] ============================ ##

RUN sudo apt-get update && sudo apt-get install -yy -qq wget autoconf automake

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
RUN opam show . -f depends: \
  | cut -d '"' -f 2 \
  | xargs opam depext --install

## =============================== [ Build ] ================================ ##

WORKDIR /home/opam/colis-language
COPY . .
RUN sudo chown -R opam .

RUN eval $(opam env) && make build
