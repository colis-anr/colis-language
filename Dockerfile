## =============================== [ Config ] =============================== ##

ARG TAG=latest
ARG IMAGE=ocaml/opam2:$TAG
ARG SWITCH=

## ============================= [ Base image ] ============================= ##

FROM $IMAGE
MAINTAINER Nicolas Jeannerod

RUN [ -z "$SWITCH" ] || opam switch create "$SWITCH"

WORKDIR /home/opam/workdir

## ============================ [ Dependencies ] ============================ ##

## FIXME: there should be a better way
RUN opam pin -n git+https://github.com/colis-anr/morsmall

COPY *.opam .
RUN opam depext -i $(opam show . -f depends: | cut -d '"' -f 2)

## =============================== [ Build ] ================================ ##

COPY . .
RUN sudo chown -R opam .
RUN eval $(opam env) && make

## =============================== [ Check ] ================================ ##

## FIXME: replay proofs
RUN eval $(opam env) && make test
