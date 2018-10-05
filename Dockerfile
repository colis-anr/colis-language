## Get an OPAM image. The tag can be changed by the `--build-arg`
## argument to the `docker build` command line. The full image name
## can also be changed by the same mean.

ARG tag=latest
ARG image=ocaml/opam2:$tag
FROM $image
MAINTAINER Nicolas Jeannerod

## Choose the right version of the OPAM switch. By default, we use the
## one coming from the dist image. But one can specify a specific
## switch with the `--build-arg`.

ARG switch=
RUN [ -z "$switch" ] || opam switch create "$switch"

## Install dependencies. `opam depext` installs (in a distribution
## independant way) first the non-opam dependencies that are required
## and then the OPAM packages.

RUN opam depext -i morbig why3

## [FIXME] Install Morsmall

RUN git clone https://github.com/colis-anr/morsmall
RUN cd morsmall && eval $(opam env) && make && make install

## Work in /home/opam/colis-language, copy all the file there with the
## right owner and group.

WORKDIR /home/opam/colis-language
ADD . .
RUN sudo chown -R opam .

## Build Colis

RUN eval $(opam env) && make
