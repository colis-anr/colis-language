ARG tag=alpine
ARG image=ocaml/opam:$tag
FROM $image
MAINTAINER Nicolas “Niols” Jeannerod
WORKDIR /home/opam/workdir

## ======================== [ Install Dependencies ] ======================== ##

ADD *.opam .
RUN sudo chown -R opam .

## Install dependencies -- both external and OPAM -- for installation, but also
## for documentation and testing.
RUN opam pin add . --no-action
RUN for pkg in $(opam pin list --short); do \
  opam depext $pkg --with-doc --with-test; \
  opam install $pkg --deps-only --with-doc --with-test; \
  done

## ============================== [ Testing ] =============================== ##

## Get all the files from the project and give them the appropriate owner within
## the Docker image.
ADD . .
RUN sudo chown -R opam .

## Try to build the project, run its tests, build its documentation, install,
## uninstall and cleanup.
RUN opam exec -- make
RUN opam exec -- make test
RUN opam exec -- make doc
RUN opam exec -- make install
RUN opam exec -- make uninstall
RUN opam exec -- make clean
