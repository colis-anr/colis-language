ARG tag=alpine
ARG image=ocaml/opam:$tag
FROM $image
MAINTAINER Nicolas “Niols” Jeannerod
WORKDIR /home/opam/workdir

## ======================== [ Install Dependencies ] ======================== ##

RUN opam install dune

## Copy only the dune-project file. This is the file that contains the
## specification of dependencies and, therefore, only this file is necessary to
## install dependencies. Moreover, adding only this file allows to change other
## files in the project and re-run the dockerfiles while caching all the part
## that installs dependencies.
ADD dune-project .
RUN sudo chown -R opam .

## Generate OPAM files from dune-project.
RUN opam exec -- dune build || true
## FIXME: The caught failure of dune build is here to mitigate #4487.

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
