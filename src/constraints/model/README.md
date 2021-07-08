Experiments can be performed in a docker container using the following steps:
<br />
- **build the docker:** obtain the docker image of the git-repository of colis-language using ``docker build -t [Image-name] .`` in the directory with the dockerfile;
 the image uploaded includes ocaml (based on opam package "ocaml/opam2:latest") which runs on "Debian GNU/Linux 10 (buster)";

- **run a container:** after the image is build create a a container of it to run a terminal using ``docker run -it [Image-name]``

- **compile the test engine:** in the directory 
    ``colis-language/src/constraints/model`` (called MDIR), 
    execute ``dune build engine.exe``;
    the mutation and verbose modes are toggled by
    Boolean flags in ``engine.ml``;
    
- **run the test for one command:** in the MDIR,
    write the command in the ``cmd.dat`` file;
    the test is run by calling ``dune exec ./engine.exe``;
    multiple commands may be tested by writing them, one by line,
    in the ``cmd.dat`` file.
