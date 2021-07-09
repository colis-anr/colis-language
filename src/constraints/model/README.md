To get a better insight of the modules and their functions check the latex file ```colis-language/src/constraints/model/doc/code.tex```
<br /><br />
Experiments can be performed in a docker container using the following steps:
<br />
- **build the docker:** obtain the docker image of the git-repository of colis-language using ``docker build -t [Image-name] .`` in the directory with the dockerfile;
 the image uploaded includes ocaml (based on opam package "ocaml/opam2:latest") which runs on "Debian GNU/Linux 10 (buster)";

- **run a container:** after the image is build create a a container of it to run a terminal using ``docker run -it [Image-name]``

- **compile the test engine:** in the directory 
    ``colis-language/src/constraints/model`` (called MDIR), 
    execute ``dune build engine.exe``;
    
- **run the test for one command:** in the MDIR,
    write the command in the ``cmd.dat`` file;
    the test is run by calling ``dune exec ./engine.exe [if_mutate] [if_verbose]``;
    the mutation and verbose modes can be specified in the arguments (0 for false and 1 for true);
    multiple commands may be tested by writing them, one by line,
    in the ``cmd.dat`` file.
