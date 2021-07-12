
Package for model generation and specification testing

Documentation is available in 'doc/code.tex'.

Experiments can be performed in a docker container using
the follozing steps:

0. Install docker
'''''''''''''''''
Use instructions from https://docs.docker.com/engine/install

1. Build the docker image
'''''''''''''''''''''''''
In the directory colis-language.git, execute:

sudo docker build -t [image-tag] .

which uses 'Dockerfile' to create an image from colis-language repository
based on 'ocaml/opam2:latest'. This may take a while.

2. Run a docker image (container)
'''''''''''''''''''''''''''''''''
Use the command

sudo docker run -it [image-tag]

3. Compile the test engine
''''''''''''''''''''''''''
Move in the container in the directory colis-language/src/constraints/model
(called from now MDIR)

cd colis-language/src/constraints/model

Compile the engine using

dune build engine.exe
    
4. Test one command
'''''''''''''''''''

Create a file 'cmd.dat' where the commad is in the first line.

Execute the test using

dune exec ./engine.exe [if_mutate] [if_verbose]

where the options are
  if_mutate     0 not to apply test mutation, by default
  if_verbose    1 print explanations, by default


To test multiple commands, write each command on a line of file 'cmd.dat'

