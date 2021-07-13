
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

To remove an image, stop all the containers using this image (see below)
and use the command :

sudo docker images

to list the existing images, and the command

sudo docker rmi xxxx

to remove the image of ID xxxx.


2. Run a docker container (from an image)
'''''''''''''''''''''''''''''''''''''''''
Use the command

sudo docker run -it [image-tag]

to obtain a terminal runing the image created.
To remove a comtainer, list all of them with

sudo docker container ls -a

and remove the ones with the id xxxx

sudo docker container rm xxxx


3. Compile the test engine
''''''''''''''''''''''''''
In the terminal corresponding to the container, go to the directory
colis-language/src/constraints/model
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

