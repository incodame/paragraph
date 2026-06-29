# First time:
```
docker run -it --rm -v $(realpath .):/opt/paragraph -v $(realpath .)/../pavements:/opt/pavements -v /tmp/paragraph:/tmp/paragraph --env PARAGRAPH_HOME=/opt/paragraph --env PARAGRAPH_TEMP=/tmp/paragraph --env PAVEMENTS_LIBRARY=/opt/pavements/library incodame/swipy
```

NOTE 1 - The docker image "swipy" can be built using the Dockerfile at https://github.com/incodame/swipy 
NOTE 2 - replace $(realpath .) with the actual OS path after cloning paragraph on your system

## configure a working session with:
```
?- consult('/opt/paragraph/prolog/paragraph_conf.pl').
?- paragraph_setup.
```

The latest command imports some python and prolog packages which are not part of the standard distribution for python / swi-prolog.

## load some BDSL definitions with:
```
?- consult('/opt/paragraph/prolog/bdsl_lib/nodeproj.bdsl.pl').
```

## start a working session with:
```
?- consult('/opt/paragraph/prolog/paragraph.pl').
```

## load and execute a test suite such as doc_t with:
```
?- consult('/opt/paragraph/t/doc_t.plt').
?- run_tests.
```
## leave session
type CTRL+D

# Next time:
```
docker start -ai $(docker container ls -a | grep swipl | head -1 | awk '{print $1}')
```

perform the previous consults by typing the UP-arrow 
