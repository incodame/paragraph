# First time:
```
docker run -it --network host -v $(pwd):/opt/paragraph -v /tmp/paragraph:/tmp/paragraph --env PARAGRAPH_HOME=/opt/paragraph --env PARAGRAPH_TEMP=/tmp/paragraph --env IPADDRE=$(ipconfig getifaddr en0) swipl
```

## install:
```
?- pack_install(list_util).
?- pack_install(regex).
```

## start a working session with:
```
?- consult('/opt/paragraph/prolog/paragraph_conf.pl').
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
