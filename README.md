# paragraph
A configuration management toolkit (module) written in prolog, including:
* application archive analysis (jar, war, ear, zips)
* declaration of parameters for navigating resources inside hierarchical containers (paramloc, paramval)
* scoping operators for constraining searches or evaluations
* diff tools working on xml, json, or yaml resources (diff DSL)
* coordination of tasks delegated to verticles, using the coworkers module

## documentation system
```javascript
?- predicates.               % lists available predicates
application/3
application_batch/5
application_cluster/4
application_cluster/5
application_jar/5
...
```

```javascript
?- objects.                  % lists objects referenced by the available predicates
ApplicationGroup
ApplicationShortId
ArchiveFile
BatchCmd
Cluster
ClusterId
Consumer
Container
DocText
DownloadUrl
GitUrl
...
```

```javascript
?- showdoc(<predicate>)      % pretty prints a short documentation of the predicate
```

examples:

```javascript
?- showdoc(predicates_using/1).
predicates_using(Object)
  Lists all predicates which use an object listed in objects/0.
  example:  predicates_using(P,'GitUrl').

?- predicates_using(P,'GitUrl').
P = application_scm/4 ;
P = application_scm/5 ;
false.
```
