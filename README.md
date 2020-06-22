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

## getting started with some analysis

```javascript
?- showdoc(paramval/4).
paramval(Parameter, Version, ParameterValue, ScopeOptions)
  A Parameter is configured in a container, via the paragraph:paramloc/4 predicate
    using a search specification like regexp() or xpath() etc...
  * ParameterValue will be extracted from the container
  * ScopeOptions: depending on the parameter, ag(ApplicationGroup), ve(Version), env(Environment)
  See also: parameters/2
true.
```

```javascript
?- parameters.
batch_java_home
context_root
pom_xml_parent
pom_xml_parent_artifact_id
pom_xml_parent_group_id
pom_xml_parent_version
pom_xml_version
war_pom_xml
true.
```

```javascript
?- paramval(pom_xml_version, Ve, Val, [ag(paragraph), ve('0.0.1-SNAPSHOT')]).
Reading archive /tmp/paragraph/paragraph-ui-0.0.1-SNAPSHOT.war: META-INF/maven/org.incodame.paragraph.sample.webapp/paragraph-ui/pom.xml
Ve = Val, Val = '0.0.1-SNAPSHOT' ;
false.

?- paramval(pom_xml_parent_artifact_id, Ve, Val, [ag(paragraph), ve('0.0.1-SNAPSHOT')]).
Reading archive /tmp/paragraph/paragraph-ui-0.0.1-SNAPSHOT.war: META-INF/maven/org.incodame.paragraph.sample.webapp/paragraph-ui/pom.xml
Ve = '0.0.1-SNAPSHOT',
Val = 'spring-boot-starter-parent' .

?- paramval(pom_xml_parent_artifact_id, App, Ve, 'spring-boot-starter-parent', [ag(paragraph), ve('0.0.1-SNAPSHOT')]).
Reading archive /tmp/paragraph/paragraph-ui-0.0.1-SNAPSHOT.war: META-INF/maven/org.incodame.paragraph.sample.webapp/paragraph-ui/pom.xml
App = 'paragraph-ui',
Ve = '0.0.1-SNAPSHOT' ;
false.
```

## paragraph configuration

paragraph relies on the configuration maintained and documented in prolog/paragraph_conf.pl

example of definitions for the above pom_xml* parameters

```javascript
paramloc(pom_xml_parent,              war_pom_xml,        xpath(//project/parent),   [ doc("pom.xml parent") ]).
paramloc(pom_xml_parent_artifact_id,  pom_xml_parent,     xpath(//artifactId(text)), [ doc("pom.xml parent artifactId") ]).
paramloc(pom_xml_parent_group_id,     pom_xml_parent,     xpath(//groupdId(text)),   [ doc("pom.xml parent groupdId") ]).
paramloc(pom_xml_parent_version,      pom_xml_parent,     xpath(//version(text)),    [ doc("pom.xml parent version") ]).
paramloc(pom_xml_version,             war_pom_xml,        xpath(//project/version(text)), [ doc("pom.xml version") ]).
paramloc(war_pom_xml,                 WebAppWar,          endswith("/pom.xml"),      [ doc(Doc) ]) :-
    app_archive(war, _App, WebAppWar, _),
    Doc = "pom.xml of a Web Archive built by Maven".

%% application archives ordered alphabetically (ear, war, jar, zip)

app_archive(war,  'paragraph-ui',           'paragraph-ui-(version).war', []).
```
