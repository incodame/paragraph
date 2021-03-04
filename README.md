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
application/4
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

paragraph relies on the configuration maintained and documented in paragraph.yml and in your own prolog scripts

example of definitions for the above pom_xml* parameters

```javascript
    file
      pom_xml:
        loc: applfile("pom.xml"), endswith("/pom.xml")
        doc: "Application or module's pom.xml"
        params:
          - pom_xml_parent: param
          - pom_xml_artifact_id: param
          - pom_xml_group_id: param
          - pom_xml_version: param
    param:
      pom_xml_parent:
        loc: xpath(//project/parent)
        doc: "pom.xml parent"
        params:
          - pom_xml_parent_artifact_id: param
          - pom_xml_parent_group_id: param
          - pom_xml_parent_version: param
      pom_xml_artifact_id:
        loc: xpath(//artifactId(text))
        doc: "pom.xml artifactId"
      pom_xml_group_id:
        loc: xpath(//groupdId(text))
        doc: "pom.xml groupdId"
      pom_xml_parent_artifact_id:
        loc: xpath(//artifactId(text))
        doc: "pom.xml parent artifactId"
      pom_xml_parent_group_id:
        loc: xpath(//groupdId(text))
        doc: "pom.xml parent groupdId"
      pom_xml_parent_version:
        loc: xpath(//version(text))
        doc: "pom.xml parent version"
      pom_xml_version:
        loc: xpath(//project/version(text))
        doc: "pom.xml version"
```
