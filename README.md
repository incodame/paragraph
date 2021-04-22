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
?- showdoc(paramv/4).
paramv(Parameter, ParameterValue, ScopeOptions, NewScoper)
  A Parameter is configured in a container file or archive, via the graph defined in paragraph.yml, 
    using a search specification like regexp() or xpath() etc...
  * ParameterValue will be extracted from the container
  * ScopeOptions: depending on the parameter, ag(ApplicationGroup), ve(Version), env(Environment), ...
  * NewScoper: = ScopeOptions enriched with additional terms describing the location where ParameterValue was extracted 
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
% Define search options

?- Opts = [ag('paragraph'), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)].

% Example of navigation: xpath -> xpath -> xml file -> warfile : find the parent pom artifact/version of the paragraph archives

?- paramv(pom_xml_parent_artifact_id, Val, $Opts, NewScoper).
Val = 'spring-boot-starter-parent',
NewScoper = [ae('META-INF/maven/org.incodame.paragraph/paragraph-ui/pom.xml'),
             ar(file("/opt/paragraph/ParagraphUI/target/paragraph-ui-0.0.1-SNAPSHOT.war")),
             ag(paragraph), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)]

?- paramv(pom_xml_parent_version, Val, $Opts, NewScoper).
Val = '2.3.0.RELEASE',
NewScoper = [ae('META-INF/maven/org.incodame.paragraph/paragraph-ui/pom.xml'),
             ar(file("/opt/paragraph/ParagraphUI/target/paragraph-ui-0.0.1-SNAPSHOT.war")),
             ag(paragraph), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)]
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
