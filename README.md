# paragraph
A configuration management toolkit (module) written in prolog, including:

* declaration of application or system parameters, for navigating resources located inside hierarchical containers (paramloc, paramval)
* scoping operators, for constraining searches or evaluations
* application archive analysis (jar, war, ear, zips)
* diff tools working on xml, json, or yaml documents or referencing parameters (diff DSL)
* coordination of search tasks using the coworkers module

The framework's purpose and examples is covered in this online [book](https://github.com/incodame/paragraph/blob/master/doc/book.org)

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
parent
parent_artifact_id
parent_group_id
parent_version
version
pom_xml
true.
```

```javascript
% Define search options

?- Opts = [ag('paragraph'), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)].

% Example of navigation: xpath -> xpath -> xml file -> warfile : find the parent pom artifact/version of the paragraph archives

?- paramv(parent_artifact_id, Val, $Opts, NewScoper).
Val = 'spring-boot-starter-parent',
NewScoper = [ae('META-INF/maven/org.incodame.paragraph/paragraph-ui/pom.xml'),
             ar(file("/opt/paragraph/ParagraphUI/target/paragraph-ui-0.0.1-SNAPSHOT.war")),
             ag(paragraph), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)]

?- paramv(parent_version, Val, $Opts, NewScoper).
Val = '2.3.0.RELEASE',
NewScoper = [ae('META-INF/maven/org.incodame.paragraph/paragraph-ui/pom.xml'),
             ar(file("/opt/paragraph/ParagraphUI/target/paragraph-ui-0.0.1-SNAPSHOT.war")),
             ag(paragraph), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)]
```

## paragraph configuration

paragraph relies on the configuration maintained and documented in paragraph.yml and in your own prolog scripts

example of definitions for the above pom_xml* parameters

```javascript
  graph:
    file:
      pom_xml:
        loc: applfile("pom.xml"), endswith("/pom.xml")
        doc: "Application or module's pom.xml"
        params:
          - parent: structure
          - dependency: structure
          - artifact_id: param
          - group_id: param
          - version: param
          - property: structure
          - build_plugin: structure
          - profile: structure
          - module: structure
    param:
      parent:
        loc: xpath(//project/parent)
        doc: "pom.xml parent"
        params:
          - parent_artifact_id: param(artifact_id)
          - parent_group_id: param(group_id)
          - parent_version: param(version)
      dependency:
        loc: xpath(//project/dependencies/dependency)
        doc: "pom.xml dependency"
        params:
          - dep_artifact_id: param(artifact_id)
          - dep_group_id: param(group_id)
          - dep_version: param(version)
      artifact_id:
        loc: xpath(//artifactId(text))
        doc: "artifactId"
      group_id:
        loc: xpath(//groupId(text))
        doc: "groupId"
      version:
        loc: xpath(//version(text))
        doc: "version"
      property:
        loc: xpath(//properties/*)
        doc: "Custom properties used in the build"
      build_plugin:
        loc: xpath(//build/plugins/plugin)
        doc: "Plugin used in the build lifecycle"
      profile:
        loc: xpath(//profiles/profile)
        doc: "Build profiles for different environments"
      module:
        loc: xpath(//modules/module)
        doc: "Sub module"
```
