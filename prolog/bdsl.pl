%% Example of BDsl definitions for a Java web application built with Maven and deployed on Tomcat.
%% these facts are generated from the pavements definitions but can also be written by hand in the paragraph REPL
%%   and from the application deployment context specified in paramv
%% their structure is of the form:  
%%   container_term . p(path_list)                   for specifying where to locate a container in the application structure
%%   container_term -o param_term(param_props_list)  for specifying parameters associated to a container

:- op(500, xfy, '.').
:- op(400, xfy, '-+').
:- discontiguous '.'/1.
:- discontiguous '-+'/1.
:- discontiguous p/1.
:- discontiguous d/2.
:- discontiguous z/2.

f('pom.xml') . p([ c, dev, git, app ]).
d('src', [ '-- main',
           '---- java',
           '-- test',
           '---- java'
         ]) . p([ c, dev, git, app ]).

z('app.jar', [ f('pom.xml'),
               d('-- com/example', 
                  [ 'a.class',
                    'b.class'
                  ])
             ]) . p([ 'webapps', 'WEB-INF', 'lib' ]).

z('lib.jar', [ f('pom.xml') | _]) . p([ 'webapps', 'WEB-INF', 'lib' ]).

p([ webapps | _]) . p([ opt, incodam, tomcat ]).

f('pom.xml') -+ s([name=parent_pom, loc=xpath('//project/parent'), doc="pom.xml parent"]).

s([name=parent_pom | _]) -+ i([name=parent_version, loc=xpath('//version(text)'), doc="parent pom version"]).

d(app, _) -+ f('readme.md').

f('readme.md') -+ i([name=help_url, loc=regexp("[(](?<V1L>.*)[)]$"), doc="help resource url"]).
      