%%
%% Example of BDsl definitions for a Java web application built with Maven and deployed on Tomcat.
%%
%% these facts are generated from the pavements definitions  
%%   and from the application deployment context specified in paramv
%% they can also be input by hand in the paragraph REPL
%% their structure is of the form:  
%%   container_term :> p(path_list)                  for specifying where to locate a container in the application structure
%%   container_term -+ param_term(param_props_list)  for specifying parameters associated to a container
%% the container term can use the _ wildcard to indicate any container matching the rest of the term
%% this approach of connecting terms with operators allows to easily express relationships between containers and parameters, without duplicating information

%% Example of query: navigate from a structured parameter to its location in the application structure
%% ?- s([name=Sname | _]) -+ i([name=parent_version | Rest]), C -+ s([name=Sname | SRest]), C :> p(Path).
%% Sname = parent_pom,
%% Rest = [loc=xpath('//version(text)'), doc="parent pom version"],
%% C = f('pom.xml'),
%% SRest = [loc=xpath('//project/parent'), doc="pom.xml parent"],
%% Path = [c, dev, git, app] .

:- op(500, xfy, ':>').
:- op(400, xfy, '-+').
:- discontiguous ':>'/1.
:- discontiguous '-+'/1.
:- discontiguous p/1.  % path specification
:- discontiguous d/2.  % directory specification
:- discontiguous z/2.  % zip/jar specification
:- discontiguous f/1.  % file specification
:- discontiguous s/1.  % structured parameter specification
:- discontiguous i/1.  % individual parameter specification    

%% application structure definition in development environment
f('pom.xml') :> p([ c, dev, git, app ]).

%% subdirectory structure definition in development environment
d('src', [ '-- main',
           '---- java',
           '-- test',
           '---- java'
         ]) :> p([ c, dev, git, app ]).

%% archive structure definition and location in deployment environment
z('app.jar', [ f('pom.xml'),
               d('-- com/example', 
                  [ 'a.class',
                    'b.class' 
                    | _
                  ])
             ]) :> p([ 'webapps', 'WEB-INF', 'lib' ]).

z('lib.jar', [ f('pom.xml') | _]) :> p([ 'webapps', 'WEB-INF', 'lib' ]).

f('a.css') :> p([ 'app.jar', 'web', 'resources', 'css' ]).

%% based on the search context, relative paths can be completed to absolute paths using wilcards
p([ webapps | _]) :> p([ opt, incodam, tomcat ]).

%% container and related parameter definitions
f('pom.xml') -+ s([name=parent_pom, loc=xpath('//project/parent'), doc="pom.xml parent"]).
f('readme.md') -+ i([name=help_url, loc=regexp("[(](?<V1L>.*)[)]$"), doc="help resource url"]).

%% structured parameter details
s([name=parent_pom | _]) -+ i([name=parent_version, loc=xpath('//version(text)'), doc="parent pom version"]).

%% container and sub-container relationships
d(app, _) -+ f('readme.md').
z('app.jar', _) -+ f('pom.xml').
      
%% f('pom.xml') is a synonym for f([name='pom.xml' | _])
f(Filename) :- f([name=Filename | _]).
z(Zipname, Contents) :- z([name=Zipname | _], Contents).
d(Dirname, Contents) :- d([name=Dirname | _], Contents).
s(Structname) :- s([name=Structname | _]).
i(Infoname) :- i([name=Infoname | _]).

% end of bdsl.pl
