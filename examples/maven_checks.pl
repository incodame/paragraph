:- use_module(paragraph_conf).
:- use_module(paragraph).
:- use_module(library(lists)).
:- use_module(library(regex)).
:- use_module(library(xpath)).

:- table module/2.
:- table module_pom_root/3.
:- table module_art/3.
:- table module_dep/3.
:- table module_plugin/3.

% Compare maven dependencies between projects

sources_directory(Project, SrcDir) :- directory_alias(Project, _, SrcDir).

% parent pom

parent_pom(Project, Pom) :-
    sources_directory(Project, SrcDir),
    format(atom(Pom), '~w/pom.xml', [SrcDir]),
    exists_file(Pom).

% sub-modules

module(Project, Module) :-
    parent_pom(Project, Pom),
    load_xml(Pom, PomRoot, _),
    xpath(PomRoot, //modules/module(text), Module).

module_pom(Project, Module, Pom) :-
    sources_directory(Project, SrcDir),
    module(Project, Module),
    format(atom(Pom), '~w/~w/pom.xml', [SrcDir, Module]),
    exists_file(Pom).

module_pom_root(Project, Module, PomRoot) :-
    module_pom(Project, Module, Pom),
    load_xml(Pom, PomRoot, _).

module_art(Project, Module, art(GrId, ArtId, Ve)) :-
    module_pom_root(Project, Module, PomRoot),
    xpath(PomRoot, //artifactId(text), ArtId),
    xpath(PomRoot, //parent/groupId(text), GrId),
    (  xpath(PomRoot, //version(text), Ve), !
     ; xpath(PomRoot, //parent/version(text), Ve)
    ).

% dependencies

module_dep(Project, Module, dep(GrId, ArtId, Ve)) :-
    module_pom_root(Project, Module, PomRoot),
    xpath(PomRoot, //dependencies/dependency, Dep),
    xpath(Dep, //groupId(text), GrId),
    xpath(Dep, //artifactId(text), ArtId),
    xpath(Dep, //version(text), Ve).

module_dep(Project, Module, dep(GrId, ArtId)) :-
    module_pom_root(Project, Module, PomRoot),
    xpath(PomRoot, //dependencies/dependency, Dep),
    xpath(Dep, //groupId(text), GrId),
    xpath(Dep, //artifactId(text), ArtId).

find_dep(Project, Module, Pattern, Dep) :-
    module_dep(Project, Module, dep(GrId, ArtId)),
    ArtId =~ Pattern/i,
    Dep = dep(GrId, ArtId).

find_dep(Project, Module, Pattern, Dep) :-
    module_dep(Project, Module, dep(GrId, ArtId, Ve)),
    ArtId =~ Pattern/i,
    Dep = dep(GrId, ArtId, Ve).

% find_intern_dep, find_extern_dep

find_intern_dep(Project, Module, Pattern, Dep) :-
    find_dep(Project, Module, Pattern, dep(GrId, ArtId)),
    module_art(Project, ArtId, art(GrId, ArtId, _)),
    Dep = dep(GrId, ArtId).

find_extern_dep(Project, Module, Pattern, Dep) :-
    find_dep(Project, Module, Pattern, dep(GrId, ArtId)),
    not(module_art(Project, ArtId, art(GrId, ArtId, _))),
    Dep = dep(GrId, ArtId).

find_extern_dep(Project, Module, Pattern, Dep) :-
    find_dep(Project, Module, Pattern, dep(GrId, ArtId, Ve)),
    not(module_art(Project, ArtId, art(GrId, ArtId, _))),
    Dep = dep(GrId, ArtId, Ve).

% plugins

module_plugin(Project, Module, plugin(GrId, ArtId, Ve)) :-
    module_pom_root(Project, Module, PomRoot),
    xpath(PomRoot, //build/plugins/plugin, Plug),
    xpath(Plug, //groupId(text), GrId),
    xpath(Plug, //artifactId(text), ArtId),
    xpath(Plug, //version(text), Ve).

module_plugin(Project, Module, plugin(GrId, ArtId)) :-
    module_pom_root(Project, Module, PomRoot),
    xpath(PomRoot, //build/plugins/plugin, Plug),
    xpath(Plug, //groupId(text), GrId),
    xpath(Plug, //artifactId(text), ArtId).

find_plugin(Project, Module, Pattern, Plug) :-
    module_plugin(Project, Module, plugin(GrId, ArtId, Ve)),
    ArtId =~ Pattern/i,
    Plug = plugin(GrId, ArtId, Ve).

find_plugin(Project, Module, Pattern, Plug) :-
    module_plugin(Project, Module, plugin(GrId, ArtId)),
    ArtId =~ Pattern/i,
    Plug = plugin(GrId, ArtId).
