:- module(paragraph_bdsl, [ ':>'/2, '-+'/2, p/1, pr/1,d/2, z/2, f/1, s/1, i/1, assert_bdsl/1, assert_bdsl/2, retract_bdsl/1, retract_bdsl_all/0, contains/4 ]).

:- op(500, xfy, ':>').
:- op(400, xfy, '-+').
:- dynamic ':>'/2.
:- discontiguous ':>'/2.
:- dynamic '-+'/2.
:- discontiguous '-+'/2.
:- dynamic p/1.
:- discontiguous p/1.  % path specification
:- dynamic pr/1.
:- discontiguous pr/1.  % path reference specification
:- dynamic d/2.
:- discontiguous d/2.  % directory specification
:- dynamic z/2.
:- discontiguous z/2.  % zip/jar specification
:- dynamic f/1.
:- discontiguous f/1.  % file specification
:- dynamic s/1.
:- discontiguous s/1.  % structured parameter specification
:- dynamic i/1.
:- discontiguous i/1.  % individual parameter specification 
:- dynamic bdsl_owner/2.

%% syntax helper
assert_bdsl(Facts) :-
    assert_bdsl(default, Facts).

assert_bdsl(_, []).
assert_bdsl(Source, [H|T]) :-
    (   bdsl_owner(Source, H)
    ->  true
    ;   (   bdsl_owner(_, H)
        ->  true
        ;   assertz(H)
        ),
        assertz(bdsl_owner(Source, H))
    ),
    assert_bdsl(Source, T).

retract_bdsl(Source) :-
    retract(bdsl_owner(Source, Fact)),
    (   bdsl_owner(_, Fact)
    ->  true
    ;   retractall(Fact)
    ),
    fail.
retract_bdsl(_).

retract_bdsl_all :-
    findall(Source, bdsl_owner(Source, _), SourceList),
    sort(SourceList, Sources),
    retract_bdsl_sources(Sources).

retract_bdsl_sources([]).
retract_bdsl_sources([Source|Sources]) :-
    retract_bdsl(Source),
    retract_bdsl_sources(Sources).

%% derived relationships
contains(Container, s, InfoName, i) :-
    s(Container) -+ i([name = InfoName | _]).
  
contains(Container, f, InfoName, i) :-
    f(Container) -+ i([name = InfoName | _]).
  
contains(Container, f, StructName, s) :-
    f(Container) -+ s([name = StructName | _]).

contains(Container, d, FileName, f, PathList) :-
    f(FileName) :> p(PathList),
    reverse(PathList, [Container|_]).

contains(Container, d, FileName, f, PathList) :-
    f(FileName) :> pr(PathRef),
    pr(PathRef) :> p(PathList),
    reverse(PathList, [Container|_]).
    
contains(Container, d, FileName, f, Path) :-
    d(Container, Contents) :> p(Path),
    member(FileName, Contents), % TODO: specific predicate to extract sub path 
    f(FileName) :> p(Path).
  
contains(Container, z, FileName, f, Path) :-
    z(Container, Contents) :> p(Path),
    member(FileName, Contents), % TODO: specific predicate to extract sub path 
    f(FileName) :> p(Path).
  
contains(Container, z, DirName, d, Path) :-
    z(Container, Contents) :> p(Path),
    member(DirName, Contents), % TODO: specific predicate to extract sub path 
    d(DirName, _) :> p(Path).