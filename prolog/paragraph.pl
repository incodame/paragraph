/*
 * paragraph toolkit
 *
 */
:- module(paragraph, [doc/3, download_as/4, download_as/5, exported_predicates/2, objects/0, predicates/0, predicates_using/2, showdoc/1]).
:- use_module(library(iostream)).
:- use_module(library(lists)).
:- use_module(library(xpath)).
:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(clpfd)).
:- use_module(library(clpr)).
%:- use_module(library(coworkers)).
%:- use_module(coworkers_02).
:- use_module(library(dcg/basics)).
%:- use_module(library(docstore)).
%:- use_module(library(getpass)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_cookie)).
:- use_module(library(http/json)).
:- use_module(library(csv)).
:- use_module(library(process)).
:- use_module(library(regex)).
:- use_module(library(www_browser)).
:- use_module(paragraph_conf).
%:- use_module(git_automation).
%:- use_module(string_ops).
%:- use_module(yaml_query).

doc(doc/3,                 spec(['Predicate', 'Spec', 'DocText']),
                           ['  Predicate short documentation in DocText',
                            '  * Spec is a predicate Signature',
                            '  * DocText is a list of strings, which can be pretty printed with showdoc/1']).
doc(showdoc/1,             spec(['Predicate']),
                           ['  pretty Prints Predicate documentation',
                            '  documentation is stored in doc/3 facts']).
doc(application/3,         spec(['View', 'ApplicationGroup', 'ApplicationShortId']),
                           ['  lists all ApplicationShortId and ApplicationGroup']).
doc(application_batch/5,   spec(['View', 'ApplicationShortId', 'Version', 'ZipFile', 'BatchCmd']),
                           ['  a BatchCmd is included in ZipFile of ApplicationShortId for version Version',
                            '  * ApplicationShortID can be looked up with application/3',
                            '  * ZipFile is a batch zip',
                            '  * Version must be a string with ""']).
doc(application_cluster/4, spec(['View', 'ApplicationShortId', 'Version', 'ClusterId']),
                           ['  ApplicationShortId for version Version is configured to be deployed on ClusterId',
                            '  * ApplicationShortID can be looked up with application/3',
                            '  * Version must be a string with ""']).
doc(application_cluster/5, spec(['View', 'ApplicationShortId', 'Version', 'ClusterId', 'ScopeOptions']),
                           ['  ApplicationShortId for version Version is configured to be deployed on ClusterId',
                            '  * ApplicationShortID can be looked up with application/3',
                            '  * Version must be a string with ""',
                            '  * ScopeOptions: depending on the View, ag(ApplicationGroup), ve(Version), env(Environment)']).
doc(application_jar/5,     spec(['View', 'ApplicationShortId', 'Version', 'ArchiveFile', 'JarFile']),
                           ['  a JarFile is included in ArchiveFile of ApplicationShortId for version Version',
                            '  * ApplicationShortID can be looked up with application/3',
                            '  * ArchiveFile can be an ear file or a batch zip',
                            '  * Version must be a string with ""']).
doc(application_java_class/6, spec(['View', 'ApplicationShortId', 'Version', 'ArchiveFile', 'JarOrWarFile', 'JavaClass']),
                           ['  a JavaClass is included in JarOrWarFile of ApplicationShortId for version Version',
                            '  * ApplicationShortID can be looked up with application/3',
                            '  * ArchiveFile can be an ear file or a batch zip',
                            '  * JarOrWarFile is included in ArchiveFile',
                            '  * JavaClass is included in JarOrWarFile',
                            '  * Version must be a string with ""']).
doc(application_java_class/7, spec(['View', 'ApplicationShortId', 'Version', 'ArchiveFile', 'WarFile', 'JarFile', 'JavaClass']),
                           ['  a JavaClass is included in JarFile of WarFile of ApplicationShortId for version Version',
                            '  * ApplicationShortID can be looked up with application/3',
                            '  * ArchiveFile can be an ear file or a batch zip',
                            '  * WarFile is the container for the JarFile',
                            '  * JarFile is included in WarFile',
                            '  * JavaClass is included in JarFile',
                            '  * Version must be a string with ""']).
doc(application_scm/4,     spec(['View', 'ApplicationShortId', 'Version', 'GitUrl']),
                           ['  ApplicationShortId for version Version has Git repository Url GitUrl',
                            '  * ApplicationShortID can be looked up with application/3',
                            "  * Version, if specified, must be with single quotes : default='current'"]).
doc(application_scm/5,     spec(['View', 'ApplicationShortId', 'Version', 'GitUrl', 'ScopeOptions']),
                           ['  ApplicationShortId for version Version has Git repository Url GitUrl',
                            '  * ApplicationShortID can be looked up with application/3',
                            "  * Version, if specified, must be with single quotes : default='current'",
                            '  * ScopeOptions: depending on the View, ag(ApplicationGroup), ve(Version), env(Environment)']).
doc(application_war/5,     spec(['View', 'ApplicationShortId', 'Version', 'ArchiveFile', 'WarFile']),
                           ['  a WarFile is included in ArchiveFile of ApplicationShortId for version Version',
                            '  * ApplicationShortID can be looked up with application/3',
                            '  * ArchiveFile can be an ear file or a batch zip',
                            '  * Version must be a string with ""']).
doc(web_jar/6,             spec(['View', 'ApplicationShortId', 'Version', 'ArchiveFile', 'WarFile', 'JarFile']),
                           ['  a JarFile is included in WarFile of ApplicationShortId for version Version',
                            '  * ApplicationShortID can be looked up with application/3',
                            '  * ArchiveFile is an .ear file',
                            '  * WarFile is a .war file included in ArchiveFile',
                            '  * Version must be a string with ""']).
doc(goto_cluster/1,        spec(['Cluster']),
                           ['  the default web browser will navigate to the configuration for cluster Cluster',
                            '  See also: application_cluster/4']).
doc(can_goto_param/2,      spec(['Parameter', 'Version']),
                           ['  A Parameter is configured in a container, via the paragraph:paramloc/5 predicate',
                            '    using a search specification like "regexp:" or "xpath:" etc...',
                            '  if a container exists in version Version, and can be navigated to with the browser',
                            '    its search specification will be printed.']).
doc(goto_param/2,          spec(['Parameter', 'Version']),
                           ['  A Parameter is configured in a container, via the paragraph:paramloc/5 predicate',
                            '  the default web browser will navigate to the container, if its "browsable"',
                            '  See also: can_goto_param/3 for browsable parameters',
                            '  See also: parameters/2']).
doc(package/4,             spec(['ApplicationGroup', 'Version', 'PackageFile', 'DownloadUrl']),
                           ['  PackageFile is available for ApplicationGroup version Version at DownloadUrl',
                            "  * Version is a string with ''"]).
doc(parameters/0,          spec([]),
                           ['  Lists all parameters configured for search in containers, via the paragraph:paramloc/5 predicate',
                            '  The value can be searched with paramval/3']).
doc(parameters/2,          spec(['Parameter', 'Container']),
                           ['  A Parameter is configured in container Container, via the paragraph:paramloc/5 predicate',
                            '  * Container can be a text file, archive file (.zip or .ear), or web resource.',
                            '  See also: parameters/0']).
doc(paramval/3,            spec(['Parameter', 'Version', 'ParameterValue']),
                           ['  A Parameter is configured in a container, via the paragraph:paramloc/5 predicate',
                            '    using a search specification like "regexp:" or "xpath:" etc...',
                            '  * ParameterValue will be extracted from the container',
                            '  * Version must be a string with ""',
                            '  See also: parameters/2']).
doc(paramval/4,            spec(['Parameter', 'Version', 'ParameterValue', 'ScopeOptions']),
                           ['  A Parameter is configured in a container, via the paragraph:paramloc/5 predicate',
                            '    using a search specification like "regexp:" or "xpath:" etc...',
                            '  * ParameterValue will be extracted from the container',
                            '  * Version must be a string with ""',
                            '  * ScopeOptions: depending on the View, ag(ApplicationGroup), ve(Version), env(Environment)',
                            '  See also: parameters/2']).
doc(objects/0,             spec([]),
                           ['  Lists all objects referenced by predicates in module paragraph.']).
doc(predicates/0,          spec([]),
                           ['  Lists all predicates available in module paragraph.']).
doc(predicates_using/2,    spec(['Predicate', 'Object']),
                           ['  Lists all predicates which use an object listed in objects/0.',
                            "  example:  predicates_using(P,'GitUrl')."]).
doc(sync/2,                spec(['View', 'Status']),
                           ['  * View is one of app, maven, build, deploy, cluster, user, appconfig',
                            '  * Status 0 if sync was successful, otherwise error status code']).
doc(sync/3,                spec(['View', 'Status', 'Options']),
                           ['  * View is one of app, maven, packages, build, deploy, cluster, user, appconfig',
                            '  * Status 0 if sync was successful, otherwise error status code',
                            '  * Options: depending on the View, ag(ApplicationGroup), ve(Version), env(Environment)']).
doc(download_as/4,         spec(['Url', 'LocalFile', 'DownloadOptions', 'StatusCode']),
                           ['  Downloads Url using http as LocalFile in the defaut directory defined in paragraph_conf.pl',
                            '  * LocalFile is a file name without any path',
                            '  * DownloadOptions is a list of valid options for http_open/3',
                            '  * StatusCode is the http return code']).
doc(download_as/5,         spec(['Url', 'LocalDir', 'LocalFile', 'DownloadOptions', 'StatusCode']),
                           ['  Downloads Url using http as LocalFile in the defaut directory defined in paragraph_conf.pl',
                            '  * LocalDir is a local directory path which might or might not exist before the download',
                            '  * LocalFile is a file name without any path',
                            '  * DownloadOptions is a list of valid options for http_open/3',
                            '  * StatusCode is the http return code']).
doc(search/2,              spec(['Consumer', 'Scoper']),
                           ['  Run a Consumer predicate within scope given by Scoper',
                            '  example: search(paramval(context_root, Ve, Val), app_clusters("lambda")).',
                            '  * Consumer is an iterator that consumes Options',
                            '  * Scoper is a generator that yields Options',
                            '  See also ask_options/1 and specify_options/1.']).
doc(ask_options/1,         spec(['ScopeOptions']),
                           ['  Used internally, to build an iterator-predicate which can be combined with search/2']).
doc(specify_options/1,     spec(['ScopeOptions']),
                           ['  Used internally, to build a generator-predicate which can be combined with search/2']).
doc(exported_predicates/2, spec(['paragraph', 'PredicateList']),
                           ['  Predicates made available by the paragraph Module.',
                            '  See also predicates/0.']).

%% predicates, their doc and objects

exported_predicates(paragraph, PredicateListe) :-
    module_property(paragraph, exports(PredicateListe)). 

predicates :-
    findall(Predikat, doc(Predikat,_,_), PredikatListe),
    list_to_set(PredikatListe, PredikatSet),
    sort(PredikatSet, Predicates),
    maplist(writeln, Predicates).

predicate_match(Predicate, Pattern) :-
    Predicate =.. [/, PredName, _],
    PredName =~ Pattern/i.

predicates(Pattern) :-
    findall(Predikat, doc(Predikat,_,_), PredikatListe),
    include({Pattern}/[Predicate]>>predicate_match(Predicate, Pattern), PredikatListe, MatchListe),
    list_to_set(MatchListe, PredikatSet),
    sort(PredikatSet, Predicates),
    maplist(writeln, Predicates).

objects :-
    findall(L, doc(_,spec(L),_), LL),
    flatten(LL, ObjectListe),
    % TODO - filter on considered_for_chaining
    list_to_set(ObjectListe, ObjectSet),
    sort(ObjectSet, Objects),
    maplist(writeln, Objects).

object_match(Object, Pattern) :-
    Object =~ Pattern/i.

objects(Pattern) :-
    findall(L, doc(_,spec(L),_), LL),
    flatten(LL, ObjectListe),
    include({Pattern}/[Object]>>object_match(Object, Pattern), ObjectListe, MatchListe),
    list_to_set(MatchListe, ObjectSet),
    sort(ObjectSet, Objects),
    maplist(writeln, Objects).

predicates_using(P, O) :-
    doc(P, spec(L), _),
    member(O, L).

showdoc(Predicate) :-
    doc(Predicate, spec(L), Lines),
    Predicate =.. [/, PredName, _],
    join_strings(L, ', ', Jstr),
    format(string(Signature), '~w(~w)', [PredName, Jstr]),
    writeln(Signature),
    maplist(writeln, Lines).

%% read input without echo

read_key([Code|Codes]) :-
    get_single_char(Code),
    read_pending_codes(user,Codes,[]).

read_keyatom(Codes, KAtom) :-
    read_key(NewCodes),
    (NewCodes = [13] ->
         atom_codes(KAtom,Codes)
    ;
    append(Codes, NewCodes, AllCodes),
    read_keyatom(AllCodes, KAtom)
    ).

get_pato(PAtom) :-
    read_keyatom([],PAtom).

%% string ops

% checks if a char is a digit
digit_code(X) :- integer(X), X >= 0'0, X =< 0'9.

% reverse of split_string/4
join_strings([X],_, X).
join_strings([H|T], Sep, J) :-
    join_strings(T, Sep, J1),
    string_concat(Sep, J1, J2),
    string_concat(H, J2, J), !.

%% sync resources

download_as(Url, LocalFile, DownloadOptions, StatusCode) :-
    http_open(Url, Reply, [status_code(StatusCode) | DownloadOptions]),
    setup_call_cleanup(
        open(LocalFile, write, LocalFileStream),
        copy_stream_data(Reply, LocalFileStream),
        close(LocalFileStream)).

download_as(Url, LocalDir, LocalFile, DownloadOptions, StatusCode) :-
    http_open(Url, Reply, [status_code(StatusCode) | DownloadOptions]),
    format(atom(LocalFilePath), '~w/~w', [ LocalDir, LocalFile ]),
    (exists_directory(LocalDir) -> true ; make_directory(LocalDir)),
    setup_call_cleanup(
        open(LocalFilePath, write, LocalFileStream),
        copy_stream_data(Reply, LocalFileStream),
        close(LocalFileStream)).
