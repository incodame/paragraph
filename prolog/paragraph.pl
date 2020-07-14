/*
 * paragraph toolkit
 *
 */
:- module(paragraph, [application_jar/5, application_jar/4, doc/3, download_as/4, download_as/5, exported_predicates/2, from_list/1, objects/0, parameters/0, parameters/2, paramval/3, paramval/4, paramval/5, pdoc/1, predicates/0, predicates_using/2, search/2, showdoc/1]).
:- use_module(library(iostream)).
:- use_module(library(lists)).
:- use_module(library(list_util)).
:- use_module(library(xpath)).
:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(clpfd)).
:- use_module(library(clpr)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_cookie)).
:- use_module(library(http/json)).
:- use_module(library(csv)).
:- use_module(library(process)).
:- use_module(library(regex)).
:- use_module(library(www_browser)).
:- use_module(library(yall)).
:- use_module(paragraph_conf).

:- table contloc_app_archive/6.


doc(doc/3,                 spec(['Predicate', 'Spec', 'DocText']),
                           ['  Predicate short documentation in DocText',
                            '  * Spec is a predicate Signature',
                            '  * DocText is a list of strings, which can be pretty printed with showdoc/1']).
doc(showdoc/1,             spec(['Predicate']),
                           ['  pretty Prints Predicate documentation',
                            '  documentation is stored in doc/3 facts']).
doc(application/4,         spec(['View', 'ApplicationGroup', 'ApplicationShortId', 'Options']),
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
doc(application_jar/4,     spec(['ApplicationShortId', 'Version', 'ArchiveFile', 'JarFile']),
                           ['  a JarFile is included in ArchiveFile of ApplicationShortId for version Version',
                            '  * ApplicationShortID can be looked up with application/3',
                            '  * ArchiveFile can be an ear file or a batch zip',
                            '']).
doc(application_jar/5,     spec(['ApplicationShortId', 'Version', 'ArchiveFile', 'JarFile', 'Options']),
                           ['  a JarFile is included in ArchiveFile of ApplicationShortId for version Version',
                            '  * ApplicationShortID can be looked up with application/3',
                            '  * ArchiveFile can be an ear file or a batch zip',
                            '  * Options can contain: ve(Version), ag(ApplicationGroup)']).
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
                           ['  A Parameter is configured in a container, via the paragraph:paramloc/4 predicate',
                            '    using a search specification like regexp() or xpath() etc...',
                            '  if a container exists in version Version, and can be navigated to with the browser',
                            '    its search specification will be printed.']).
doc(goto_param/2,          spec(['Parameter', 'Version']),
                           ['  A Parameter is configured in a container, via the paragraph:paramloc/4 predicate',
                            '  the default web browser will navigate to the container, if its "browsable"',
                            '  See also: can_goto_param/3 for browsable parameters',
                            '  See also: parameters/2']).
doc(package/4,             spec(['ApplicationGroup', 'Version', 'PackageFile', 'DownloadUrl']),
                           ['  PackageFile is available for ApplicationGroup version Version at DownloadUrl',
                            "  * Version is a string with ''"]).
doc(parameters/0,          spec([]),
                           ['  Lists all parameters configured for search in containers, via the paragraph:paramloc/4 predicate',
                            '  Documentation of a parameter can be displayed via pdoc/1',
                            '  The value can be searched with paramval/3']).
doc(parameters/2,          spec(['Parameter', 'Container']),
                           ['  A Parameter is configured in container Container, via the paragraph:paramloc/4 predicate',
                            '  * Container can be a text file, archive file (.zip or .ear), or web resource.',
                            '  See also: parameters/0, pdoc/1']).
doc(pdoc/1,                spec(['Parameter']),
                           ['  Display documentation for Parameter']).
doc(paramval/3,            spec(['Parameter', 'ParameterValue', 'ScopeOptions']),
                           ['  A Parameter is configured in a container, via the paragraph:paramloc/4 predicate',
                            '    using a search specification like regexp() or xpath() etc...',
                            '  * ParameterValue will be extracted from the container',
                            '  * ScopeOptions: depending on the parameter, ag(ApplicationGroup), ve(Version), env(Environment)',
                            '  See also: parameters/2']).
doc(paramval/4,            spec(['Parameter', 'Version', 'ParameterValue', 'ScopeOptions']),
                           ['  A Parameter is configured in a container, via the paragraph:paramloc/4 predicate',
                            '    using a search specification like regexp() or xpath() etc...',
                            '  * ParameterValue will be extracted from the container',
                            '  * Version is the version of the application where the parameter was found',
                            '  * ScopeOptions: depending on the parameter, ag(ApplicationGroup), ve(Version), env(Environment)',
                            '  See also: parameters/2']).
doc(paramval/5,            spec(['Parameter', 'ApplicationShortId', 'Version', 'ParameterValue', 'ScopeOptions']),
                           ['  A Parameter is configured in a container, via the paragraph:paramloc/4 predicate',
                            '    using a search specification like regexp() or xpath() etc...',
                            '  * ParameterValue will be extracted from the container',
                            '  * ApplicationShortId is the identifier of the application where the parameter was found',
                            '  * Version is the version of the application where the parameter was found',
                            '  * ScopeOptions: depending on the parameter, ag(ApplicationGroup), ve(Version), env(Environment)',
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
doc(from_list/1,           spec(['ScopeOptions']),
                           ['  Scoper based on a list, which can be combined with search/2']).
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

appdirectory(AppId, AppDirectory, Options) :-
    member(ad(AppDirectoryAlias), Options),
    directory_alias(AppId, AppDirectoryAlias, AppDirectory),
    format(string(Message), 'Trying app directory = ~w', [AppDirectory]),
    writeln(Message).

default_workdirectory(WorkDirectory) :-
    getenv("PARAGRAPH_TEMP", WorkDirectory).

workdirectory(WorkDirectory, Options) :-
    (member(wd(WorkDirectoryAlias), Options),
     directory_alias(WorkDirectoryAlias, WorkDirectory)
    ;
     default_workdirectory(WorkDirectory)
    ),
    format(string(Message), 'Trying work directory = ~w', [WorkDirectory]),
    writeln(Message).

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

%% archive management

% List of Zip Entries
list_zipfile_entries(ZipFile, ZipEntryList) :-
    open(ZipFile, read, ArchiveIn, [type(binary)]),
    archive_entries(ArchiveIn, ZipEntryList),
    close(ArchiveIn).

% List of Zip Entries matching a pattern
archive_entries_matching(ArchiveIn, EndsWithSpec, Entry) :-
    archive_entries(ArchiveIn, ZipAllList),
    member(Entry, ZipAllList),
    string_concat(_Start, EndsWithSpec, Entry).

zipfile_entry_matches(ZipFile, EndsWithSpec, Entry) :-
    setup_call_cleanup(
        open_any(ZipFile, read, ArchiveIn, Close, [type(binary)]),
        archive_entries_matching(ArchiveIn, EndsWithSpec, Entry),
        close_any(Close)).

% Open archive entry, Entry must be an atom with '', not a string
open_archive_entry(ArchiveFile, Entry, Stream) :-
    open(ArchiveFile, read, In, [type(binary)]),
    format(string(Info), 'Reading archive ~w: ~w', [ArchiveFile, Entry]),
    writeln(Info),
    archive_open(In, Archive, [close_parent(true)]),
    archive_next_header(Archive, Entry),
    archive_open_entry(Archive, Stream),
    archive_close(Archive).

application_jar(AppId, Ver, ArchiveFile, Jar, Options) :-
    contloc(AppId, _, Ver, file(ArchiveFile), [], Options),
    zipfile_entry_matches(ArchiveFile, ".jar", Jar).

application_jar(AppId, Ver, ArchiveFile, Jar) :-
    application_jar(AppId, Ver, ArchiveFile, Jar, []).

%% scoping and search

want_opt(Opt, Options) :-
    search_option(Opt, Text, DefaultGet),  % see definition in paragraph_conf
    (ground(Opt) ->
         (Options = [] -> true             % incorrect, use functor !
         ;
          memberchk(Opt, Options)
         )
    ;
        (member(Opt, Options) -> true
        ;
         catch(ask_option(Opt), _, (
             format(string(Message), 'Confirm ~w:', [Text]),
             writeln(Message),
             call(DefaultGet)
         ))
        )
    ).

%???(S, C) :- search(C, S).
%:- op(700, xfx, user:(???)).

specify_option(SearchOption) :-
    shift(specify_option(SearchOption)).

no_option(SearchOption) :-
    shift(no_option(SearchOption)).

ask_option(SearchOption) :-
    shift(ask_option(SearchOption)).

% search C works within scope S
%  C is an iterator that consumes Options ("Consumer"), S is a generator that yields Options ("Scoper")
%  S = wanted instructs search to print the wanted options of the consumer
search(C,S) :-
    reset(C,Term1,C1),
    ( C1 == 0 ->
      true
    ; Term1 = ask_option(X) ->
      (S = wanted, X =.. [ FunctorName, Var ] ->
           search_option(X, Text, _),                   % see definition in paragraph_conf
           format(string(Message), 'Wanted ~w:  ~w', [FunctorName, Text]),
           Var = '_',
           writeln(Message),
           search(C1, S)        % recurse on unfinished goal
      ;
           (reset(S,Term2,S1),
            ( Term2 == 0 ->
                  X = eof,
                  call(C1)      % unfinished goal from C
            ; S1 == 0 ->        % neu
                  X = eof,
                  call(C1)
            ; Term2 = specify_option(X) ->
                  search(C1,S1) % X now unifies with ask_option(X), recursion with unfinished goals
            ; Term2 = no_option(X) ->
                  X = eof,
                  call(C1)
            )
           )
      )
    ).

%%% scopers

from_list([]).
from_list([X|Xs]) :-
    specify_option(X),
    from_list(Xs).

%%% special consumers

slurp_options([Opt | Ropts]) :-
    ask_option(GivenOpt),
    (GivenOpt == eof -> Opt = [], Ropts = []; Opt = GivenOpt, slurp_options(Ropts)).

%% container location

:- discontiguous contloc/6.

%%% application archives

package_version(PackageFile, Type, Version, AppId) :-
    app_archive(Type, AppId, ArNameTemplate, []),
    split_string(ArNameTemplate, "()", "", [Prefix, VersionTokenStr, Suffix]),
    version_tok(VersionTokenStr, VePrefix, VeSuffix),
    atom_concat(Prefix, Rest, PackageFile),
    atom_concat(VersionToken, Suffix, Rest),
    atom_length(VersionToken, VtLen),
    (VtLen is 0 ->
         Version = ''
    ;
         atom_concat(VePrefix, VeRest, VersionToken),
         atom_concat(Version, VeSuffix, VeRest)
    ).

contloc(AppId,    earfile(EarFile), Version, LocSpec, [], Options) :-
    contloc_app_archive(EarFile, ear, AppId, Version, LocSpec, [], Options).

contloc(AppId,    warfile(WarFile), Version, LocSpec, [], Options) :-
    contloc_app_archive(WarFile, war, AppId, Version, LocSpec, [], Options).

contloc(AppId,    jarfile(JarFile), Version, LocSpec, [], Options) :-
    contloc_app_archive(JarFile, jar, AppId, Version, LocSpec, [], Options).

contloc(AppId,    zipfile(ZipFile), Version, LocSpec, [], Options) :-
    contloc_app_archive(ZipFile, zip, AppId, Version, LocSpec, [], Options).

contloc_app_archive(ArTest, FileType, AppId, Version, file(LocSpec), [], Options) :-
    want_opt(ag(AppGroup), Options),
    want_opt(ve(Version), Options),
    workdirectory(WorkDirectory, Options),
    directory_files(WorkDirectory, FileList),
    archive_match(ArTest, FileList, ArMatch, FileType, Version, AppId),
    application(app, AppId, AppGroup, _),
    format(string(LocSpec), "~w/~w", [WorkDirectory, ArMatch]).


list([])     --> [].
list([L|Ls]) --> [L], list(Ls).

conc_of(Prefix, Suffix) --> list(Prefix), "version", list(Suffix).

version_tok(VersionTokenStr, VePrefixStr, VeSuffixStr) :-
    string_codes(VersionTokenStr, VersionTokenCodes),
    phrase(conc_of(VePrefixL, VeSuffixL), VersionTokenCodes),
    string_codes(VePrefixStr, VePrefixL),
    string_codes(VeSuffixStr, VeSuffixL).

% TODO: remove call to app_archive, use FileTest only
archive_match(FileTest, FileList, File, FileType, Version, AppId) :-
    (app_archive(FileType, AppId, ArNameTemplate, _), atom_codes(ArNameTemplate, Codes), memberchk(0'(, Codes),
        split_string(ArNameTemplate, "()", "", [Prefix, VersionTokenStr, Suffix]),
        version_tok(VersionTokenStr, VePrefix, VeSuffix),
        (ground(Version), atom_length(Version, Len)  ->
             (Len is 0 ->
                  join_strings([Prefix, Suffix], "", FileStr)
             ;
                  join_strings([Prefix, VePrefix, Version, VeSuffix, Suffix], "", FileStr)
             ),
        %atom_string(FileTest, FileStr), File = FileTest
             atom_string(File, FileStr)
        ;
             true
        ),
        member(File, FileList),
        (\+ground(Version) ->
             atom_concat(Prefix, Rest, File),
             atom_concat(VersionToken, Suffix, Rest),
             atom_length(VersionToken, VtLen),
             (VtLen is 0 ->
                  Version = ''
             ;
              atom_concat(VePrefix, VeRest, VersionToken),
              atom_concat(Version, VeSuffix, VeRest)
             )
        ;
             true
        )
    ;atom_concat('*.', FileType, FileTest), atom_concat('*', Suffix, FileTest),
        member(File, FileList), atom_concat(_, Suffix, File)
    ;
        member(FileTest, FileList), File = FileTest, Version = ''
    ).

resolve_entry_spec(pv(Param), ActualEntry, Options) :-
    paramval(Param, ActualEntry, Options),
    format(atom(Message), 'Resolved entry spec pv(~w) to ~w', [Param, ActualEntry]),
    writeln(Message).

resolve_entry_spec(pv(Param, Conversion), ActualEntry, Options) :-
    paramval(Param, ParamVal, Options),
    transform_val(Conversion, ParamVal, ActualEntry),
    format(atom(Message), 'Resolved entry spec pv(~w, ~w) to ~w', [Param, Conversion, ActualEntry]),
    writeln(Message).

resolve_entry_spec(ActualEntry, ActualEntry, _) :-
    \+compound(ActualEntry).

%%% flat files

contloc(AppId,    lfile(File), Version, LocSpec, [], Options) :-
    contloc_app_file(File, pom, AppId, Version, LocSpec, [], Options).

contloc(AppId,    lfile(File), Version, LocSpec, [], Options) :-
    contloc_app_file(File, md, AppId, Version, LocSpec, [], Options).

contloc_app_file(FileTest, FileType, AppId, Version, file(LocSpec), [], Options) :-
    want_opt(ag(AppGroup), Options),
    want_opt(ve(Version), Options),
    application(app, AppId, AppGroup, _),
    appdirectory(AppId, AppDirectory, Options),
    directory_files(AppDirectory, FileList),
    file_match(FileTest, FileList, FileMatch, FileType, Version, AppId),
    format(string(LocSpec), "~w/~w", [AppDirectory, FileMatch]).

% TODO: remove call to app_file, use FileTest only
file_match(FileTest, FileList, File, FileType, Version, AppId) :-
    (app_file(FileType, AppId, FileNameTemplate, _), atom_codes(FileNameTemplate, Codes), memberchk(0'(, Codes),
        split_string(FileNameTemplate, "()", "", [Prefix, VersionTokenStr, Suffix]),
        version_tok(VersionTokenStr, VePrefix, VeSuffix),
        (ground(Version), atom_length(Version, Len)  ->
             (Len is 0 ->
                  join_strings([Prefix, Suffix], "", FileStr)
             ;
                  join_strings([Prefix, VePrefix, Version, VeSuffix, Suffix], "", FileStr)
             ),
             atom_string(File, FileStr)
        ;
             true
        ),
        member(File, FileList),
        (\+ground(Version) ->
             atom_concat(Prefix, Rest, File),
             atom_concat(VersionToken, Suffix, Rest),
             atom_length(VersionToken, VtLen),
             (VtLen is 0 ->
                  Version = ''
             ;
              atom_concat(VePrefix, VeRest, VersionToken),
              atom_concat(Version, VeSuffix, VeRest)
             )
        ;
             true
        )
    ;atom_concat('*.', FileType, FileTest), atom_concat('*', Suffix, FileTest),
        member(File, FileList), atom_concat(_, Suffix, File)
    ;
        member(FileTest, FileList), File = FileTest, Version = ''
    ).

%% parameters defined in paragraph_conf

parameters :-
    findall(Param, paramloc(Param,_,_,_), ParList),
    list_to_set(ParList, ParSet),
    sort(ParSet, Parameters),
    maplist(writeln, Parameters).
parameters(Param, Container) :- paramloc(Param, Container, _LocSpec, _LocArgs).
pdoc(Param) :-
    paramloc(Param,_,LocString,Opts),
    member(doc(DocString), Opts),
    format(string(Documentation), '~w [~w]', [DocString, LocString]),
    writeln(Documentation), !.

%% paramval - navigating resources inside hierarchical containers

% shortcuts
paramval(Param, Val, Options) :-
    paramval(Param, _, _, Val, Options).

paramval(Param, Ve, Val, Options) :-
    paramval(Param, _, Ve, Val, Options).

%%% xpath

% xpath for a flat file
paramval(Param, AppId, Version, Val, Options) :-
    paramloc(Param, XmlSource, xpath(Xpath), _),
    paramloc(AppId, XmlSource, _Ffile, lfile(FileSpec), _),
    dbg_paramval('xpath->lfile', Param, XmlSource, Options),
    contloc(AppId, lfile(FileSpec), Version, file(FilePath), _, Options),
    format(string(Info), 'Reading file ~w', [FilePath]),
    writeln(Info),
    load_xml(FilePath, XmlRoot, _),
    xpath(XmlRoot, Xpath, Val).

% xpath for an archive xml resource (war / jar / zip)
paramval(Param, AppId, Version, Val, Options) :-
    paramloc(Param, XmlSource, xpath(Xpath), _),
    paramloc(AppId, XmlSource, Afile, endswith(EntrySpec), _),
    resolve_entry_spec(EntrySpec, Entry2Find, Options),
    dbg_paramval('xpath->endswith', Param, XmlSource, Options),
    member(Archive, [warfile(Afile), jarfile(Afile), zipfile(Afile)]),
    contloc(AppId, Archive, Version, file(AfilePath), _, Options),
    zipfile_entry_matches(AfilePath, Entry2Find, EntryStr),
    atom_string(Entry, EntryStr),
    setup_call_cleanup(
        open_archive_entry(AfilePath, Entry, XmlStream),
        (
            load_xml(stream(XmlStream), XmlRoot, _),
            xpath(XmlRoot, Xpath, Val)
        ),
        close(XmlStream)).

% nested xpath definitions
paramval(Param, AppId, Version, Val, Options) :-
    paramloc(Param, XmlParentTag, xpath(Xpath), _),
    %paramloc(XmlParentTag, _, xpath(_), _), %  creates duplicates due to various containers
    dbg_paramval('xpath->*', Param, XmlParentTag, Options),
    inc_dbg_level(Options, NewOptions),
    paramval(XmlParentTag, AppId, Version, ParentXml, NewOptions),  % NB: here the container is not used
    xpath(ParentXml, Xpath, Val).

%%% regexp

% regexp for a flat file
paramval(Param, AppId, Version, Val, Options) :-
    paramloc(Param, TxtSource, regexp(Regexp), _),
    paramloc(AppId, TxtSource, _Ffile, lfile(FileSpec), _),
    dbg_paramval('regexp->lfile', Param, TxtSource, Options),
    contloc(AppId, lfile(FileSpec), Version, file(FilePath), _, Options),
    format(string(Info), 'Reading file ~w', [FilePath]),
    writeln(Info),
    lines(file(FilePath), Lines),
    lazy_include({Regexp, Val}/[Line]>>text_match(Line, Regexp, Val), Lines, [_]). % non empty list

% regexp for an archive xml resource (war / jar / zip)
paramval(Param, AppId, Version, Val, Options) :-
    paramloc(Param, TxtSource, regexp(Regexp), _),
    paramloc(AppId, TxtSource, Afile, endswith(EntrySpec), _),
    resolve_entry_spec(EntrySpec, Entry2Find, Options),
    dbg_paramval('regexp->endswith', Param, TxtSource, Options),
    member(Archive, [warfile(Afile), jarfile(Afile), zipfile(Afile)]),
    contloc(AppId, Archive, Version, file(AfilePath), _, Options),
    zipfile_entry_matches(AfilePath, Entry2Find, EntryStr),
    atom_string(Entry, EntryStr),
    setup_call_cleanup(
        open_archive_entry(AfilePath, Entry, FileStream),
        (
            lines(stream(FileStream), Lines),
            lazy_include({Regexp, Val}/[Line]>>text_match(Line, Regexp, Val), Lines, [_])
        ),
        close(FileStream)).

% nested regexp definitions
paramval(Param, AppId, Version, Val, Options) :-
    paramloc(Param, Parent, regexp(Regexp), _),
    dbg_paramval('regexp->*', Param, Parent, Options),
    inc_dbg_level(Options, NewOptions),
    paramval(Parent, AppId, Version, ParentTxt, NewOptions),
    open_string(ParentTxt, TxtStream),
    lines(stream(TxtStream), Lines),
    lazy_include({Regexp, Val}/[Line]>>text_match(Line, Regexp, Val), Lines, [_]).

text_match(Line, Regexp, Val) :-
    regex(Regexp, [], Line, ['V1L'=V1L]),
    text_to_string(V1L, Val).

dbg_paramval(Transition, Param, Container, Options) :-
    (memberchk(dbg(Level), Options) ->
         Nsp is Level * 4,
         repeat(' ', Rs), take(Nsp, Rs, Tabs),
         format(atom(Message), '~w paramval ~w : ~w -> ~w', [Tabs, Transition, Param, Container]),
         writeln(Message)
    ;
         true
    ).

inc_dbg_level([], []) :- !.
inc_dbg_level([dbg(Level)|Rest], [dbg(NewLevel)|Rest]) :-
    NewLevel is Level+1, !.
inc_dbg_level([H|Rest], [H|NewRest]) :-
    inc_dbg_level(Rest, NewRest).

