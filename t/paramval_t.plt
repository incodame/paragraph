:- use_module(library(plunit)).
:- begin_tests(paragraph_paramval).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), working_directory(_, Ps).
:- use_module(paragraph).

%% package version

test('parse package type, version and app') :-
    paragraph:package_version('paragraph-ui-0.1.war', war, '0.1', 'paragraph-ui').

%% archive match

test('archive match, version is known') :-
    FileList = [ '.', '..', 'paragraph-ui-0.1.war', 'paragraph-ui-0.2.war' ],
    paragraph:archive_match('paragraph-ui-0.1.war', FileList, 'paragraph-ui-0.1.war', war, '0.1', 'paragraph-ui').

test('archive match, version unknown') :-
    FileList = [ '.', '..', 'paragraph-ui-0.1.war', 'paragraph-ui-0.2.war' ],
    paragraph:archive_match('paragraph-ui-(version).war', FileList, 'paragraph-ui-0.1.war', war, '0.1', 'paragraph-ui'),
    paragraph:archive_match('paragraph-ui-(version).war', FileList, 'paragraph-ui-0.2.war', war, '0.2', 'paragraph-ui').

%% contloc

test('container loc of paragraph-ui app archive, version already set') :-
    paragraph:contloc('paragraph-ui', warfile('paragraph-ui-0.0.1-SNAPSHOT.war'), '0.0.1-SNAPSHOT', "file:/tmp/paragraph/paragraph-ui-0.0.1-SNAPSHOT.war", [], [ ag('paragraph'), ve('0.0.1-SNAPSHOT')]).

test('container loc of paragraph-ui app archive, version not set') :-
    paragraph:contloc('paragraph-ui', warfile('paragraph-ui-(version).war'),      '0.0.1-SNAPSHOT', "file:/tmp/paragraph/paragraph-ui-0.0.1-SNAPSHOT.war", [], [ ag('paragraph'), ve('0.0.1-SNAPSHOT')]).

%% paramval

test('find the pom version of paragraph archives') :-
    paramval(pom_xml_version, Version, Val, [ag('paragraph'), ve('0.0.1-SNAPSHOT')]),
    Version = '0.0.1-SNAPSHOT',
    Val = '0.0.1-SNAPSHOT'.

:- end_tests(paragraph_paramval).
