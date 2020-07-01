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

test('archive match, with or without version') :-
    FileList = [ '.', '..', 'paragraph-ui.war', 'paragraph-ui-0.2.war' ],
    paragraph:archive_match('paragraph-ui(-version).war', FileList, 'paragraph-ui.war', war, '', 'paragraph-ui'),
    paragraph:archive_match('paragraph-ui(-version).war', FileList, 'paragraph-ui-0.2.war', war, '0.2', 'paragraph-ui').

test('archive match, without version') :-
    FileList = [ '.', '..', 'paragraph-ui.war', 'paragraph-ui-0.2.war' ],
    paragraph:archive_match('paragraph-ui.war', FileList, 'paragraph-ui.war', war, '', 'paragraph-ui').

%% file match

test('file match, without version') :-
    FileList = [ '.', '..', 'pom.xml', '' ],
    paragraph:file_match('pom.xml', FileList, 'pom.xml', pom, '', 'paragraph-ui').

%% contloc

test('container loc of paragraph-ui app archive, version already set') :-
    paragraph:contloc('paragraph-ui', warfile('paragraph-ui-0.0.1-SNAPSHOT.war'), '0.0.1-SNAPSHOT', file("/tmp/paragraph/paragraph-ui-0.0.1-SNAPSHOT.war"), [], [ ag('paragraph'), ve('0.0.1-SNAPSHOT')]).

test('container loc of paragraph-ui app archive, version not set') :-
    paragraph:contloc('paragraph-ui', warfile('paragraph-ui-(version).war'),      '0.0.1-SNAPSHOT', file("/tmp/paragraph/paragraph-ui-0.0.1-SNAPSHOT.war"), [], [ ag('paragraph'), ve('0.0.1-SNAPSHOT')]).

test('container loc of paragraph-ui pom.xml, version not set') :-
    paragraph:contloc('paragraph-ui', lfile('pom.xml'), '', file("/opt/paragraph/ParagraphUI/pom.xml"), [], [ ag('paragraph'), ve(''), ad(paragraph_ui) ]).


%% parameters defined in paragraph_conf

test('print the parameters (fails if there are any inconsistencies)') :-
    parameters.

test('find the container of parameter pom_xml_version') :-
    parameters(pom_xml_version, C),
    C = war_pom_xml.

test('find one of the parameters of container war_pom_xml') :-
    parameters(P, war_pom_xml),
    P = pom_xml_version.

%% paramval

test('xpath -> xml file -> warfile : find the pom version of paragraph archives') :-
    paramval(pom_xml_version, Version, Val, [ag('paragraph'), ve('0.0.1-SNAPSHOT')]),
    Version = '0.0.1-SNAPSHOT',    % app version
    Val = '0.0.1-SNAPSHOT'.        % value

test('xpath -> xpath -> xml file -> warfile : find the parent pom version of paragraph archives') :-
    paramval(pom_xml_parent_version, Version, Val, [ag('paragraph'), ve('0.0.1-SNAPSHOT')]),
    Version = '0.0.1-SNAPSHOT',    % app version
    Val = '2.3.0.RELEASE'.         % value = spring boot starter parent version

test('search archive with wd option : find the parent pom version of paragraph archives') :-
    paramval(pom_xml_parent_version, Version, Val, [ag('paragraph'), ve('0.0.1-SNAPSHOT'), wd(examples)]),
    Version = '0.0.1-SNAPSHOT',    % app version
    Val = '2.3.0.RELEASE'.         % value = spring boot starter parent version

test('search file with ad option : find the parent pom version of paragraph') :-
    paramval(pom_xml_parent_version, Version, Val, [ag('paragraph'), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui)]),
    Version = '0.0.1-SNAPSHOT',    % app version
    Val = '2.3.0.RELEASE'.         % value = spring boot starter parent version

:- end_tests(paragraph_paramval).
