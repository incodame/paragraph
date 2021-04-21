:- use_module(library(plunit)).
:- use_module(library(xpath)).
:- begin_tests(paragraph_paramv).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), working_directory(_, Ps).
:- use_module(paragraph).


%% package version

test('parse package type, version and app') :-
    paragraph:package_version('paragraph-ui-0.1.war', war, '0.1', 'paragraph-ui').

%% archive match

% TODO - broken, should fail
test('archive match, version is known') :-
    FileList = [ '.', '..', 'paragraph-ui-0.1.war', 'paragraph-ui-0.2.war' ],
    paragraph:archive_match('paragraph-ui-0.9.war', FileList, 'paragraph-ui-0.1.war', war, '0.1', 'paragraph-ui').

test('archive match, version unknown') :-
    FileList = [ '.', '..', 'paragraph-ui-0.1.war', 'paragraph-ui-0.2.war' ],
    (paragraph:archive_match('paragraph-ui-(version).war', FileList, 'paragraph-ui-0.1.war', FileType, Ve1, 'paragraph-ui'),
     Ve1 = '0.1',
     FileType = war
    ),
    (paragraph:archive_match('paragraph-ui-(version).war', FileList, 'paragraph-ui-0.2.war', FileType, Ve2, 'paragraph-ui'),
     Ve2 = '0.2',
     FileType = war
    ).

test('archive match, with or without version') :-
    FileList = [ '.', '..', 'paragraph-ui.war', 'paragraph-ui-0.2.war' ],
    paragraph:archive_match('paragraph-ui(-version).war', FileList, 'paragraph-ui.war', war, '', 'paragraph-ui'),
    paragraph:archive_match('paragraph-ui(-version).war', FileList, 'paragraph-ui-0.2.war', war, '0.2', 'paragraph-ui').

test('archive match, without version') :-
    FileList = [ '.', '..', 'paragraph-ui.war', 'paragraph-ui-0.2.war' ],
    paragraph:archive_match('paragraph-ui.war', FileList, 'paragraph-ui.war', war, '', 'paragraph-ui').

test('archive match, wilcard') :-
    FileList = [ '.', '..', 'a.jar', 'b.jar' ],
    paragraph:archive_match('*.jar', FileList, 'a.jar', jar, '', 'paragraph-verticles'),
    paragraph:archive_match('*.jar', FileList, 'b.jar', jar, '', 'paragraph-verticles').

%% file match

test('file match, without version') :-
    FileList = [ '.', '..', 'pom.xml', '' ],
    paragraph:file_match('pom.xml', FileList, 'pom.xml', pom, '', 'paragraph-ui').

test('file match, wilcard') :-
    FileList = [ '.', '..', 'a.xml', 'b.xml' ],
    paragraph:file_match('*.xml', FileList, 'a.xml', xml, '', 'paragraph-verticles'),
    paragraph:file_match('*.xml', FileList, 'b.xml', xml, '', 'paragraph-verticles').

%% contloc

test('container loc of paragraph-ui app archive, version already set') :-
    paragraph:contloc('paragraph-ui', warfile('paragraph-ui-0.0.1-SNAPSHOT.war'), '0.0.1-SNAPSHOT', file("/opt/paragraph/ParagraphUI/target/paragraph-ui-0.0.1-SNAPSHOT.war"), [ ag('paragraph'), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)], NewScoper),
    memberchk(ar(file("/opt/paragraph/ParagraphUI/target/paragraph-ui-0.0.1-SNAPSHOT.war")), NewScoper).

test('container loc of paragraph-ui app archive, version not set') :-
    paragraph:contloc('paragraph-ui', warfile('paragraph-ui-(version).war'),      '0.0.1-SNAPSHOT', file("/opt/paragraph/ParagraphUI/target/paragraph-ui-0.0.1-SNAPSHOT.war"), [ ag('paragraph'), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)], NewScoper),
    memberchk(ar(file("/opt/paragraph/ParagraphUI/target/paragraph-ui-0.0.1-SNAPSHOT.war")), NewScoper).

test('container loc of paragraph-ui pom.xml, version not set') :-
    paragraph:contloc('paragraph-ui', applfile('pom.xml'), '', file("/opt/paragraph/ParagraphUI/pom.xml"), [ ag('paragraph'), ve(''), ad(paragraph_ui) ], NewScoper),
    memberchk(af(file("/opt/paragraph/ParagraphUI/pom.xml")), NewScoper).


%% parameters defined in paragraph_conf

test('print the parameters (fails if there are any inconsistencies)') :-
    parameters.

test('find the container of parameter pom_xml_version') :-
    parameters(pom_xml_version, C),
    C = pom_xml.

test('find one of the parameters of container pom_xml') :-
    parameters(P, pom_xml),
    P = pom_xml_version.

%% navigate_graph_up

test('navigate graph from pom_xml_version up') :-
    navigate_graph_up(pom_xml_version, 'paragraph-ui', L),
    memberchk(L, [
                  [xpath(//project/version(text)), endswith("/pom.xml"), warfile('paragraph-ui(-version).war')],
                  [xpath(//project/version(text)), applfile("pom.xml")]
                 ]).

test('navigate graph from json_ktext up') :-
    navigate_graph_up(json_ktext, 'paragraph-ui', L),
    L = [jsonget('d/_/t'), endswith(".json"), warfile('paragraph-ui(-version).war')].

%% paramval

test('xpath -> xml file : find the pom version of paragraph project') :-
    paragraph:paramv(pom_xml_version, Val, [ag('paragraph'), ve(''), ad(paragraph_ui)], NewScoper),
    member(af(file("/opt/paragraph/ParagraphUI/pom.xml")), NewScoper),
    Val = '0.0.1-SNAPSHOT'.        % value

test('xpath -> xml file -> warfile : find the pom version of paragraph archives') :-
    paragraph:paramv(pom_xml_version, Val, [ag('paragraph'), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)], _),
    Val = '0.0.1-SNAPSHOT'.        % value

test('xpath -> xpath -> xml file -> warfile : find the parent pom version of paragraph archives') :-
    paragraph:paramv(pom_xml_parent_version, Val, [ag('paragraph'), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)], _),
    Val = '2.3.0.RELEASE'.         % value = spring boot starter parent version

test('phrase -> js file -> warfile : find the angularjs modules in paragraph archives') :-
    paragraph:paramv(angular_module, Val, [ag('paragraph'), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)], _),
    Val = 'paragraph'.             % value = angular module name

test('jsonget -> json file -> warfile : extract values from json in paragraph archives') :-
    paragraph:paramv(json_ktext, Val, [ag('paragraph'), ve('0.0.1-SNAPSHOT'), ad(paragraph_ui_target)], _),
    memberchk(Val, [ "hello", "json" ]). % multiple values extracted via "_"

test('search archive with wd option : find the parent pom version of paragraph archives') :-
    paragraph:paramv(pom_xml_parent_version, Val, [ag('paragraph'), ve('0.0.1-SNAPSHOT'), wd(examples)], _),
    Val = '2.3.0.RELEASE'.         % value = spring boot starter parent version

test('search file with ad option : find the parent pom version of paragraph') :-
    paragraph:paramv(pom_xml_parent_version, Val, [ag('paragraph'), ve(''), ad(paragraph_ui)], _),
    Val = '2.3.0.RELEASE'.         % value = spring boot starter parent version

test('regexp -> txt file : find help url') :-
    paragraph:paramv(help_url, Val, [ag('paragraph'), ve(''), ad(paragraph_ui)], _),
    Val = "https://maven.apache.org/guides/index.html".

:- end_tests(paragraph_paramv).
