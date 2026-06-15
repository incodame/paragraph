:- use_module(library(plunit)).
:- use_module(library(xpath)).
:- begin_tests(paragraph_paramv).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), working_directory(_, Ps).
:- use_module('../prolog/paragraph').


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

%% paramv is the rewrite of paramval

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

test('regexp -> txt file : find book url') :-
    paragraph:paramv(doc_url, Val, [ag('paragraph'), ve(''), ad(paragraph_main)], _),
    Val = "https://github.com/incodame/paragraph/blob/master/doc/book.org".

:- end_tests(paragraph_paramv).
