:- use_module(library(plunit)).
:- begin_tests(paragraph_application_jar).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), working_directory(_, Ps).
:- use_module(paragraph).

% application_jar and and application_java_class with options

test('given app, get jar from web archive') :-
    application_jar('paragraph-ui', Ve, _, Jar, [ag(paragraph),ve('0.0.1-SNAPSHOT'),wd(examples)]),
    Ve = '0.0.1-SNAPSHOT',
    Jar = 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'.

%% test('given app, no jar in this version') :-
%%     \+application_jar('paragraph-ui', _, _, _, [ag(paragraph),ve('0.1'),wd(examples)]).

test('given jar from web archive, get app') :-
    application_jar(App, Ve, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar', [ag(paragraph),ve('0.0.1-SNAPSHOT'),wd(examples)]),
    Ve = '0.0.1-SNAPSHOT',
    App = 'paragraph-ui'.

%% test('given jar, no app in this version') :-
%%     \+application_jar(_, _, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar', [ag(paragraph),ve('0.1'),wd(examples)]).

test('get application *.jar from app directory') :-
    application_jar(AppId, Ver, JarFile, [ag(paragraph),ve(''),ad(pv_dependencies)]),
    Ver = '',
    AppId = 'paragraph-verticles',
    JarFile = "/opt/paragraph/ParagraphVerticles/target/dependency/ant-antlr-1.10.7.jar".

test('get application classes from jars in web archive') :-
    application_java_class(AppId, Ver, _, _, ClassRelPath, [ag(paragraph),ve('0.0.1-SNAPSHOT'),wd(examples)]),
    Ver = '0.0.1-SNAPSHOT',
    AppId = 'paragraph-ui',
    ClassRelPath = rpath('com/fasterxml/jackson/core/Base64Variant.class').

% FIXME
test('get application classes from jars in app directory') :-
    application_java_class(AppId, Ver, JarFilePath, ClassRelPath, [ag(paragraph),ve(''),ad(pv_dependencies)]),
    Ver = '',
    AppId = 'paragraph-verticles',
    JarFilePath = "/opt/paragraph/ParagraphVerticles/target/dependency/ant-antlr-1.10.7.jar",
    ClassRelPath = rpath('org/apache/tools/ant/AntTypeDefinition.class').

% search with scoper - new implementation required

my_scoper :-
    paragraph:specify_option(ag(paragraph)),
    paragraph:specify_option(ve('0.0.1-SNAPSHOT')),
    paragraph:specify_option(wd(examples)).

no_match_scoper :-
    paragraph:specify_option(ag(paragraph)),
    paragraph:specify_option(ve('0.1')),
    paragraph:specify_option(wd(examples)).

%% test('using scoper, given app, get jar') :-
%%     search(application_jar('paragraph-ui', Ve, _, Jar), plunit_paragraph_application_jar:my_scoper),
%%     Ve = '0.0.1-SNAPSHOT',
%%     Jar = 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'.

%% test('using scoper, given app, no jar in this version') :-
%%     \+search(application_jar('paragraph-ui', _, _, _), plunit_paragraph_application_jar:no_match_scoper).

%% test('using from_list, given app, get jar') :-
%%     search(application_jar('paragraph-ui', Ve, _, Jar), from_list([ag(paragraph), ve('0.0.1-SNAPSHOT'),wd(examples)])),
%%     Ve = '0.0.1-SNAPSHOT',
%%     Jar = 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'.

%% test('using scoper, given jar, get app') :-
%%     search(application_jar(App, Ve, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'), plunit_paragraph_application_jar:my_scoper),
%%     Ve = '0.0.1-SNAPSHOT',
%%     App = 'paragraph-ui'.

%% test('using scoper, given jar, no app in this version') :-
%%     \+search(application_jar(_, _, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'), plunit_paragraph_application_jar:no_match_scoper).

%% test('using from_list, given jar, get app') :-
%%     search(application_jar(App, Ve, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'), from_list([ag(paragraph), ve('0.0.1-SNAPSHOT'),wd(examples)])),
%%     Ve = '0.0.1-SNAPSHOT',
%%     App = 'paragraph-ui'.

:- end_tests(paragraph_application_jar).
