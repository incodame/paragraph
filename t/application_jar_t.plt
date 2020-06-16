:- use_module(library(plunit)).
:- begin_tests(paragraph_application_jar).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), working_directory(_, Ps).
:- use_module(paragraph).

% use with options

test('using options, given app, get jar') :-
    application_jar('paragraph-ui', Ve, _, Jar, [ag(paragraph),ve('0.0.1-SNAPSHOT')]),
    Ve = '0.0.1-SNAPSHOT',
    Jar = 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'.

test('using options, given app, no jar in this version') :-
    \+application_jar('paragraph-ui', _, _, _, [ag(paragraph),ve('0.1')]).

test('using options, given jar, get app') :-
    application_jar(App, Ve, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar', [ag(paragraph),ve('0.0.1-SNAPSHOT')]),
    Ve = '0.0.1-SNAPSHOT',
    App = 'paragraph-ui'.

test('using options, given jar, no app in this version') :-
    \+application_jar(_, _, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar', [ag(paragraph),ve('0.1')]).

% search with scoper

my_scoper :-
    paragraph:specify_option(ag(paragraph)),
    paragraph:specify_option(ve('0.0.1-SNAPSHOT')).

no_match_scoper :-
    paragraph:specify_option(ag(paragraph)),
    paragraph:specify_option(ve('0.1')).

test('using scoper, given app, get jar') :-
    search(application_jar('paragraph-ui', Ve, _, Jar), plunit_paragraph_application_jar:my_scoper),
    Ve = '0.0.1-SNAPSHOT',
    Jar = 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'.

test('using scoper, given app, no jar in this version') :-
    \+search(application_jar('paragraph-ui', _, _, _), plunit_paragraph_application_jar:no_match_scoper).

test('using from_list, given app, get jar') :-
    search(application_jar('paragraph-ui', Ve, _, Jar), from_list([ag(paragraph), ve('0.0.1-SNAPSHOT')])),
    Ve = '0.0.1-SNAPSHOT',
    Jar = 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'.

test('using scoper, given jar, get app') :-
    search(application_jar(App, Ve, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'), plunit_paragraph_application_jar:my_scoper),
    Ve = '0.0.1-SNAPSHOT',
    App = 'paragraph-ui'.

test('using scoper, given jar, no app in this version') :-
    \+search(application_jar(_, _, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'), plunit_paragraph_application_jar:no_match_scoper).

test('using from_list, given jar, get app') :-
    search(application_jar(App, Ve, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'), from_list([ag(paragraph), ve('0.0.1-SNAPSHOT')])),
    Ve = '0.0.1-SNAPSHOT',
    App = 'paragraph-ui'.

:- end_tests(paragraph_application_jar).
