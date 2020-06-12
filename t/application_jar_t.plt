:- use_module(library(plunit)).
:- begin_tests(paragraph_application_jar).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), working_directory(_, Ps).
:- use_module(paragraph).

test('given app, get jar') :-
    application_jar('paragraph-ui', Ve, _, Jar, [ag(paragraph),ve('0.0.1-SNAPSHOT')]),
    Ve = '0.0.1-SNAPSHOT',
    Jar = 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar'.

test('given app, no jar in this version') :-
    \+application_jar('paragraph-ui', _, _, _, [ag(paragraph),ve('0.1')]).

test('given jar, get app') :-
    application_jar(App, Ve, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar', [ag(paragraph),ve('0.0.1-SNAPSHOT')]),
    Ve = '0.0.1-SNAPSHOT',
    App = 'paragraph-ui'.

test('given jar, no app in this version') :-
    \+application_jar(_, _, _, 'WEB-INF/lib/spring-boot-2.3.0.RELEASE.jar', [ag(paragraph),ve('0.1')]).

:- end_tests(paragraph_application_jar).
