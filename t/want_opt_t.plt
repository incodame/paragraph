:- use_module(library(plunit)).
:- begin_tests(paragraph_want_opt).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), working_directory(_, Ps).
:- use_module(paragraph).

test('Opt is var : present in Options') :-
    paragraph:want_opt(ve(Ver), [ve('0.1')]), Ver = '0.1'.

% this test requires user input
%test('Opt is var : not present in Options') :-
%    paragraph:want_opt(ve(Ver), []), Ver = '0.1'.

test('Opt is ground : present in Options with same value') :-
    paragraph:want_opt(ve('0.1'), [ve('0.1'), ve('0.2')]).

test('Opt is ground : present in Options with another value') :-
    \+paragraph:want_opt(ve('0.1'), [ve('0.2')]).

my_consumer_once(Ver, Options) :-
    paragraph:want_opt(ve(Ver), Options).

my_consumer_twice(Ver1, Ver2, Options) :-
    paragraph:want_opt(ve(Ver1), Options),
    paragraph:want_opt(ve(Ver2), Options).

my_scoper :-
    paragraph:specify_option(ve('0.1')),
    paragraph:specify_option(ve('0.2')).

test('want_opt in search, called once') :-
    findall(Ver, paragraph:search(
                                   plunit_paragraph_want_opt:my_consumer_once(Ver, []),
                                   plunit_paragraph_want_opt:my_scoper
                               ), L),
    L = [ '0.1' ].

test('want_opt in search, called twice') :-
    findall((Ver1, Ver2), paragraph:search(
                                   plunit_paragraph_want_opt:my_consumer_twice(Ver1, Ver2, []),
                                   plunit_paragraph_want_opt:my_scoper
                               ), L),
    L = [ ('0.1', '0.2') ].

:- end_tests(paragraph_want_opt).
