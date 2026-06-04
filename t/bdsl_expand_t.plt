:- use_module(library(plunit)).
:- begin_tests(paragraph_bdsl_expand).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), writeln(Ps),working_directory(_, Ps).
:- use_module('../prolog/paragraph_bdsl').

setup_bdsl_test_data :-
    retract_bdsl(test_expand),
    assert_bdsl(test_expand, [
        fr(tsconfig) :> f('tsconfig.json'),
        fr(tsconfig) :> f('tsconfig.app.json'),
        fr(tsconfig) -+ s([name='ts_option', loc=jsonget('compilerOptions/:'), doc="typescript compiler option"])
    ]).

cleanup_bdsl_test_data :-
    retract_bdsl(test_expand).

test('expand fr(file_ref) -+ into one f(file) per mapping',
     [ setup(setup_bdsl_test_data), cleanup(cleanup_bdsl_test_data) ]) :-
    '-+'(f('tsconfig.json'), s([name='ts_option', loc=jsonget('compilerOptions/:'), doc="typescript compiler option"])),
    '-+'(f('tsconfig.app.json'), s([name='ts_option', loc=jsonget('compilerOptions/:'), doc="typescript compiler option"])),
    not('-+'(fr(tsconfig), s([name='ts_option', loc=jsonget('compilerOptions/:'), doc="typescript compiler option"]))).

:- end_tests(paragraph_bdsl_expand).