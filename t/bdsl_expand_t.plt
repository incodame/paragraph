:- use_module(library(plunit)).
:- begin_tests(paragraph_bdsl_expand).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), writeln(Ps),working_directory(_, Ps).
:- use_module('../prolog/paragraph_bdsl').

% application file reference
setup_bdsl_test_data_fr :-
    retract_bdsl(test_expand_fr),
    assert_bdsl(test_expand_fr, [
        fr(tsconfig) :> f('tsconfig.json'),
        fr(tsconfig) :> f('tsconfig.app.json'),
        fr(tsconfig) -+ s([name='ts_option', loc=jsonget('compilerOptions/:'), doc="typescript compiler option"])
    ]).

cleanup_bdsl_test_data_fr :-
    retract_bdsl(test_expand_fr).

% application archive reference
setup_bdsl_test_data_zr :-
    retract_bdsl(test_expand_zr),
    assert_bdsl(test_expand_zr, [
        z('paragraph-ui-0.0.1-SNAPSHOT.war') :> pr('sbTarget'),
        zr(sb_archive) :> z('paragraph-ui-0.0.1-SNAPSHOT.war'),
        zr(sb_archive) -+ f([name='pom.xml', loc=endswith('/pom.xml'), doc="maven project xml"])
    ]).

cleanup_bdsl_test_data_zr :-
    retract_bdsl(test_expand_zr).

% test expansion for application file reference
test('expand fr(file_ref) -+ into one f(file) per mapping',
     [ setup(setup_bdsl_test_data_fr), cleanup(cleanup_bdsl_test_data_fr) ]) :-
    '-+'(f('tsconfig.json'), s([name='ts_option', loc=jsonget('compilerOptions/:'), doc="typescript compiler option"])),
    '-+'(f('tsconfig.app.json'), s([name='ts_option', loc=jsonget('compilerOptions/:'), doc="typescript compiler option"])),
    not('-+'(fr(tsconfig), s([name='ts_option', loc=jsonget('compilerOptions/:'), doc="typescript compiler option"]))).

% test expansion for application archive reference
test('expand zr(archive_ref) -+ into one z(archive) per mapping',
     [ setup(setup_bdsl_test_data_zr), cleanup(cleanup_bdsl_test_data_zr) ]) :-
    '-+'(z('paragraph-ui-0.0.1-SNAPSHOT.war'), f([name='pom.xml', loc=endswith('/pom.xml'), doc="maven project xml"])),
    not('-+'(zr(sb_archive), f([name='pom.xml', loc=endswith('/pom.xml'), doc="maven project xml"]))).

:- end_tests(paragraph_bdsl_expand).