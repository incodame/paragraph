:- use_module(library(plunit)).
:- begin_tests(paragraph_paramval).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), working_directory(_, Ps).
:- use_module(paragraph).

test('parse package type, version and app') :-
    paragraph:package_version('paragraph-ui-0.1.war', war, '0.1', 'paragraph-ui').

test('container loc of paragraph-ui app archive') :-
    paragraph:contloc('paragraph-ui', warfile('paragraph-ui-0.0.1-SNAPSHOT.war'), '0.0.1-SNAPSHOT', "file:/tmp/paragraph/paragraph-ui-0.0.1-SNAPSHOT.war", [], [ ag('paragraph'), ve('0.0.1-SNAPSHOT')]).

%test('find the pom dependencies of a built artifact') :-
%    from_list([[ag('paragraph'), ve('0.0.1-SNAPSHOT')]]) ??? paramval(pom_xml_version, Version, Val),
%    Version = '0.0.1-SNAPSHOT',
%    Val = '0.0.1-SNAPSHOT'.

:- end_tests(paragraph_paramval).
