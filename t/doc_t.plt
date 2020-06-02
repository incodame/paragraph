:- use_module(library(plunit)).
:- begin_tests(paragraph_doc).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), working_directory(_, Ps).
:- use_module(paragraph).

%% help system (doc, showdoc)

% all exported predicates are present in doc
test('doc exported predicates', []) :-
    exported_predicates(paragraph, PredicateList),
    forall(member(Predicate, PredicateList), (doc(Predicate, _, Texts), length(Texts, L), L >= 1)).

:- end_tests(paragraph_doc).
