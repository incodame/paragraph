:- use_module(library(plunit)).
:- begin_tests(paragraph_doc).
:- working_directory(_, '/opt/paragraph/prolog').
:- use_module(paragraph).

%% help system (doc, showdoc)

% all exported predicates are present in doc
test('doc exported predicates', []) :-
    exported_predicates(paragraph, PredicateList),
    forall(member(Predicate, PredicateList), (doc(Predicate, _, Texts), length(Texts, L), L >= 1)).

:- end_tests(paragraph_doc).
