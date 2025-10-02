:- use_module(library(plunit)).
:- use_module(library(xpath)).
:- begin_tests(paragraph_paramv_isa).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), working_directory(_, Ps).
:- use_module(paragraph).

%% paramv for parameter with isa clause

test('class annotation -> java file : read a class annotation') :-
    paragraph:paramv(jpa_database_table, Val, [ag('paragraph'), ve(''), ad(paragraph_ui)], NewScoper),
    member(af(file("/opt/paragraph/ParagraphUI/src/main/java/org/incodame/paragraph/sample/webapp/demo/Parameter.java")), NewScoper),
    Val = '@DatabaseEntity(name = "parameter", 
        tableName = "parameter", 
        description = "Parameter entity for storing parameters with name, location, and documentation.")'.        % value

:- end_tests(paragraph_paramv_isa).
