:- module(java_dcg, [java_class_decl//4, java_class_file_content//1]).
:- use_module(library(dcg/basics)).

java_class_decl(class(C,As), Fields, Constructors, Methods) -->
    java_annotations(As), "public class ", string_without(" ", C), { writeln(C) }, 
    " {", blanks,
    java_field_decls(Fields), blanks,
    java_constructor_decls(Constructors), blanks,
    java_method_decls(Methods), blanks,
    "}".

java_field_decls([]) --> [].
java_field_decls([Field|Fields]) -->
    java_field_decl(Field), blanks,
    java_field_decls(Fields).

java_field_decl(field(F,As)) -->  blanks, java_annotations(As), "private ", string(F), ";", blanks, { writeln(F) }.

java_annotations([]) --> [].
java_annotations([Annotation|Annotations]) -->
    java_annotation(Annotation), blanks,
    java_annotations(Annotations).

java_annotation(A) --> "@", string(A), blanks.

java_constructor_decls([]) --> [].
java_constructor_decls([Constructor|Constructors]) -->
    java_constructor_decl(Constructor), blanks,
    java_constructor_decls(Constructors).

java_method_decls([]) --> [].
java_method_decls([Method|Methods]) -->
    java_method_decl(Method), blanks,
    java_method_decls(Methods).

java_constructor_decl(C) --> blanks, "public ", string(C), "(", java_parameter_list(_), ")", blanks, "{", blanks, "}", blanks.

java_parameter_list([]) --> [].
java_parameter_list([P|Ps]) --> java_parameter(P), ",", java_parameter_list(Ps).
java_parameter(P) --> blanks, "final ", string(P), blanks.

java_method_decl(M) --> blanks, "public ", string(M), "(", java_parameter_list(_), ")", blanks, "{", blanks, "}", blanks.

java_class_file_content(Class) -->
    java_package_decl(_), blanks,
    java_import_statements(_), blanks,
    java_class_decl(Class, _, _, _).

java_package_decl(_) --> "package", blanks, string(_), ";", blanks.
java_package_decl(_) --> [].

java_import_statements([]) --> [].
java_import_statements([Import|Imports]) -->
    java_import_statement(Import), blanks,
    java_import_statements(Imports).

java_import_statement(Import) --> "import", blanks, string(Import), ";", blanks.

test1(Class) :-
    Data = "public class MyClass { private String name; }",
    string_codes(Data, Codes),
    phrase(java_class_decl(class(Class, []), [ field('name', []) ], _, _), Codes).

test2(Class) :-
    phrase_from_file(java_class_file_content(class(Class, _)), '/opt/paragraph/ParagraphUI/src/main/java/org/incodame/paragraph/sample/webapp/demo/Parameter.java').

test3(Class) :-
    Data = "package com.example; import java.util.*; public class MyClass { private String name; }",
    string_codes(Data, Codes),
    phrase(java_class_file_content(class(Class, _)), Codes).


