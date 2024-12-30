:- module(java_dcg, [java_class_decl//4, java_field_decl//1, java_class_file_content//1]).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(list_util)).


java_class_decl(class(CStr,As), Fields, Constructors, Methods) -->
    java_annotations(As), java_visibility_modifier(_), "class ", string_without(" ", C), 
    { string_codes(CStr, C), format(string(Class), "class: ~w", [CStr]), writeln(Class) }, 
    " {", blanks,
    java_field_decls(Fields), blanks,
    java_constructor_decls(Constructors), { replicate(_, CStr, Constructors) }, blanks,
    java_method_decls(Methods), { include(=(CStr), Methods, []) }, blanks,
    "}", !.

java_field_decls([]) --> [].
java_field_decls([Field|Fields]) -->
    java_field_decl(Field), blanks,
    java_field_decls(Fields).

java_field_decl(field(FStr,As)) -->  
    java_annotations(As), 
    java_visibility_modifier(_), 
    string_without(";", F), ";", blanks, 
    { string_codes(FStr, F), format(string(Field), "field: ~w", [FStr]), writeln(Field) }, !.

java_visibility_modifier(Modifier) --> ( "public", { ModifierStr = "public"} 
                                       | "private", { ModifierStr = "private"} 
                                       | "protected", { ModifierStr = "protected"}
                                       ), blanks, { string_codes(ModifierStr, Modifier) }.

java_annotations([]) --> [].
java_annotations([Annotation|Annotations]) -->
    java_annotation(Annotation), blanks,
    java_annotations(Annotations).

java_annotation(AStr) --> 
    "@", string_without("\n", A), blanks,
    { string_codes(AStr, A), format(string(Anno), "annotation: ~w", [AStr]), writeln(Anno) }, !.

java_constructor_decls([Constructor|Constructors]) -->
        java_constructor_decl(Constructor), blanks,
        java_constructor_decls(Constructors).
java_constructor_decls([]) --> [].

java_method_decls([]) --> [].
java_method_decls([Method|Methods]) -->
    java_method_decl(Method), blanks,
    java_method_decls(Methods).

java_constructor_decl(CStr) --> 
    blanks, 
    java_visibility_modifier(_), 
    string_without("\n( ", C), "(", java_parameter_list(_), ")", blanks, 
    "{", balanced_cbrackets, "}", blanks, 
    { string_codes(CStr, C), format(string(Cons), "constructor: ~w", [CStr]), writeln(Cons) }.

java_parameter_list([]) --> [].
java_parameter_list([P]) --> java_parameter(P).
java_parameter_list([P|Ps]) --> java_parameter(P), ",", java_parameter_list(Ps).
java_parameter(PStr) --> 
    blanks, 
    final_p, string_without("),", P), blanks, 
    { string_codes(PStr, P), format(string(Param), "param: ~w", [PStr]), writeln(Param) }.

final_p --> "final", blanks.
final_p --> []. 

% balanced curly brackets
balanced_cbrackets --> [].
balanced_cbrackets --> string_without("{}", _).
balanced_cbrackets -->
    balanced_cbrackets,
	"{",
	balanced_cbrackets,
	"}",
    balanced_cbrackets.

java_method_decl(MStr) --> 
    blanks, 
    java_visibility_modifier(_), 
    string_without("\n(", M), "(", java_parameter_list(_), ")", blanks, 
    "{", balanced_cbrackets, "}", blanks, 
    { string_codes(MStr, M), format(string(Method), "method: ~w", [MStr]), writeln(Method) }.

java_class_file_content(Class) -->
    java_package_decl(_), blanks,
    java_import_statements(_), blanks,
    java_class_decl(Class, _, _, _).

java_package_decl(_) --> "package", blanks, string_without(";", _), ";", blanks.
java_package_decl(_) --> [].

java_import_statements([]) --> [].
java_import_statements([Import|Imports]) -->
    java_import_statement(Import), blanks,
    java_import_statements(Imports).

java_import_statement(Import) --> "import", blanks, string_without(";", Import), ";", blanks.

test1(Class) :-
    Data = "public class MyClass { private String name; }",
    string_codes(Data, Codes),
    phrase(java_class_decl(class(Class, []), [ field("String name", []) ], _, _), Codes).

test2(Class) :-
    phrase_from_file(java_class_file_content(class(Class, _)), '/opt/paragraph/ParagraphUI/src/main/java/org/incodame/paragraph/sample/webapp/demo/Parameter.java').

test3(Class) :-
    Data = "package com.example; import java.util.*; public class MyClass { private String name; }",
    string_codes(Data, Codes),
    phrase(java_class_file_content(class(Class, _)), Codes).


