:- module(js_dcg, [ angular_module_decl//1 ]).
:- use_module(library(dcg/basics)).

angular_module_decl(M) -->
    string(_),
    %"angular.module(", string(M), ", [])",
    angular_module(M),
    remainder(_).

angular_module(M) --> "angular.module", { string_codes("angular.module", M) }.

test1(Module) :-
    Data = "var app = angular.module(\"Paragraph\", []);",
    string_codes(Data, Codes),
    phrase(angular_module_decl(Module), Codes).

test2(Module) :-
    phrase_from_file(angular_module_decl(Module), '/opt/paragraph/ParagraphUI/src/main/resources/static/js/paragraph.js').
