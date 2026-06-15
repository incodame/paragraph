:- use_module(library(plunit)).
:- begin_tests(commons).
:- use_module('../prolog/paragraph_commons').

%% archive match

% TODO - broken, should fail
test('archive match, version is known') :-
    FileList = [ '.', '..', 'paragraph-ui-0.1.war', 'paragraph-ui-0.2.war' ],
    assertion(\+ paragraph_commons:file_match('paragraph-ui-0.9.war', FileList, 'paragraph-ui-0.1.war', war, '0.1')).

test('archive match, version unknown') :-
    FileList = [ '.', '..', 'paragraph-ui-0.1.war', 'paragraph-ui-0.2.war' ],
    (paragraph_commons:file_match('paragraph-ui-(version).war', FileList, 'paragraph-ui-0.1.war', FileType, Ve1),
     Ve1 = '0.1',
     FileType = war
    ),
    (paragraph_commons:file_match('paragraph-ui-(version).war', FileList, 'paragraph-ui-0.2.war', FileType, Ve2),
     Ve2 = '0.2',
     FileType = war
    ).

test('archive match, with or without version') :-
    FileList = [ '.', '..', 'paragraph-ui.war', 'paragraph-ui-0.2.war' ],
    paragraph_commons:file_match('paragraph-ui(-version).war', FileList, 'paragraph-ui.war', war, ''),
    paragraph_commons:file_match('paragraph-ui(-version).war', FileList, 'paragraph-ui-0.2.war', war, '0.2').

test('archive match, without version') :-
    FileList = [ '.', '..', 'paragraph-ui.war', 'paragraph-ui-0.2.war' ],
    paragraph_commons:file_match('paragraph-ui.war', FileList, 'paragraph-ui.war', war, '').

test('archive match, wilcard') :-
    FileList = [ '.', '..', 'a.jar', 'b.jar' ],
    paragraph_commons:file_match('*.jar', FileList, 'a.jar', jar, ''),
    paragraph_commons:file_match('*.jar', FileList, 'b.jar', jar, '').

%% file match

test('file match, without version') :-
    FileList = [ '.', '..', 'pom.xml', '' ],
    paragraph_commons:file_match('pom.xml', FileList, 'pom.xml', pom, '').

test('file match, wilcard') :-
    FileList = [ '.', '..', 'a.xml', 'b.xml' ],
    paragraph_commons:file_match('*.xml', FileList, 'a.xml', xml, ''),
    paragraph_commons:file_match('*.xml', FileList, 'b.xml', xml, '').

%% version tok

test('version tok without wrappers') :-
    paragraph_commons:version_tok(version, '', '').

test('version tok with parentheses') :-
    paragraph_commons:version_tok('(version)', '(', ')').

:- end_tests(commons).
