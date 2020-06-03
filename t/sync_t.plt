:- use_module(library(plunit)).
:- begin_tests(paragraph_sync).
:- getenv('PARAGRAPH_HOME', Ph), format(atom(Ps), '~w/prolog', [ Ph ]), working_directory(_, Ps).
:- use_module(paragraph).

%% sync resouces

%

requires_tmp_dir :-
    (exists_directory('/tmp/paragraph') -> true ; make_directory('/tmp/paragraph')).

static_url(RFile, Url) :-
    getenv('IPADDRE', IpAddr),
    format(atom(Url), 'http://~w:8089/static/~w', [ IpAddr, RFile ]).

test('download as file', []) :-
    requires_tmp_dir,
    static_url('rest_api.yaml', Url),
    download_as(Url, '/tmp/paragraph/rest_api.yaml', [], 200),
    exists_file('/tmp/paragraph/rest_api.yaml').

test('download as new directory, file', []) :-
    requires_tmp_dir,
    static_url('rest_api.yaml', Url),
    download_as(Url, '/tmp/paragraph/yaml', 'rest_api.yaml', [], 200),
    exists_file('/tmp/paragraph/yaml/rest_api.yaml').

:- end_tests(paragraph_sync).
