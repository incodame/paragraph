/*
 * paragraph commons
 */
:- module(paragraph_commons, [file_match/5, version_tok/3]).

list([])     --> [].
list([L|Ls]) --> [L], list(Ls).

conc_of(Prefix, Suffix) --> list(Prefix), "version", list(Suffix).

version_tok(VersionTokenStr, VePrefixStr, VeSuffixStr) :-
    string_codes(VersionTokenStr, VersionTokenCodes),
    phrase(conc_of(VePrefixL, VeSuffixL), VersionTokenCodes),
    string_codes(VePrefixStr, VePrefixL),
    string_codes(VeSuffixStr, VeSuffixL).

file_match(FileTest, FileList, File, FileType, Version) :-
    ( sub_atom(FileTest, _, 1, _, '(') ->
        atomic_list_concat([Prefix, RestWithClosing], '(', FileTest),
        atomic_list_concat([VersionToken, Suffix], ')', RestWithClosing),
        version_tok(VersionToken, VePrefix, VeSuffix),
        ( ground(Version) ->
            ( Version == '' ->
                atomic_list_concat([Prefix, Suffix], '', FileAtom)
            ;
                atomic_list_concat([Prefix, VePrefix, Version, VeSuffix, Suffix], '', FileAtom)
            ),
            File = FileAtom
        ;
            true
        ),
        member(File, FileList),
        ( \+ ground(Version) ->
            atom_concat(Prefix, Rest, File),
            atom_concat(VersionTokenExtract, Suffix, Rest),
            atom_length(VersionTokenExtract, VtLen),
            ( VtLen =:= 0 ->
                Version = ''
            ;
                atom_concat(VePrefix, VeRest, VersionTokenExtract),
                atom_concat(Version, VeSuffix, VeRest)
            )
        ;
            true
        )
    ;   member(FileTest, FileList),
        File = FileTest,
        Version = ''
    ;   atom_concat('*.', FileType, FileTest), atom_concat('*', Suffix, FileTest),
        member(File, FileList), 
        atom_concat(_, Suffix, File)
    ;
        ground(FileType),
        atomic_list_concat([FileTest, '.', FileType], FileName), 
        member(FileName, FileList), 
        File = FileName, 
        Version = ''
    ).
