%%
%% Retract BDsl facts by source id
%%
:- use_module('../paragraph_bdsl').

retractall_bdsl :-
    paragraph_bdsl:retract_bdsl_all.

:- retractall_bdsl.

% end of bdsl
