%%
%% BDsl definitions for node project
%%
:- use_module('../paragraph_bdsl').
:- op(500, xfy, ':>').
:- op(400, xfy, '-+').

:- assert_bdsl([
  
  % root dev dir
  pr('nodeproj') :> p([ '/opt', 'paragraph', 'ParagraphAng', '$DevRoot' ]),

  % build config
  f('package.json') :> pr('nodeproj'), 
  f('package.json') -+ s([name='node_dep', loc=jsonget('dependencies/:'), doc="node project dependency"]),
  s('node_dep') -+ i([name='node_dep_name', loc=jsonget('key()'), doc="node dependency name"]),
  s('node_dep') -+ i([name='node_dep_ver', loc=jsonget('val()'), doc="node dependency version"])

]).  

% end of bdsl
