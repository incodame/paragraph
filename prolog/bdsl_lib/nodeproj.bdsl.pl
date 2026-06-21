%%
%% BDsl definitions for node project
%%
:- use_module('../paragraph_bdsl').
:- op(500, xfy, ':>').
:- op(400, xfy, '-+').

:- assert_bdsl(nodeproj, [
  
  % root dev dir
  pr('nodeproj') :> p([ '/opt', 'paragraph', 'ParagraphAng', '$DevRoot' ]),
  pr('appdir') :> p([ '/opt', 'paragraph', 'ParagraphAng', 'paragraph-monorepo', apps, '$App' ]),

  % build config
  f('package.json') :> pr('nodeproj'), 
  f('package.json') -+ s([name='node_dep', loc=jsonget('dependencies/:'), doc="node project dependency", owl="child_infos()"]),
  s('node_dep') -+ i([name='node_dep_name', loc=jsonget('key()'), doc="node dependency name", owl=":packageName"]),
  s('node_dep') -+ i([name='node_dep_ver', loc=jsonget('val()'), doc="node dependency version", owl=":packageVersion"]),

  % typescript config
  f('tsconfig.json') :> pr('appdir'),
  f('tsconfig.app.json') :> pr('appdir'),
  fr(tsconfig) :> f('tsconfig.json'),
  fr(tsconfig) :> f('tsconfig.app.json'),
  fr(tsconfig) -+ s([name='ts_option', loc=jsonget('compilerOptions/:'), doc="typescript compiler option"])

]).  

% end of bdsl
