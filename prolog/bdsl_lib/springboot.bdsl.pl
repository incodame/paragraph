%%
%% BDsl definitions for spring boot project
%%
:- use_module('../paragraph_bdsl').
:- op(500, xfy, ':>').
:- op(400, xfy, '-+').

:- assert_bdsl(springboot, [
  
  % root dev dir
  pr('sbRoot') :> p([ '/opt', 'paragraph', 'ParagraphUI' ]),
  pr('sbTarget') :> p([ '/opt', 'paragraph', 'app:ParagraphUI', target ]),
  
  % build config
  f('pom.xml') :> pr('sbRoot'),
  f('pom.xml') -+ s([name='mvn_dep', loc=xpath('//project/dependencies/dependency'), doc="maven dependency"]),
  s('mvn_dep') -+ i([name='mvn_dep_gid', loc=xpath('//artifactId(text)'), doc="maven dependency group id"]),
  s('mvn_dep') -+ i([name='mvn_dep_aid', loc=xpath('//artifactId(text)'), doc="maven dependency artifact id"]),
  s('mvn_dep') -+ i([name='mvn_dep_ver', loc=xpath('//version(text)'), doc="maven dependency version"]),

  % archives
  z('paragraph-ui-(version).war') :> pr('sbTarget'),
  zr(sb_archive) :> z('paragraph-ui-(version).war'),
  zr(sb_archive) -+ f([name='pom.xml', loc=endswith('/pom.xml'), doc="maven project xml"])

]).  

% end of bdsl
