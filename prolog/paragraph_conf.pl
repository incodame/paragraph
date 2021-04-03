:- module(paragraph_conf, [ application_group/3, application/4, directory_alias/2, directory_alias/3, paramloc/5, paramloc/6, app_archive/4, app_file/4, search_option/3, transform_val/3 ]).
:- use_module(library(xpath)).
:- use_module(library(yaml)).
%%
%% Paragraph Configuration
%%
%% - edit your project configuration in paragraph.yaml
%%

paragraph_graph('paragraph.yml').

%% application and groups (ordered by application groups)


application_group(build, paragraph, 'org.incodame.paragraph').

:- table pgraph/1.
pgraph(YamlDoc) :-
    yaml_read('/opt/paragraph/paragraph.yml', YamlDoc).

application(app, paragraph, AppShortName, AppProps) :-
    pgraph(yaml{paragraph:yaml{apps:AppList, graph:_}}),
    member(App, AppList),
    select_dict(yaml{name:AppShortNameStr, build:AppBuildStr}, App, _),
    atom_string(AppShortName, AppShortNameStr),
    atom_string(AppBuild, AppBuildStr),
    %AppShortName = App.get(name),
    %AppBuild = App.get(build),
    AppProps = [ build(AppBuild) ].

%% application(app, 'paragraph-ui',        paragraph, [ build(maven) ]).
%% application(app, 'paragraph-verticles', paragraph, [ build(maven) ]).
%% application(app, 'paragraph_conf',      paragraph, []).
%% application(app, 'paragraph',           paragraph, []).

%% parameters (ordered alphabetically), document for your own benefit !
%% {Path, To, LocTerm}/(pgraph(yaml{paragraph:yaml{apps:_, graph:Graph}}), LocTerm = Graph.get(Path/To/loc)).
%% Path = file,
%% To = application_xml,
%% LocTerm = "rpath(\"META-INF/application.xml\")" ;
%% Path = param,
%% To = batch_java_home,
%% LocTerm = "regexp(\"^(?<V1L>.*JAVA_HOME=.*)$\")" ;
%% Path = param,
%% To = context_root,
%% LocTerm = "xpath(//'context-root'(text))" ;

% application parameters
paramloc(App, Param, Container, LocTerm, ContLocTerm, ParamProps) :-
    (app_archive(_, App, Container, _) ; app_file(_, App, Container, _)),
    pgraph(yaml{paragraph:yaml{apps:_, graph:Graph}}),
    LocExprStr = Graph.get(file/Param/loc),
    location_term(LocExprStr, LocTerm),
    Doc = Graph.get(file/Param/doc),
    ContLocTerm = 'resolved from directory aliases',
    ParamProps = [ doc(Doc) ].

% generic parameters
paramloc(Param, Container, LocTerm, ContLocTerm, ParamProps) :-
    pgraph(yaml{paragraph:yaml{apps:_, graph:Graph}}),
    LocExprStr = Graph.get(param/Param/loc),
    location_term(LocExprStr, LocTerm),
    Doc = Graph.get(param/Param/doc),
    parent_param(Param, _, Container, ContLocTerm),
    ParamProps = [ doc(Doc) ].

:- table parent_param/4.
parent_param(Param, ParentType, Parent, ParentLocTerm) :-
    pgraph(yaml{paragraph:yaml{apps:_, graph:Graph}}),
    ChildList = Graph.get(ParentType/Parent/params),
    member(Child, ChildList),
    dict_keys(Child, Keys),
    member(Param, Keys),
    ParentLocExprStr = Graph.get(ParentType/Parent/loc),
    location_term(ParentLocExprStr, ParentLocTerm).

location_term(LocExprStr, LocTerm) :-
    atom_string(LocExpr, LocExprStr),
    atomic_list_concat(LocList, ', ', LocExpr),
    member(LocAtom, LocList),
    term_to_atom(LocTerm, LocAtom).

%% paramloc(App, application_xml,  WebAppEar,       rpath("META-INF/application.xml"), [ doc(Doc) ]) :-
%%     app_archive(ear, App, WebAppEar, _),
%%     Doc = "The application.xml deployment descriptor contains one module element for each module in the Enterprise Archive file.".
%% paramloc(App, batch_cmd,        BatchAppZip,     endswith(".cmd"), [ doc(Doc) ]) :-
%%     app_archive(zip, App, BatchAppZip, _),
%%     Doc = "Any batch cmd file within an application's zip".
%% paramloc(batch_java_home,  batch_cmd,       regexp("^(?<V1L>.*JAVA_HOME=.*)$"), [ doc("JAVA_HOME definition within a .cmd") ]).
%% paramloc(App, batch_properties, BatchAppZip,     endswith(".properties"), [ doc(Doc) ]) :-
%%     app_archive(zip, App, BatchAppZip, _),
%%     Doc = "Any properties file within an application's zip".
%% paramloc(context_root,     application_xml, xpath(//'context-root'(text)),      [ doc("context root of a Java EE web application")]).
%% paramloc(App, help_md, no_parent, lfile('HELP.md'), [ doc(Doc) ]) :-
%%     app_file(md, App, 'HELP.md', [ doc(Doc) ]).
%% paramloc(help_url,  help_md,       regexp("[(](?<V1L>.*)[)]$"), [ doc("help resource url") ]).
%% paramloc(App, persistence_xml,  WebAppEar,       endswith("persistence-context.xml"), [ doc(Doc) ]) :-
%%     app_archive(ear, App, WebAppEar, _),
%%     Doc = "Main configuration of JPA".
%% paramloc(pom_xml_parent,   war_pom_xml,     xpath(//project/parent), [ doc("pom.xml parent in app archive") ]).
%% paramloc(pom_xml_parent,   app_pom_xml,     xpath(//project/parent), [ doc("pom.xml parent in app file") ]).
%% paramloc(pom_xml_parent_artifact_id,  pom_xml_parent,     xpath(//artifactId(text)), [ doc("pom.xml parent artifactId") ]).
%% paramloc(pom_xml_parent_group_id,     pom_xml_parent,     xpath(//groupdId(text)),   [ doc("pom.xml parent groupdId") ]).
%% paramloc(pom_xml_parent_version,      pom_xml_parent,     xpath(//version(text)),    [ doc("pom.xml parent version") ]).
%% paramloc(pom_xml_version,  war_pom_xml,     xpath(//project/version(text)), [ doc("pom.xml version") ]).
%% paramloc(App, war_pom_xml,      WebAppWar,       endswith("/pom.xml"),     [ doc(Doc) ]) :-
%%     app_archive(war, App, WebAppWar, _),
%%     Doc = "pom.xml of a Web Archive built by Maven".
%% paramloc(App, app_pom_xml,      no_parent,       lfile('pom.xml'), [ doc(Doc) ]) :-
%%     app_file(pom, App, 'pom.xml', [ doc(Doc) ]).

%% application archives ordered alphabetically (ear, war, jar, zip)

app_archive(war,  'paragraph-ui',           'paragraph-ui(-version).war', []).
app_archive(jar,  'paragraph-verticles',    'paragraph-verticles-(version)-fat.jar', []).

%% application files

app_file(pom, AppId, 'pom.xml',  [ doc("application pom.xml") ]) :-
    application(app, _ApplicationGroup, AppId, AppOpts), memberchk(build(maven), AppOpts).

% add specific files here
app_file(md,  'paragraph-ui', 'HELP.md', [ doc("application help") ]).
app_file(xml, 'paragraph-verticles',    '*.xml', []).
app_file(jar, 'paragraph-verticles',    '*.jar', []).

%% search options: term, printable text and default way to get it

search_option(ag(ApplicationGroup), "Application group", read(ApplicationGroup)).
search_option(ve(Version),          "Version", read(Version)).
search_option(wd(WorkDirectory),    "Work directory alias", read(WorkDirectory)).
search_option(ad(AppDirectory),     "App directory alias",  read(AppDirectory)).

%% global directory aliases

directory_alias(examples, '/opt/paragraph/examples').

%% application specific directory aliases

directory_alias('paragraph-ui', paragraph_ui, '/opt/paragraph/ParagraphUI').
directory_alias('paragraph-ui', paragraph_ui_target, '/opt/paragraph/ParagraphUI/target').
directory_alias('paragraph-verticles', paragraph_verticles, '/opt/paragraph/ParagraphVerticles').
directory_alias('paragraph-verticles', pv_dependencies, '/opt/paragraph/ParagraphVerticles/target/dependency').

%% value transformation

transform_val(id, ValStr, Val) :-
    atom_string(Val, ValStr).
