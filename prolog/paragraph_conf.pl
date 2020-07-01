:- module(paragraph_conf, [ application_group/3, application/4, directory_alias/2, directory_alias/3, paramloc/4, app_archive/4, app_file/4, search_option/3 ]).
:- use_module(library(xpath)).
%%
%% Paragraph Configuration
%%
%% please manage this file under version control (git or whatever fits your project)
%%

%% application and groups (ordered by application groups)

application_group(build, paragraph, 'org.incodame.paragraph').

application(app, 'paragraph-ui',        paragraph, [ build(maven) ]).
application(app, 'paragraph-verticles', paragraph, [ build(maven) ]).
application(app, 'paragraph_conf',      paragraph, []).
application(app, 'paragraph',           paragraph, []).

%% parameters (ordered alphabetically), document for your own benefit !

paramloc(application_xml,  WebAppEar,       rpath("META-INF/application.xml"), [ doc(Doc) ]) :-
    app_archive(ear, _App, WebAppEar, _),
    Doc = "The application.xml deployment descriptor contains one module element for each module in the Enterprise Archive file.".
paramloc(batch_cmd,        BatchAppZip,     endswith(".cmd"), [ doc(Doc) ]) :-
    app_archive(zip, _App, BatchAppZip, _),
    Doc = "Any batch cmd file within an application's zip".
paramloc(batch_java_home,  batch_cmd,       regexp("^(?<V1L>.*JAVA_HOME=.*)$"), [ doc("JAVA_HOME definition within a .cmd") ]).
paramloc(batch_properties, BatchAppZip,     endswith(".properties"), [ doc(Doc) ]) :-
    app_archive(zip, _App, BatchAppZip, _),
    Doc = "Any properties file within an application's zip".
paramloc(context_root,     application_xml, xpath(//'context-root'(text)),      [ doc("context root of a Java EE web application")]).
paramloc(persistence_xml,  WebAppEar,       endswith("persistence-context.xml"), [ doc(Doc) ]) :-
    app_archive(ear, _App, WebAppEar, _),
    Doc = "Main configuration of JPA".
paramloc(pom_xml_parent,   war_pom_xml,     xpath(//project/parent), [ doc("pom.xml parent in app archive") ]).
paramloc(pom_xml_parent,   app_pom_xml,     xpath(//project/parent), [ doc("pom.xml parent in app file") ]).
paramloc(pom_xml_parent_artifact_id,  pom_xml_parent,     xpath(//artifactId(text)), [ doc("pom.xml parent artifactId") ]).
paramloc(pom_xml_parent_group_id,     pom_xml_parent,     xpath(//groupdId(text)),   [ doc("pom.xml parent groupdId") ]).
paramloc(pom_xml_parent_version,      pom_xml_parent,     xpath(//version(text)),    [ doc("pom.xml parent version") ]).
paramloc(pom_xml_version,  war_pom_xml,     xpath(//project/version(text)), [ doc("pom.xml version") ]).
paramloc(war_pom_xml,      WebAppWar,       endswith("/pom.xml"),     [ doc(Doc) ]) :-
    app_archive(war, _App, WebAppWar, _),
    Doc = "pom.xml of a Web Archive built by Maven".
paramloc(app_pom_xml,      no_parent,       lfile('pom.xml'), [ doc(Doc) ]) :-
    app_file(pom, _App, 'pom.xml', [ doc(Doc) ]).

%% application archives ordered alphabetically (ear, war, jar, zip)

app_archive(war,  'paragraph-ui',           'paragraph-ui(-version).war', []).
app_archive(jar,  'paragraph-verticles',    'paragraph-verticles-(version)-fat.jar', []).

%% application files

app_file(pom, AppId, 'pom.xml',  [ doc("application pom.xml") ]) :-
    application(app, AppId, _ApplicationGroup, AppOpts), memberchk(build(maven), AppOpts).
% add specific files here

%% search options: term, printable text and default way to get it

search_option(ag(ApplicationGroup), "Application group", read(ApplicationGroup)).
search_option(ve(Version),          "Version", read(Version)).
search_option(wd(WorkDirectory),    "Work directory alias", read(WorkDirectory)).
search_option(ad(AppDirectory),     "App directory alias",  read(AppDirectory)).

%% global directory aliases

directory_alias(examples, '/opt/paragraph/examples').

%% application specific directory aliases

directory_alias('paragraph-ui', paragraph_ui, '/opt/paragraph/ParagraphUI').
directory_alias('paragraph-verticles', paragraph_verticles, '/opt/paragraph/ParagraphVerticles').

