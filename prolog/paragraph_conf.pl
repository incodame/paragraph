:- module(paragraph_conf, [ application_group/3, application/4, paramloc/5, app_archive/4, search_option/3 ]).
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

%% parameters (ordered alphabetically)

paramloc(app,     application_xml, WebAppEar,      "rpath:META-INF/application.xml", []) :- app_archive(ear, _WebApp, WebAppEar, _).
paramloc(app,     context_root, application_xml,   "xpath://\'context-root\'(text)", []).
paramloc(app,     batch_cmd, BatchAppZip,          "endswith:.cmd", []) :- batchzip(app, _, BatchAppZip).
paramloc(app,     batch_properties, BatchAppZip,   "endswith:.properties", []) :- batchzip(app, _, BatchAppZip).
paramloc(app,     batch_java_home, batch_cmd,      "regexp:^(?<V1L>.*JAVA_HOME=.*)$", []).
paramloc(app,     persistence_xml, WebAppEar,      "endswith:persistence-context.xml", []) :- app_archive(ear, _WebApp, WebAppEar, _).
paramloc(app,     war_pom_xml,     WebAppWar,      "endswith:/pom.xml",     []) :- app_archive(war, _WebApp, WebAppWar, _).
paramloc(app,     pom_xml_version, war_pom_xml,    "xpath://version(text)", []).

%% application archives ordered alphabetically (ear, war, jar, zip)

app_archive(war,  'paragraph-ui',        'paragraph-ui-(version).war', []).
app_archive(jar,  'paragraph-verticles', 'paragraph-verticles-(version)-fat.jar', []).

%% search options: term, printable text and default way to get it

search_option(ag(ApplicationGroup), "Application group", read(ApplicationGroup)).
search_option(ve(Version), "Version", read(Version)).
