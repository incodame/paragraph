:- module(paragraph_conf, [ paramloc/5 ]).

paramloc(app,     application_xml, WebAppEar,      "rpath:META-INF/application.xml", []) :- webappear(app, _WebApp, WebAppEar).
paramloc(app,     persistence_xml, WebAppEar,      "endswith:persistence-context.xml", []) :- webappear(app, _WebApp, WebAppEar).
paramloc(app,     context_root, application_xml,   "xpath://\'context-root\'(text)", []).
paramloc(app,     batch_cmd, BatchAppZip,          "endswith:.cmd", []) :- batchzip(app, _, BatchAppZip).
paramloc(app,     batch_properties, BatchAppZip,   "endswith:.properties", []) :- batchzip(app, _, BatchAppZip).
paramloc(app,     batch_java_home, batch_cmd,      "regexp:^(?<V1L>.*JAVA_HOME=.*)$", []).

