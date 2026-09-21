This is the transformation of paragraph model and extracted paramv values to some RDF data and datalog rules.

# Instructions for RDFox

Create a new datastore

Using the RDFox console, load the pgraphlog files in that order:
    - paragraph_ontology.txt
    - paragraph_extensions.ttl
    - nodeproj_bdsl.ttl
    - compromised-node-deps.ttl
    - application-node-deps.ttl
    - paragraph_rules.dlog

# Instructions for terminusdb

Create a new datastore

```
terminusdb triples load admin/para_graph/local/branch/main/instance paragraph_extensions.ttl
terminusdb triples load admin/para_graph/local/branch/main/instance nodeproj_bdsl.ttl
```

# Instructions for QLever

QLever binaries have been installed:
  - qlever-index: for loading and indexing data
  - qlever-server: start a SPARQL endpoint and query data
  - qlever: CLI tool for controlling (almost) everything QLever can do

For more information: https://github.com/ad-freiburg/qlever-control

