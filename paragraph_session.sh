echo "Welcome to Paragraph. "
echo ""
echo "Start a working session with:"
echo "?- consult('prolog/paragraph_conf.pl')."
echo ""
echo "?- consult('prolog/paragraph.pl')."
echo ""
echo "Load and execute a test suite such as doc_t with:"
echo "?- consult('t/doc_t.plt')."
echo ""
echo "?- run_tests."
export PARAGRAPH_HOME=$(pwd)
export PARAGRAPH_TEMP=/tmp/paragraph
export IPADDRE=localhost
/usr/local/Cellar/swi-prolog/8.2.0/bin/swipl
