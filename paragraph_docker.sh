echo "Welcome to Paragraph. "
echo ""
echo "Start a working session with:"
echo "?- consult('/opt/paragraph/prolog/paragraph_conf.pl')."
echo ""
echo "?- consult('/opt/paragraph/prolog/paragraph.pl')."
echo ""
echo "Load and execute a test suite such as doc_t with:"
echo "?- consult('/opt/paragraph/t/doc_t.plt')."
echo ""
echo "?- run_tests."
#docker run -it --rm -v $(pwd):/opt/paragraph -v /tmp/paragraph:/tmp/paragraph --env PARAGRAPH_HOME=/opt/paragraph --env PARAGRAPH_TEMP=/tmp/paragraph --env IPADDRE=$(ipconfig getifaddr en0) incodame/swipl_images:om_search
docker run -it --rm -v $(pwd):/opt/paragraph -v /tmp/paragraph:/tmp/paragraph --env PARAGRAPH_HOME=/opt/paragraph --env PARAGRAPH_TEMP=/tmp/paragraph --env IPADDRE=$(ipconfig getifaddr en0) incodame/swipl_images:par_yaml
