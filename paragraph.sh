echo "Welcome to Paragraph. "
echo ""
echo "?- consult('/opt/paragraph/prolog/paragraph_conf.pl')."
echo ""
echo "?- consult('/opt/paragraph/prolog/paragraph.pl')."
echo ""
docker run -it -v $(pwd):/opt/paragraph incodame/swipl_images:om_search
