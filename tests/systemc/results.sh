for f in `ls -d */`; do echo -n "$f&"; wc -l $f/*.ll | awk '{printf $1}'; echo -n "&"; cat $f/stats.log; echo ""; done
