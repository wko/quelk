#!/bin/bash

shopt -s nullglob

DIR="$1"
OUT="$2"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
XSLT="$SCRIPT_DIR/display.xslt"
STATS="$SCRIPT_DIR/stats.xslt"

mkdir -p $OUT

ln -s $SCRIPT_DIR/style.css $OUT/style.css
ln -s $SCRIPT_DIR/javascript.js $OUT/javascript.js

echo "<html>

  <head>
    <link rel='stylesheet' href='style.css' />
  </head>

  <body><h1>Index </h1> <table><tr><th> Criterion </th> <th> Precision </th><th> Recall </th><th> F1 Score </th> </tr>" > "$OUT/index.html"

for f in $DIR/*.xml; do
    echo $f
    b=$(basename $f)
    outfile="${b%.*}"
    xsltproc "$XSLT" "$f" > "$OUT/$outfile.html"
    xsltproc --stringparam outfile $outfile "$STATS" "$f" >> "$OUT/index.html" 
done

echo "</table></body>
</html>" >> "$OUT/index.html"
