#!/bin/bash
set -e

HERE="$(dirname "$(realpath "$0")")"
cd "$HERE"

FORMAT=markdown # pandoc markdown has plenty of things I want by default
FORMAT+=+abbreviations
FORMAT+=+lists_without_preceding_blankline

# katexUrl='https://cdn.jsdelivr.net/npm/katex@0.13.11/dist/'
# htmlTex=--katex="$katexUrl"
mathjaxUrl='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'
htmlTex=--mathjax="$mathjaxUrl"

# MONOFONT="Unifont" # looks like trach even on ascii chars
# MONOFONT="Everson Mono" # blegh, this lisence is some garbo from the '90s
# MONOFONT='FreeMono' # no dot in zero, 1 and l aren't very distinct
# above here is mostly ruled-out
MONOFONT='Source Code Pro' # I like the spacing here, but there are lots of missing chars relative to DejaVu
MONOFONT='DejaVu Sans Mono' # the spacing is a bit tight, but it has ~100 more chars than Source Code Pro

mkTarget() {
  local basename="$1"
  pandoc "$basename.pandoc" --from "$FORMAT" --to html \
    -s --template "template.html" \
    "$htmlTex" -o "$basename.html"
  pandoc "$basename.pandoc" --from "$FORMAT" \
    --to pdf --pdf-engine=xelatex \
      -V monofont="$MONOFONT" \
      --template "template.tex" -L texfilters.lua \
    -o "$basename.pdf"
}

mkTarget report
