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

mkTarget() {
  local basename="$1"
  pandoc "$basename.pandoc" --from "$FORMAT" --to html -s "$htmlTex" -o "$basename.html"
  pandoc "$basename.pandoc" --from "$FORMAT" \
    --to pdf --pdf-engine=xelatex \
      -V 'monofont:Source Code Pro' \
    -o "$basename.pdf"
}

mkTarget report
