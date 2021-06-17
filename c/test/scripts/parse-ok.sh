#!/bin/bash
set -e

cmd=../../../bin/static/eexpr2json

set +e
"$cmd" \
  -ddumpRawTokens rawTokens.output \
  -ddumpTokens tokens.output \
  -ddumpEexprs eexprs.output \
  input.eexpr
echo "$?" >exitcode.output
