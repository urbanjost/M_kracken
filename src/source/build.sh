#!/bin/bash
cd $(dirname $0)
export GITHUB=TRUE
export DEMO_OUTDIR=../../example
export DEMO_SUBDIR=FALSE
GPF_build_module M_kracken
cp ../../docs/man3.html ../../docs/index.html
cp ../../docs/BOOK_M_kracken.html ../../docs/index.html
ccall ../../test/test_suite_M_kracken.[fF]90
exit
