# Hey emacs, this is a -*- makefile -*-

TOP = ..

include $(TOP)/include.mk

DATEVAR = `date +%F`

all: cxxdocs tcldocs flowsolverdocs post-build

cxxdocs:
	-rm -f sedscript
	-rm -f useme.Doxyfile
	-rm -Rf htmldocs/cxx_html
	-mkdir -p htmldocs
	-echo "puts \"s+REPLACEME_SV_DOX_OUTPUT_DIR+[file rootname `pwd`]/htmldocs+g\"" | tclsh > sedscript
	-echo "puts \"s+REPLACEME_SV_DOX_INPUT_DIR+[file rootname `pwd`]/../../Code+g\"" | tclsh >> sedscript
	-echo "puts \"s+REPLACEME_SV_PROJECT_NUMBER+${DATEVAR}+g\"" | tclsh >> sedscript
	-sed -f sedscript simvascular.Doxyfile > useme.Doxyfile
	-doxygen useme.Doxyfile
	-rm useme.Doxyfile
	-rm sedscript

tcldocs:
	-rm -f htmldocs/tcl_all_html
	-mkdir -p htmldocs/tcl_all_html
	-tclsh /SV15/src/autodoc/autodoc -srcdir `pwd`/../../Tcl/SimVascular_2.0 -outputdir `pwd`/htmldocs/tcl_all_html
	-cp styles.css `pwd`/htmldocs/tcl_all_html
	-rm -f htmldocs/tcl_common_and_core_html
	-rm -f -R TclCode
	-mkdir -p htmldocs/tcl_common_and_core_html
	-mkdir -p TclCode
	-svn export --force $(TOP)/../Tcl/Common TclCode/Common
	-svn export --force $(TOP)/../Tcl/SimVascular_2.0/Core TclCode/Core
	-tclsh /SV15/src/autodoc/autodoc -srcdir `pwd`/TclCode -outputdir `pwd`/htmldocs/tcl_common_and_core_html
	-cp styles.css `pwd`/htmldocs/tcl_common_and_core_html
	-rm -f -R TclCode

flowsolverdocs:
	-rm -f sedscript
	-rm -f useme.Doxyfile
	-rm -f htmldocs/flowsolver_html
	-mkdir -p htmldocs
	-echo "puts \"s+REPLACEME_SV_DOX_OUTPUT_DIR+[file rootname `pwd`]/htmldocs+g\"" | tclsh > sedscript
	-echo "s+REPLACEME_SV_PROJECT_NUMBER+${DATEVAR}+g" >> sedscript
	-sed -f sedscript flowsolver.Doxyfile > useme.Doxyfile
	-doxygen useme.Doxyfile
	-rm useme.Doxyfile
	-rm sedscript

post-build:
	-cp index.html htmldocs/
	-rm -f htmldocs-${DATEVAR}.tar.gz
	-tar cvzf htmldocs-${DATEVAR}.tar.gz htmldocs/

clean: veryclean

veryclean:
	if [ -e sedscript ];then /bin/rm -f sedscript;fi
	if [ -e useme.Doxyfile ];then /bin/rm -f useme.Doxyfile;fi
	if [ -e htmldocs ];then /bin/rm -Rf htmldocs;fi
	if [ -e TclCode ];then /bin/rm -Rf TclCode;fi

#
