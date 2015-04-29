# Hey emacs, this is a -*- makefile -*-

TOP = ../Code

include $(TOP)/include.mk

all: cxxdocs tcldocs flowsolverdocs

cxxdocs:
	-rm -f sedscript
	-rm -f useme.Doxyfile
	-rm -Rf cxx_html
	-echo "puts \"s+REPLACEME_SIMVASCULAR_DOX_OUTPUT_DIR+[file rootname `pwd`]+g\"" | tclsh > sedscript
	-echo "puts \"s+REPLACEME_SIMVASCULAR_DOX_INPUT_DIR+[file rootname `pwd`]/../Code+g\"" | tclsh >> sedscript
	-echo "puts \"s+REPLACEME_SIMVASCULAR_DOX_INCLUDE_DIR+[file rootname `pwd`]/../Code/Swig/Utils+g\"" | tclsh >> sedscript
	-sed -f sedscript simvascular.Doxyfile > useme.Doxyfile
	-doxygen useme.Doxyfile
	-rm useme.Doxyfile
	-rm sedscript

tcldocs:
	-rm -f tcl_html
	-tclsh /sv_extern/src/autodoc/autodoc -srcdir `pwd`/../Tcl/SimVascular_2.0 -outputdir `pwd`/tcl_html
	-cp styles.css `pwd`/tcl_html

flowsolverdocs:
	-rm -f flowsolverdocs
	-doxygen flowsolver.Doxyfile

clean:
	if [ -e sedscript ];then /bin/rm -f sedscript;fi
	if [ -e useme.Doxyfile ];then /bin/rm -f useme.Doxyfile;fi
	if [ -e cxx_html ];then /bin/rm -Rf cxx_html;fi
	if [ -e tcl_html ];then /bin/rm -Rf tcl_html;fi
	if [ -e flowsolverdocs ];then /bin/rm -Rf flowsolverdocs;fi


veryclean: clean

#
