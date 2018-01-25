# Hey emacs, this is a -*- makefile -*-

# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
