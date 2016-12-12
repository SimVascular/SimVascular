# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical
# Software Corporation, University of California, San Diego.
#
# Portions Copyright (c) 2000-2007, Stanford University, 
#    Rensselaer Polytechnic Institute, Kenneth E. Jansen, 
#    Charles A. Taylor (see SimVascular Acknowledgements file 
#    for additional contributors to the source code).
#
# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions 
# are met:
#
# Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer. 
# Redistributions in binary form must reproduce the above copyright 
# notice, this list of conditions and the following disclaimer in the 
# documentation and/or other materials provided with the distribution. 
# Neither the name of the Stanford University or Rensselaer Polytechnic
# Institute nor the names of its contributors may be used to endorse or
# promote products derived from this software without specific prior 
# written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
# OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
# AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
# THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

TOP=../../../BuildWithMake

include $(TOP)/include.mk

SVQTGUIEXT_DIRS = org.sv.projectdatanodes/src/internal \
                  org.sv.gui.qt.projectmanager/src/internal \
                  org.sv.gui.qt.modeling/src/internal \
                  org.sv.gui.qt.pathplanning/src/internal \
                  org.sv.gui.qt.segmentation/src/internal \
                  org.sv.gui.qt.meshing/src/internal \
                  org.sv.gui.qt.simulation/src/internal \
                  org.sv.gui.qt.application/src/internal

lib-complete: moc ui qrc lib

static-complete:  create_plugin_export_h create_manifest_qrc create_cached_qrc moc ui qrc static

shared-complete:  create_plugin_export_h create_manifest_qrc create_cached_qrc moc ui qrc shared

lib:
	@for i in ${SVQTGUIEXT_DIRS}; do ( \
	  cd $$i; \
	  $(MAKE)) ; done

static:
	@for i in ${SVQTGUIEXT_DIRS}; do ( \
	  cd $$i; \
	  $(MAKE)) ; done

shared:
	@for i in ${SVQTGUIEXT_DIRS}; do ( \
	  cd $$i; \
	  $(MAKE) shared) ; done

moc:
	@for i in ${SVQTGUIEXT_DIRS}; do ( \
	  cd $$i; \
	  $(MAKE) moc) ; done

ui:
	@for i in ${SVQTGUIEXT_DIRS}; do ( \
	  cd $$i; \
	  $(MAKE) ui) ; done

qrc:
	@for i in ${SVQTGUIEXT_DIRS}; do ( \
	  cd $$i; \
	  $(MAKE) qrc) ; done

create_plugin_export_h:
	@for i in ${SVQTGUIEXT_DIRS}; do ( \
	  cd $$i; \
	  $(MAKE) create_plugin_export_h) ; done

create_manifest_qrc:
	@for i in ${SVQTGUIEXT_DIRS}; do ( \
	  cd $$i; \
	  $(MAKE) create_manifest_qrc) ; done

create_cached_qrc:
	@for i in ${SVQTGUIEXT_DIRS}; do ( \
	  cd $$i; \
	  $(MAKE) create_cached_qrc) ; done

clean:
	for i in ${SVQTGUIEXT_DIRS}; do ( \
	  cd $$i; \
	  $(MAKE) clean ) ; done

veryclean: clean
	for i in ${SVQTGUIEXT_DIRS}; do ( \
	  cd $$i; \
	  $(MAKE) veryclean ) ; done
