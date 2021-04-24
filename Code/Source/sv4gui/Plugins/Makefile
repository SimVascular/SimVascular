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

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

SVQTGUIEXT_DIRS = org.sv.projectdatanodes \
		  org.sv.pythondatanodes \
                  org.sv.gui.qt.projectmanager \
                  org.sv.gui.qt.datamanager \
                  org.sv.gui.qt.pathplanning \
                  org.sv.gui.qt.segmentation \
                  org.sv.gui.qt.mitksegmentation \
                  org.sv.gui.qt.modeling \
                  org.sv.gui.qt.meshing \
                  org.sv.gui.qt.simulation \
                  org.sv.gui.qt.romsimulation \
                  org.sv.gui.qt.imageprocessing \
                  org.sv.gui.qt.svfsi  \
                  org.sv.gui.qt.application

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

create_exports_cv_h:

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
