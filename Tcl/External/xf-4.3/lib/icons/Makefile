#
# Configuration control file
#
# $Header: Makefile[2.2] Wed Mar 10 12:00:18 1993 garfield@garfield frozen $
#

# --------------------------------------------------------------------
#		locations and general macros
# --------------------------------------------------------------------

# The base directory of the project's source repository area.

BASE = /home/stone/garfield/development/xf

# Path to this node system relative to the root of the source
# repository defined above (e.g. vc/save).

NODEPATH = lib/edge

# A short name for this node system

NODENAME = lib/edge

# The operating system, $(TARGET) shall be built for.

HOSTSYSTEM = s-sunos_4

# The processor type.

HOSTTYPE = sun4

# Preprocessor switches. (eg. -DDEBUG)

SWITCHES = 

# Locations and modes for the installation of executables, header
# files, libraries and manuals.

INSTALLBASE = /usr/local
INSTALLBINPATH = $(INSTALLBASE)/bin
INSTALLBINMODE = 755
INSTALLINCPATH = $(INSTALLBASE)/include
INSTALLINCMODE = 444
INSTALLLIBPATH = $(INSTALLBASE)/lib/shape
INSTALLLIBMODE = 644
INSTALLMANPATH = $(INSTALLBASE)/man
INSTALLMANMODE = 444

# Directories, where local libraries and header files are to be
# installed for project wide use.

LIBPATH     = $(BASE)/lib
INCLUDEPATH = $(BASE)/include

# --------------------------------------------------------------------
#		the system's components
# --------------------------------------------------------------------

#
# The system (program, library, etc.) to be built. If you want to
# manage multiple programs, you should introduce multiple targets
# (like PROGTARGET LIBTARGET or any better names). In this case you 
# have to adjust the system building actions accordingly.

TARGET = noTarget

# The release number generator. The version number of this file will
# be used as release identifier for the whole system.

VERSIONFILE = Version	# source
VERSIONOBJECT =		# derived (if source contains program code)

# The names of the subdirectories containing subsystems which are also
# to be built.

SUBSYSTEMS = 

# Aliases for (filesystem links to) $(TARGET).

ALIASES = 

# The regular source and header files.

SOURCES = Button Canvas CheckButton Checkbutton Empty Entry Frame \
	  Label Listbox Menu Menubutton Message RadioButton \
	  Radiobutton Scale Scrollbar Text Tk Toplevel Unknown \
	  Barchart Bargraph Dial Hypertext Photo Pie Stripchart \
	  TkEmacs XFForm XYGraph

HEADERS = 

# Auxiliary source and header files that are not genuine part of the
# system (eg. a test environment). These will also be processed on
# system building, but will *not* be included in releases.

AUXSOURCES = 

AUXHEADERS = 

# Sources of variant source components stored under equal names in
# different locations. During system builds, only one of each (equally
# named) group is chosen. Source distributions need all of them.

VARIANTSOURCES =

VARIANTHEADERS =

# The manuals

MANUALS = $(MAN1) $(MAN3) $(MAN4) $(MAN5) $(MAN6) $(MAN7) $(MAN8)
MAN1 = 
MAN3 = 
MAN4 = 
MAN5 = 
MAN6 = 
MAN7 = 
MAN8 = 

# All source components of the system (should not be changed)

COMPONENTS = $(SOURCES) $(HEADERS) $(MANUALS) Shapefile Makefile Dependencies

# The derived files. All files, that are autamatically produced during
# a build process should be listed here.

OBJECTS = $(VERSIONOBJECT)

# --------------------------------------------------------------------
#		tools, flags, libraries etc.
# --------------------------------------------------------------------

MAKE = make
SHELL = /bin/sh
CC = cc
CFLAGS  = -I$(INCLUDEPATH) $(SWITCHES) -O
LDFLAGS = -s
RANLIB  = /usr/bin/ranlib

# System libraries, local libraries and lint libraries.

SYSLIBS   = 
LOCALLIBS = 
LINTLIBS  =

# --------------------------------------------------------------------
#			the targets
# --------------------------------------------------------------------

# The default action (do not change)

all: +all $(ALLTARGETS)

# The final system building action.

targets: $(TARGET)

$(TARGET): $(LOCALLIBS) $(OBJECTS)
	@_aliases="$(ALIASES)"; \
	for i in $$_aliases; \
	do \
	  rm -f $$i; \
	  echo linking $(TARGET) to $$i; \
	  ln $(TARGET) $$i; \
	done

# Construction of a library would look like:
#$(LIBTARGET): $(OBJECTS)
#	@ar ruv $(LIBTARGET) $(OBJECTS); \
#	($(RANLIB) $(LIBTARGET)) 2> /dev/null

#local installation of a project wide library
#localinstalltargets: $(INSTALLLIBPATH)/$(LIBTARGET) \
#		$(INSTALLINCPATH)/lib-header.h

installtargets:

$(INSTALLBINPATH)/$(TARGET): $(TARGET)
	@-echo "installing $(TARGET) in $(INSTALLBINPATH)"; \
	if [ -f $(INSTALLBINPATH)/$(TARGET) ] && \
	   [ ! -w $(INSTALLBINPATH)/$(TARGET) ]; \
	then \
	  chmod u+w $(INSTALLBINPATH)/$(TARGET); \
	fi; \
	cp $(TARGET) $(INSTALLBINPATH)/$(TARGET); \
	chmod $(INSTALLBINMODE) $(INSTALLBINPATH)/$(TARGET); \
	_aliases="$(ALIASES)"; \
	for i in $$_aliases; \
	do \
	  rm -f $(INSTALLBINPATH)/$$i; \
	  echo "linking $(INSTALLBINPATH)/$(TARGET) to $(INSTALLBINPATH)/$$i"; \
	  ln $(INSTALLBINPATH)/$(TARGET) $(INSTALLBINPATH)/$$i; \
	done

# Installing a library
#installtargets: $(INSTALLLIBPATH)/$(TARGET) installincludes
#
#$(INSTALLLIBPATH)/$(TARGET): $(TARGET)
#	@-echo "installing $(TARGET) in $(INSTALLLIBPATH)"; \
#	if [ -f $(INSTALLLIBPATH)/$(TARGET) ] ;\
#	then \
#	mv -f $(INSTALLLIBPATH)/$(TARGET) \
#	      $(INSTALLLIBPATH)/$(TARGET).old;\
#	fi; \
#	cp $(TARGET) $(INSTALLLIBPATH)/$(TARGET); \
#	chmod $(INSTALLLIBMODE) $(INSTALLLIBPATH)/$(TARGET); \
#	($(RANLIB) $(INSTALLLIBPATH)/$(TARGET)) 2> /dev/null

# installing a header file belonging to the library above
# $(INSTALLINCPATH)/lib-header.h: lib-header.h
#	@-echo "Installing lib-header.h in $(INSTALLINCPATH)"; \
#	if [ -f $(INSTALLINCPATH)/lib-header.h ] && \
#	   [ ! -w $(INSTALLINCPATH)/lib-header.h ]; \
#	then \
#	  chmod u+w $(INSTALLINCPATH)/lib-header.h; \
#	fi; \
#	cp lib-header.h $(INSTALLINCPATH)/lib-header.h; \
#	chmod $(INSTALLINCMODE) $(INSTALLINCPATH)/lib-header.h

installmanuals: $(MANUALS)
	@-_manuals="$(MAN1)"; \
	for i in $$_manuals; \
	do \
	  echo "installing $$i in $(INSTALLMANPATH)/man1"; \
	  if [ -f $(INSTALLMANPATH)/man1/$$i ] && \
	     [ ! -w $(INSTALLMANPATH)/man1/$$i ]; \
	  then \
	    chmod u+w $(INSTALLMANPATH)/man1/$$i; \
	  fi; \
	  cp $$i $(INSTALLMANPATH)/man1/$$i; \
	  chmod $(INSTALLMANMODE) $(INSTALLMANPATH)/man1/$$i; \
	done

# The cleanup action. Removes all automatically rederivable files.

doclean:
	rm -f $(TARGET) $(ALIASES) $(OBJECTS) *~

# Recursive builds. Performed *before* building $(TARGET)

subsystems:
	@_subsystems="$(SUBSYSTEMS)"; \
	for i in $$_subsystems; \
	do \
	  echo cd $$i; \
	  (cd $$i; $(MAKE) \
		BASE=$(BASE) \
		HOSTSYSTEM=$(HOSTSYSTEM) \
		HOSTTYPE=$(HOSTTYPE) \
		SWITCHES="$(SWITCHES)" \
		INSTALLBASE=$(INSTALLBASE) \
		INSTALLBINPATH=$(INSTALLBINPATH) \
		INSTALLBINMODE=$(INSTALLBINMODE) \
		INSTALLINCPATH=$(INSTALLINCPATH) \
		INSTALLINCMODE=$(INSTALLINCMODE) \
		INSTALLLIBPATH=$(INSTALLLIBPATH) \
		INSTALLLIBMODE=$(INSTALLLIBMODE) \
		INSTALLMANPATH=$(INSTALLMANPATH) \
		INSTALLMANMODE=$(INSTALLMANMODE) \
		LIBPATH=$(LIBPATH) \
		INCLUDEPATH=$(INCLUDEPATH) \
		MAKE="$(MAKE)" \
		SHELL="$(SHELL)" \
		CC="$(CC)" \
		CFLAGS="$(CFLAGS)" \
		LDFLAGS="$(LDFLAGS)" \
		RANLIB="$(RANLIB)" \
		SYSLIBS="$(SYSLIBS)" \
		BINDDEFAULT=$(BINDDEFAULT) \
		BINDINSTALL=$(BINDINSTALL) \
		COMPILER=$(COMPILER) \
		QUALITY=$(QUALITY) \
		TOTALRELEASEBASE=$(TOTALRELEASEBASE) \
		PARTIALRELEASEBASE=$(PARTIALRELEASEBASE) \
		SHAPELIBPATH=$(SHAPELIBPATH) \
		ALLTARGETS= \
		MAINTARGET= \
		$(MAINTARGET) ); \
	done

# --------------------------------------------------------------------
#			internals (do not modify)
# --------------------------------------------------------------------

install: +install $(ALLTARGETS)

clean: +clean $(ALLTARGETS)

+all:
	@-if [ -n "$(ALLTARGETS)" ]; \
	then : ; \
	else \
	  $(MAKE) ALLTARGETS="subsystems targets" MAINTARGET=all \
		BASE=$(BASE) \
		HOSTSYSTEM=$(HOSTSYSTEM) \
		HOSTTYPE=$(HOSTTYPE) \
		SWITCHES="$(SWITCHES)" \
		INSTALLBASE=$(INSTALLBASE) \
		INSTALLBINPATH=$(INSTALLBINPATH) \
		INSTALLBINMODE=$(INSTALLBINMODE) \
		INSTALLINCPATH=$(INSTALLINCPATH) \
		INSTALLINCMODE=$(INSTALLINCMODE) \
		INSTALLLIBPATH=$(INSTALLLIBPATH) \
		INSTALLLIBMODE=$(INSTALLLIBMODE) \
		INSTALLMANPATH=$(INSTALLMANPATH) \
		INSTALLMANMODE=$(INSTALLMANMODE) \
		LIBPATH=$(LIBPATH) \
		INCLUDEPATH=$(INCLUDEPATH) \
		MAKE="$(MAKE)" \
		SHELL="$(SHELL)" \
		CC="$(CC)" \
		CFLAGS="$(CFLAGS)" \
		LDFLAGS="$(LDFLAGS)" \
		RANLIB="$(RANLIB)" \
		SYSLIBS="$(SYSLIBS)" all; \
	fi

+install:
	@-if [ -n "$(ALLTARGETS)" ]; \
	then : ; \
	else \
	  $(MAKE) ALLTARGETS="subsystems installtargets" \
		MAINTARGET=install \
		BASE=$(BASE) \
		HOSTSYSTEM=$(HOSTSYSTEM) \
		HOSTTYPE=$(HOSTTYPE) \
		SWITCHES="$(SWITCHES)" \
		INSTALLBASE=$(INSTALLBASE) \
		INSTALLBINPATH=$(INSTALLBINPATH) \
		INSTALLBINMODE=$(INSTALLBINMODE) \
		INSTALLINCPATH=$(INSTALLINCPATH) \
		INSTALLINCMODE=$(INSTALLINCMODE) \
		INSTALLLIBPATH=$(INSTALLLIBPATH) \
		INSTALLLIBMODE=$(INSTALLLIBMODE) \
		INSTALLMANPATH=$(INSTALLMANPATH) \
		INSTALLMANMODE=$(INSTALLMANMODE) \
		LIBPATH=$(LIBPATH) \
		INCLUDEPATH=$(INCLUDEPATH) \
		MAKE="$(MAKE)" \
		SHELL="$(SHELL)" \
		CC="$(CC)" \
		CFLAGS="$(CFLAGS)" \
		LDFLAGS="$(LDFLAGS)" \
		RANLIB="$(RANLIB)" \
		SYSLIBS="$(SYSLIBS)" install; \
	fi

+clean:
	@-if [ -n "$(ALLTARGETS)" ]; \
	then : ; \
	else \
	  $(MAKE) ALLTARGETS="subsystems doclean" MAINTARGET=clean \
		BASE=$(BASE) \
		HOSTSYSTEM=$(HOSTSYSTEM) \
		HOSTTYPE=$(HOSTTYPE) \
		SWITCHES="$(SWITCHES)" \
		INSTALLBASE=$(INSTALLBASE) \
		INSTALLBINPATH=$(INSTALLBINPATH) \
		INSTALLBINMODE=$(INSTALLBINMODE) \
		INSTALLINCPATH=$(INSTALLINCPATH) \
		INSTALLINCMODE=$(INSTALLINCMODE) \
		INSTALLLIBPATH=$(INSTALLLIBPATH) \
		INSTALLLIBMODE=$(INSTALLLIBMODE) \
		INSTALLMANPATH=$(INSTALLMANPATH) \
		INSTALLMANMODE=$(INSTALLMANMODE) \
		LIBPATH=$(LIBPATH) \
		INCLUDEPATH=$(INCLUDEPATH) \
		MAKE="$(MAKE)" \
		SHELL="$(SHELL)" \
		CC="$(CC)" \
		CFLAGS="$(CFLAGS)" \
		LDFLAGS="$(LDFLAGS)" \
		RANLIB="$(RANLIB)" \
		SYSLIBS="$(SYSLIBS)" clean; \
	fi

