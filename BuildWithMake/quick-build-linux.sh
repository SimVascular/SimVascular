#!/bin/bash

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

#export SV_EXTERNALS_VERSION_NUMBER=2019.02
export SV_EXTERNALS_VERSION_NUMBER=2019.06

export EXTERNALS_SV_ARCH_DIR=x64

osid=$(lsb_release -si)
osrel=$(lsb_release -sr)
osver=$(lsb_release -sc)

case "$osid" in

    'Ubuntu')
        export EXTERNALS_SV_OS_DIR=ubuntu
	export EXTERNALS_SV_OS_LONG_NAME_DIR=ubuntu
	export EXTERNALS_SV_COMPILER_SHORT_NAME=gnu
	case "$osver" in
	    'focal')
		export SV_EXTERN_LINUX_VERSION=ubuntu_20
		export EXTERNALS_SV_FULL_OS_VER_NO=20.04
		export EXTERNALS_SV_COMPILER_VER_NO=7.5
		if [ $SV_EXTERNALS_VERSION_NUMBER == '2019.02' ]; then
		  echo "Error - no 2019.02 for ubuntu 20!"
		  exit
		elif [ $SV_EXTERNALS_VERSION_NUMBER == '2019.06' ]; then
                  export EXTERNALS_BUILD_DATE=2021.06.10
                  export EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE=release
		fi
		;;
	    'bionic')
		export SV_EXTERN_LINUX_VERSION=ubuntu_18
		export EXTERNALS_SV_FULL_OS_VER_NO=18.04
		export EXTERNALS_SV_COMPILER_VER_NO=7.3
		if [ $SV_EXTERNALS_VERSION_NUMBER == '2019.02' ]; then
                  export EXTERNALS_BUILD_DATE=2019.07.11
                  export EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE=release
		elif [ $SV_EXTERNALS_VERSION_NUMBER == '2019.06' ]; then
                  export EXTERNALS_BUILD_DATE=2021.02.01
                  export EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE=release
		fi
		;;
	    'xenial')
		export SV_EXTERN_LINUX_VERSION=ubuntu_16
		export EXTERNALS_SV_FULL_OS_VER_NO=16.04
		export EXTERNALS_SV_COMPILER_VER_NO=5.4
		if [ $SV_EXTERNALS_VERSION_NUMBER == '2019.02' ]; then
                  export EXTERNALS_BUILD_DATE=2019.07.11
                  export EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE=release
		elif [ $SV_EXTERNALS_VERSION_NUMBER == '2019.06' ]; then
                  export EXTERNALS_BUILD_DATE=2019.07.14
                  export EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE=release
		fi
		;;
	    'trusty')
		export SV_EXTERN_LINUX_VERSION=ubuntu_14
                export EXTERNALS_SV_FULL_OS_VER_NO=14.04
                export EXTERNALS_SV_COMPILER_VER_NO=4.8
		echo "Error - Ubuntu 14 no longer supported!"
		exit
		;;
	    *)
		echo "Error - Unsupported version of Ubuntu!"
		exit
		;;
	esac
	;;

    'CentOS')
        export EXTERNALS_SV_OS_DIR=centos
	export EXTERNALS_SV_OS_LONG_NAME_DIR=centos
	export EXTERNALS_SV_COMPILER_SHORT_NAME=gnu
	case "$osrel" in
	   8*)
		export SV_EXTERN_LINUX_VERSION=centos_8
		export EXTERNALS_SV_COMPILER_VER_NO=8.3
		if [ $SV_EXTERNALS_VERSION_NUMBER == '2019.02' ]; then
		  echo "Error - no 2019.02 for centos 8!"
		elif [ $SV_EXTERNALS_VERSION_NUMBER == '2019.06' ]; then
		  export EXTERNALS_SV_FULL_OS_VER_NO=8.1
                  export EXTERNALS_BUILD_DATE=2020.04.10
                  export EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE=release
		fi
		;;
	    7*)
		export SV_EXTERN_LINUX_VERSION=centos_7
		export EXTERNALS_SV_COMPILER_VER_NO=6.3
		if [ $SV_EXTERNALS_VERSION_NUMBER == '2019.02' ]; then
		  export EXTERNALS_SV_FULL_OS_VER_NO=7.6
                  export EXTERNALS_BUILD_DATE=2019.07.11
                  export EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE=release
		elif [ $SV_EXTERNALS_VERSION_NUMBER == '2019.06' ]; then
		  export EXTERNALS_SV_FULL_OS_VER_NO=7.6
                  export EXTERNALS_BUILD_DATE=2019.07.17
                  export EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE=release
		fi
		;;
	    6*)
		export SV_EXTERN_LINUX_VERSION=centos_6
		export EXTERNALS_SV_FULL_OS_VER_NO=6.9
                export EXTERNALS_SV_COMPILER_VER_NO=5.3
		echo "Error - CentOS 6 not supported!"
		;;
	    *)
		echo "Error - Unsupported version of CentOS!"
		exit
		;;
	esac
	;;

    *)

	echo "Error - Unknown version of Linux!  Use Ubuntu or CentOS"
	exit
	;;

esac

export EXTERNALS_TOP=`pwd`/ext

export PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/$SV_EXTERNALS_VERSION_NUMBER/linux/$EXTERNALS_SV_OS_LONG_NAME_DIR/$EXTERNALS_SV_FULL_OS_VER_NO/$EXTERNALS_SV_COMPILER_SHORT_NAME/$EXTERNALS_SV_COMPILER_VER_NO/$EXTERNALS_SV_ARCH_DIR/$EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE/$EXTERNALS_BUILD_DATE

export TAR_FILE_PREFIX=$EXTERNALS_SV_OS_LONG_NAME_DIR.$EXTERNALS_SV_FULL_OS_VER_NO.$EXTERNALS_SV_COMPILER_SHORT_NAME.$EXTERNALS_SV_COMPILER_VER_NO.$EXTERNALS_SV_ARCH_DIR.$EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE.$EXTERNALS_BUILD_DATE

# default is windows, so override with gnu compilers and linux

echo "CLUSTER=x64_linux" > cluster_overrides.mk
echo "CXX_COMPILER_VERSION=gcc" >> cluster_overrides.mk
echo "FORTRAN_COMPILER_VERSION=gfortran" >> cluster_overrides.mk
echo "SV_COMPILER=gnu" >> cluster_overrides.mk
echo "SV_COMPILER_VERSION=$EXTERNALS_SV_COMPILER_VER_NO" >> cluster_overrides.mk

echo "SV_EXTERNALS_VERSION_NUMBER=$SV_EXTERNALS_VERSION_NUMBER" >> global_overrides.mk
echo "OPEN_SOFTWARE_BINARIES_TOPLEVEL=$EXTERNALS_TOP/bin/$EXTERNALS_SV_COMPILER_SHORT_NAME/$EXTERNALS_SV_COMPILER_VER_NO/$EXTERNALS_SV_ARCH_DIR/$EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE" > global_overrides.mk

mkdir -p $EXTERNALS_TOP
chmod -R a+rwx $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/tarfiles
mkdir -p $EXTERNALS_TOP/bin/$EXTERNALS_SV_COMPILER_SHORT_NAME/$EXTERNALS_SV_COMPILER_VER_NO/$EXTERNALS_SV_ARCH_DIR/$EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE

pushd $EXTERNALS_TOP/tarfiles

if [ $SV_EXTERNALS_VERSION_NUMBER == '2019.02' ]; then
  wget $PARENT_URL/$TAR_FILE_PREFIX.freetype.2.6.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.gdcm.2.6.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.hdf5.1.10.1.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.itk.4.13.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.mitk.2018.04.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.mmg.5.3.9.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.opencascade.7.3.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.python.3.5.5.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.tcltk.8.6.4.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.tinyxml2.6.2.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.vtk.8.1.1.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.qt.5.6.3.tar.gz
elif [ $SV_EXTERNALS_VERSION_NUMBER == '2019.06' ]; then
  wget $PARENT_URL/$TAR_FILE_PREFIX.freetype.2.6.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.gdcm.2.6.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.hdf5.1.10.1.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.itk.4.13.2.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.mitk.2018.04.2.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.mmg.5.3.9.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.opencascade.7.3.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.python.3.5.5.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.qt.5.11.3.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.tcltk.8.6.4.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.tinyxml2.6.2.0.tar.gz
  wget $PARENT_URL/$TAR_FILE_PREFIX.vtk.8.1.1.tar.gz
fi

popd

pushd $EXTERNALS_TOP/bin/$EXTERNALS_SV_COMPILER_SHORT_NAME/$EXTERNALS_SV_COMPILER_VER_NO/$EXTERNALS_SV_ARCH_DIR/$EXTERNALS_SV_LOWERCASE_CMAKE_BUILD_TYPE
for i in $EXTERNALS_TOP/tarfiles/$TAR_FILE_PREFIX.*.tar.gz; do
    echo "untar $i"
    tar xzf $i
done
popd

make fast
