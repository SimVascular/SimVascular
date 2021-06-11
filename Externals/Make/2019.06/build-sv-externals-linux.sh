#EXTERNALS_TOP=/usr/local/sv/ext/2019.06/release/gl1
EXTERNALS_TOP=/usr/local/sv/ext/2019.06/release/gl2
EXTERNALS_BUILD_TOP=$EXTERNALS_TOP/build

SV_EXTERN_OS=linux

#
#  must have write permissions to dirs!
#

osid=$(lsb_release -si)
osrel=$(lsb_release -sr)
osver=$(lsb_release -sc)

case "$osid" in

    'Ubuntu')
	
	case "$osver" in
	    'focal')
		export SV_EXTERN_LINUX_VERSION=ubuntu_20
		;;
	    'disco')
		export SV_EXTERN_LINUX_VERSION=ubuntu_19
		;;
	    'bionic')
		export SV_EXTERN_LINUX_VERSION=ubuntu_18
		;;
	    'xenial')
		export SV_EXTERN_LINUX_VERSION=ubuntu_16
		;;
	    'trusty')
	        echo "Error: ubuntu_14 no longer supported!"
		exit
		;;   
	    *)
		echo "Error!"
		exit
		;;
	esac
	;;

    'CentOS')

	case "$osrel" in

	    8*)
		export SV_EXTERN_LINUX_VERSION=centos_8
		;;
	    7*)
		export SV_EXTERN_LINUX_VERSION=centos_7
		;;

	    '6.9')
		export SV_EXTERN_LINUX_VERSION=centos_6
		;;

	    *)
		echo "Error!"
		exit
		;;

	esac	       
	;;

    'AmazonAMI')
        export SV_EXTERN_LINUX_VERSION=ami_2018_03
        ;;

    'Amazon')
        export SV_EXTERN_LINUX_VERSION=amazon_2019_03
        ;;

    *)
	 
	echo "Error!"
	exit
	;;

esac
	
#
# initial setup
#

echo "Deleting previous build dir ($EXTERNALS_BUILD_TOP)"
sudo rm -Rf $EXTERNALS_BUILD_TOP
sudo mkdir -p $EXTERNALS_BUILD_TOP

echo "Deleting previous src+bin dir ($EXTERNALS_TOP)"
sudo rm -Rf $EXTERNALS_TOP

sudo mkdir -p /usr/local/sv
sudo chmod a+rwx /usr/local/sv
sudo chown -R $USER /usr/local/sv

sudo mkdir -p $EXTERNALS_TOP
sudo chown -R $USER $EXTERNALS_TOP
sudo chgrp -R $USER $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/src
 
cp -Rf BuildHelpers $EXTERNALS_TOP/src
cp -Rf ../../Patches $EXTERNALS_TOP/src/BuildHelpers

#
# let's do it
#

pushd $EXTERNALS_TOP/src/BuildHelpers

echo "Starting build in ($EXTERNALS_TOP)..."
echo "  note: see logs in stdout.superbuild.txt, tmp/stdout*, etc."
source Scripts/superbuild-linux-gcc.sh >& stdout.superbuild.txt

popd
