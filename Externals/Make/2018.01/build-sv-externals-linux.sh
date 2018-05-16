EXTERNALS_TOP=/usr/local/sv/ext
EXTERNALS_BUILD_TOP=$EXTERNALS_TOP/build

#
#  must have write permissions to dirs!
#

sudo mkdir -p /usr/local/sv
sudo chmod a+rwx /usr/local/sv

osid=$(lsb_release -si)
osrel=$(lsb_release -sr)
osver=$(lsb_release -sc)

case "$osid" in

    'Ubuntu')
	
	case "$osver" in
	    'xenial')
		export SV_EXTERN_LINUX_VERSION=ubuntu_16
		;;
	    'trusty')
		export SV_EXTERN_LINUX_VERSION=ubuntu_14
		;;   
	    *)
		echo "Error!"
		exit
		;;
	esac
	;;

    'CentOS')

	case "$osrel" in

	    '7')
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

    *)
	 
	echo "Error!"
	exit
	;;

esac
	
#
# initial setup
#

echo "Deleting previous build dir ($EXTERNALS_BUILD_TOP)"
rm -Rf $EXTERNALS_BUILD_TOP
mkdir -p $EXTERNALS_BUILD_TOP

echo "Deleting previous src+bin dir ($EXTERNALS_TOP)"
rm -Rf $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/src
cp -Rf BuildHelpers $EXTERNALS_TOP/src

#
# let's do it
#

pushd $EXTERNALS_TOP/src/BuildHelpers

echo "Starting build in ($EXTERNALS_TOP)..."
echo "  note: see logs in stdout.superbuild.txt, tmp/stdout*, etc."
source Scripts/superbuild-linux-gcc.sh >& stdout.superbuild.txt

popd


