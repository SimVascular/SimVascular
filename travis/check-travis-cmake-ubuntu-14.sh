
# set by travis
export TRAVIS_OS_NAME="linux"

#from the "matrix" in .travis.xml

export WITH_CMAKE=true
export SV_EXTERNALS_VERSION_NUMBER=2018.05
export USE_SYSTEM_QT=false
export SV_EXTERNALS_USE_PREBUILT_QT=0

#from "before_install" in .travis.xml

export NUM_THREADS=4
export cwd=$(pwd)
export BUILD_DIR=$cwd/build
export SV_EXTERNALS_BIN_DIR=$cwd/sv_externals/bin
export SV_CODE_DIR=$cwd/Code
export SV_EXTERNALS_DIR=$cwd/Externals
export SV_EXTERNALS_BUILD_DIR=$SV_EXTERNALS_DIR/build
export SV_EXTERNALS_BIN_DIR=$SV_EXTERNALS_BUILD_DIR/sv_externals/bin
export SCRIPTS=$cwd/travis
# if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then $SCRIPTS/travis_update_ubuntu.sh; fi
# if $USE_SYSTEM_QT; then source $SCRIPTS/travis_get_system_qt.sh; fi
# if $WITH_CMAKE; then $SCRIPTS/travis_get_cmake_latest.sh; fi

# from script
source $SCRIPTS/travis_cmake_build.sh
