# NOTE: need to install gnutar via macports.org!
#
BUILDDATE=`date +%F`
mkdir -p tar_output
gnutar --transform='s,SV16/bin/,,g' -cvzf tar_output/clang_70.x64.tcltk-8.5.18-BUILD${BUILDDATE}.tar.gz  /SV16/bin/osx/clang_70/x64/tcltk-8.5.18
gnutar --transform='s,SV16/bin/,,g' -cvzf tar_output/clang_70.x64.tcltk-8.6.4-BUILD${BUILDDATE}.tar.gz  /SV16/bin/osx/clang_70/x64/tcltk-8.6.4


