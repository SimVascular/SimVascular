export PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/src/originals/

export SV_PATCH_BRANCH_MITK_NAME=simvascular-patch-2016.03.0

rm -Rf tmp/mitk
mkdir -p tmp/mitk
pushd tmp/mitk

BUILDDATE=`date +%F`
echo "WGET_MITK"
wget $PARENT_URL/mitk/mitk-v2016.03.0.tar.gz
rm -Rf mitk-2016.03  
tar xzf ./mitk-v2016.03.0.tar.gz
  
echo "checkout patched version from git"

git clone https://github.com/SimVascular/MITK.git $SV_PATCH_BRANCH_MITK_NAME
cd $SV_PATCH_BRANCH_MITK_NAME
git fetch -a
git checkout -b $SV_PATCH_BRANCH_MITK_NAME origin/$SV_PATCH_BRANCH_MITK_NAME
cd ..

diff -aur --new-file -x ".git" mitk-2016.03/ $SV_PATCH_BRANCH_MITK_NAME > mitk-2016.03-$BUILDDATE.patch

cp mitk-2016.03-$BUILDDATE.patch ../../Patches

popd
  
