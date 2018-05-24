pushd ../itk-4.7.1
patch -p1 < ../BuildHelpers/Patches/ITK-4.7.1-gcc-5.patch
patch -p1 < ../BuildHelpers/Patches/ITK-4.7.1.patch
popd
