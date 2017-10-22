BUILDDATE=`date +%F`

SV_TAR_FILE_PREFIX=windows.10.0.msvc.18.0.x64.relwithdebinfo.${BUILDDATE}
SV_ZIP_FILE_PREFIX=windows.10.0.msvc.18.0.x64.relwithdebinfo.${BUILDDATE}

rm -Rf zip_output_tmp
mkdir -p zip_output_tmp

#  tcl/tk 8.6
if [[ $SV_SUPER_OPTIONS == *ZIP_TCL* ]]; then
  echo "ZIP_TCL"
  REPLACEME_TAR -C zip_output_tmp/ -xvzf tar_output/$SV_TAR_FILE_PREFIX.REPLACEME_SV_TCLTK_DIR.tar.gz
  pushd zip_output_tmp
  REPLACEME_ZIP -r ../zip_output/$SV_ZIP_FILE_PREFIX.REPLACEME_SV_TCLTK_DIR.zip REPLACEME_SV_TCLTK_DIR
  popd
fi

rm -Rf zip_output_tmp
mkdir -p zip_output_tmp

# python 2.7
if [[ $SV_SUPER_OPTIONS == *ZIP_PYTHON* ]]; then
  echo "ZIP_PYTHON"
  REPLACEME_TAR -C zip_output_tmp/ -xvzf tar_output/$SV_TAR_FILE_PREFIX.REPLACEME_SV_PYTHON_DIR.tar.gz
  pushd zip_output_tmp
  REPLACEME_ZIP -r ../zip_output/$SV_ZIP_FILE_PREFIX.REPLACEME_SV_PYTHON_DIR.zip REPLACEME_SV_PYTHON_DIR
  popd
fi

rm -Rf zip_output_tmp
mkdir -p zip_output_tmp

# numpy
# NOTE: numpy is contained in the python zip

# freetype
if [[ $SV_SUPER_OPTIONS == *ZIP_FREETYPE* ]]; then
  echo "ZIP_FREETYPE"
  REPLACEME_TAR -C zip_output_tmp/ -xvzf tar_output/$SV_TAR_FILE_PREFIX.REPLACEME_SV_FREETYPE_DIR.tar.gz
  pushd zip_output_tmp
  REPLACEME_ZIP -r ../zip_output/$SV_ZIP_FILE_PREFIX.REPLACEME_SV_FREETYPE_DIR.zip REPLACEME_SV_FREETYPE_DIR
  popd
fi

rm -Rf zip_output_tmp
mkdir -p zip_output_tmp

# gdcm
if [[ $SV_SUPER_OPTIONS == *ZIP_GDCM* ]]; then
  echo "ZIP_GDCM"
  REPLACEME_TAR -C zip_output_tmp/ -xvzf tar_output/$SV_TAR_FILE_PREFIX.REPLACEME_SV_GDCM_DIR.tar.gz
  pushd zip_output_tmp
  REPLACEME_ZIP -r ../zip_output/$SV_ZIP_FILE_PREFIX.REPLACEME_SV_GDCM_DIR.zip REPLACEME_SV_GDCM_DIR
  popd
fi

rm -Rf zip_output_tmp
mkdir -p zip_output_tmp

# vtk
if [[ $SV_SUPER_OPTIONS == *ZIP_VTK* ]]; then
  echo "ZIP_VTK"
  REPLACEME_TAR -C zip_output_tmp/ -xvzf tar_output/$SV_TAR_FILE_PREFIX.REPLACEME_SV_VTK_DIR.tar.gz
  pushd zip_output_tmp
  REPLACEME_ZIP -r ../zip_output/$SV_ZIP_FILE_PREFIX.REPLACEME_SV_VTK_DIR.zip REPLACEME_SV_VTK_DIR
  popd
fi

rm -Rf zip_output_tmp
mkdir -p zip_output_tmp

# itk
if [[ $SV_SUPER_OPTIONS == *ZIP_ITK* ]]; then
  echo "ZIP_ITK"
  REPLACEME_TAR -C zip_output_tmp/ -xvzf tar_output/$SV_TAR_FILE_PREFIX.REPLACEME_SV_ITK_DIR.tar.gz
  pushd zip_output_tmp
  REPLACEME_ZIP -r ../zip_output/$SV_ZIP_FILE_PREFIX.REPLACEME_SV_ITK_DIR.zip REPLACEME_SV_ITK_DIR
  popd
fi

rm -Rf zip_output_tmp
mkdir -p zip_output_tmp

# opencascade
if [[ $SV_SUPER_OPTIONS == *ZIP_OPENCASCADE* ]]; then
  echo "ZIP_OPENCASCADE"
  REPLACEME_TAR -C zip_output_tmp/ -xvzf tar_output/$SV_TAR_FILE_PREFIX.REPLACEME_SV_OPENCASCADE_DIR.tar.gz
  pushd zip_output_tmp
  REPLACEME_ZIP -r ../zip_output/$SV_ZIP_FILE_PREFIX.REPLACEME_SV_OPENCASCADE_DIR.zip REPLACEME_SV_OPENCASCADE_DIR
  popd
fi

rm -Rf zip_output_tmp
mkdir -p zip_output_tmp

# mmg
if [[ $SV_SUPER_OPTIONS == *ZIP_MMG* ]]; then
  echo "ZIP_MMG"
  REPLACEME_TAR -C zip_output_tmp/ -xvzf tar_output/$SV_TAR_FILE_PREFIX.REPLACEME_SV_MMG_DIR.tar.gz
  pushd zip_output_tmp
  REPLACEME_ZIP -r ../zip_output/$SV_ZIP_FILE_PREFIX.REPLACEME_SV_MMG_DIR.zip REPLACEME_SV_MMG_DIR
  popd
fi

rm -Rf zip_output_tmp
mkdir -p zip_output_tmp

# mitk
if [[ $SV_SUPER_OPTIONS == *ZIP_MITK* ]]; then
  echo "ZIP_MITK"
  REPLACEME_TAR -C zip_output_tmp/ -xvzf tar_output/$SV_TAR_FILE_PREFIX.REPLACEME_SV_MITK_DIR.tar.gz
  pushd zip_output_tmp
  REPLACEME_ZIP -r ../zip_output/$SV_ZIP_FILE_PREFIX.REPLACEME_SV_MITK_DIR.zip REPLACEME_SV_MITK_DIR
  popd
fi

rm -Rf zip_output_tmp
mkdir -p zip_output_tmp
