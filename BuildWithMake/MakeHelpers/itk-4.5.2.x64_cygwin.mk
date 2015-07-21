ifeq ($(CLUSTER), x64_cygwin)
    ITK_SRC_DIR	= $(OPEN_SOFTWARE_SOURCES_TOPLEVEL)/itk-4.5.2
    ITK_BIN_DIR = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/itk-4.5.2
    ITK_DEFS = -D ITK_IO_FACTORY_REGISTER_MANAGER -D MSVC
    ITK_INCDIRS = \
-I$(ITK_BIN_DIR)/Examples/ITKIOFactoryRegistration \
-I$(ITK_SRC_DIR)/Modules/Segmentation/Watersheds/include \
-I$(ITK_SRC_DIR)/Modules/Bridge/VtkGlue/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/Voronoi/include \
-I$(ITK_SRC_DIR)/Modules/Video/IO/include \
-I$(ITK_SRC_DIR)/Modules/Video/Filtering/include \
-I$(ITK_SRC_DIR)/Modules/Video/Core/include \
-I$(ITK_SRC_DIR)/Modules/Bridge/VTK/include \
-I$(ITK_SRC_DIR)/Modules/Core/TestKernel/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/SpatialFunction/include \
-I$(ITK_SRC_DIR)/Modules/Registration/RegistrationMethodsv4/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/RegionGrowing/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/QuadEdgeMeshFiltering/include \
-I$(ITK_SRC_DIR)/Modules/Numerics/NeuralNetworks/include \
-I$(ITK_SRC_DIR)/Modules/Registration/Metricsv4/include \
-I$(ITK_SRC_DIR)/Modules/Numerics/Optimizersv4/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/MarkovRandomFieldsClassifiers/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/LevelSetsv4/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/LabelVoting/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/KLMRegionGrowing/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/ImageFusion/include \
-I$(ITK_SRC_DIR)/Modules/IO/VTK/include \
-I$(ITK_SRC_DIR)/Modules/IO/TransformMatlab/include \
-I$(ITK_SRC_DIR)/Modules/IO/TransformInsightLegacy/include \
-I$(ITK_SRC_DIR)/Modules/IO/TransformHDF5/include \
-I$(ITK_SRC_DIR)/Modules/IO/TransformBase/include \
-I$(ITK_SRC_DIR)/Modules/IO/Stimulate/include \
-I$(ITK_SRC_DIR)/Modules/IO/Siemens/include \
-I$(ITK_SRC_DIR)/Modules/IO/RAW/include \
-I$(ITK_SRC_DIR)/Modules/IO/PNG/include \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/PNG/src \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/PNG/src \
-I$(ITK_SRC_DIR)/Modules/IO/NRRD/include \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/NrrdIO/src/NrrdIO \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/NrrdIO/src/NrrdIO \
-I$(ITK_SRC_DIR)/Modules/IO/NIFTI/include \
-I$(ITK_SRC_DIR)/Modules/IO/Meta/include \
-I$(ITK_SRC_DIR)/Modules/IO/Mesh/include \
-I$(ITK_SRC_DIR)/Modules/IO/MRC/include \
-I$(ITK_SRC_DIR)/Modules/IO/LSM/include \
-I$(ITK_SRC_DIR)/Modules/IO/TIFF/include \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/TIFF/src \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/TIFF/src/itktiff \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/TIFF/src \
-I$(ITK_SRC_DIR)/Modules/IO/JPEG/include \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/JPEG/src \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/JPEG/src \
-I$(ITK_SRC_DIR)/Modules/IO/HDF5/include \
-I$(ITK_SRC_DIR)/Modules/IO/GIPL/include \
-I$(ITK_SRC_DIR)/Modules/IO/GE/include \
-I$(ITK_SRC_DIR)/Modules/IO/IPL/include \
-I$(ITK_SRC_DIR)/Modules/IO/GDCM/include \
-I$(ITK_SRC_DIR)/Modules/IO/CSV/include \
-I$(ITK_SRC_DIR)/Modules/IO/BioRad/include \
-I$(ITK_SRC_DIR)/Modules/IO/BMP/include \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/HDF5/src \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/HDF5/src \
-I$(ITK_SRC_DIR)/Modules/Filtering/GPUThresholding/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/GPUSmoothing/include \
-I$(ITK_SRC_DIR)/Modules/Registration/GPUPDEDeformable/include \
-I$(ITK_SRC_DIR)/Modules/Registration/GPUCommon/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/GPUImageFilterBase/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/GPUAnisotropicSmoothing/include \
-I$(ITK_SRC_DIR)/Modules/Core/GPUFiniteDifference/include \
-I$(ITK_SRC_DIR)/Modules/Core/GPUCommon/include \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GIFTI/src/gifticlib \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/NIFTI/src/nifti/znzlib \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/NIFTI/src/nifti/niftilib \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/DataStructureAndEncodingDefinition \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/MessageExchangeDefinition \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/InformationObjectDefinition \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/Common \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/DataDictionary \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/MediaStorageAndFileFormat \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/Common \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/GDCM \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/OpenJPEG/src/openjpeg \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/OpenJPEG/src/openjpeg \
-I$(ITK_SRC_DIR)/Modules/Registration/FEM/include \
-I$(ITK_SRC_DIR)/Modules/Registration/PDEDeformable/include \
-I$(ITK_SRC_DIR)/Modules/Numerics/FEM/include \
-I$(ITK_SRC_DIR)/Modules/Registration/Common/include \
-I$(ITK_SRC_DIR)/Modules/IO/SpatialObjects/include \
-I$(ITK_SRC_DIR)/Modules/IO/XML/include \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/Expat/src/expat \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/Expat/src/expat \
-I$(ITK_SRC_DIR)/Modules/Numerics/Eigen/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/DisplacementField/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/DiffusionTensorImage/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/Denoising/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/DeformableMesh/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/Deconvolution/include \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/DICOMParser/src/DICOMParser \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/DICOMParser/src/DICOMParser \
-I$(ITK_SRC_DIR)/Modules/Filtering/Convolution/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/FFT/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/Colormap/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/Classifiers/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/BioCell/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/BiasCorrection/include \
-I$(ITK_SRC_DIR)/Modules/Numerics/Polynomials/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/AntiAlias/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/LevelSets/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/SignedDistanceFunction/include \
-I$(ITK_SRC_DIR)/Modules/Numerics/Optimizers/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/ImageFeature/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/ImageSources/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/ImageGradient/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/Smoothing/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/ImageCompare/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/FastMarching/include \
-I$(ITK_SRC_DIR)/Modules/Core/QuadEdgeMesh/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/DistanceMap/include \
-I$(ITK_SRC_DIR)/Modules/Numerics/NarrowBand/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/BinaryMathematicalMorphology/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/LabelMap/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/MathematicalMorphology/include \
-I$(ITK_SRC_DIR)/Modules/Segmentation/ConnectedComponents/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/Thresholding/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/ImageLabel/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/ImageIntensity/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/Path/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/ImageStatistics/include \
-I$(ITK_SRC_DIR)/Modules/Core/SpatialObjects/include \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/MetaIO/src/MetaIO \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/MetaIO/src/MetaIO \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/ZLIB/src \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/ZLIB/src \
-I$(ITK_SRC_DIR)/Modules/Core/Mesh/include \
-I$(ITK_SRC_DIR)/Modules/IO/ImageBase/include \
-I$(ITK_BIN_DIR)/Modules/IO/ImageBase \
-I$(ITK_SRC_DIR)/Modules/Filtering/ImageCompose/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/AnisotropicSmoothing/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/ImageGrid/include \
-I$(ITK_SRC_DIR)/Modules/Core/ImageFunction/include \
-I$(ITK_SRC_DIR)/Modules/Core/Transform/include \
-I$(ITK_SRC_DIR)/Modules/Numerics/Statistics/include \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/Netlib \
-I$(ITK_SRC_DIR)/Modules/Core/ImageAdaptors/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/CurvatureFlow/include \
-I$(ITK_SRC_DIR)/Modules/Filtering/ImageFilterBase/include \
-I$(ITK_SRC_DIR)/Modules/Core/FiniteDifference/include \
-I$(ITK_SRC_DIR)/Modules/Core/Common/include \
-I$(ITK_BIN_DIR)/Modules/Core/Common \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/VNLInstantiation/include \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/VNL/src/vxl/core \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/VNL/src/vxl/vcl \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/VNL/src/vxl/v3p/netlib \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/VNL/src/vxl/core \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/VNL/src/vxl/vcl \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/VNL/src/vxl/v3p/netlib \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/KWSys/src \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/DoubleConversion/src/double-conversion

   ITK_LIBDIRS = $(ITK_BIN_DIR)/lib/RelWithDebInfo
   ITK_LIBS =    $(LIBPATH_COMPILER_FLAG)$(ITK_LIBDIRS) \
$(LIBFLAG)ITKBiasCorrection-4.5.lib     $(LIBFLAG)ITKIOJPEG-4.5.lib                    $(LIBFLAG)ITKOptimizers-4.5.lib \
$(LIBFLAG)ITKBioCell-4.5.lib            $(LIBFLAG)ITKIOLSM-4.5.lib                     $(LIBFLAG)ITKPath-4.5.lib \
$(LIBFLAG)ITKCommon-4.5.lib             $(LIBFLAG)ITKIOMesh-4.5.lib                    $(LIBFLAG)itkpng-4.5.lib \
$(LIBFLAG)ITKDICOMParser-4.5.lib        $(LIBFLAG)ITKIOMeta-4.5.lib                    $(LIBFLAG)ITKPolynomials-4.5.lib \
$(LIBFLAG)itkdouble-conversion-4.5.lib  $(LIBFLAG)ITKIOMRC-4.5.lib                     $(LIBFLAG)ITKQuadEdgeMesh-4.5.lib \
$(LIBFLAG)ITKEXPAT-4.5.lib              $(LIBFLAG)ITKIONIFTI-4.5.lib                   $(LIBFLAG)ITKSpatialObjects-4.5.lib \
$(LIBFLAG)ITKFEM-4.5.lib                $(LIBFLAG)ITKIONRRD-4.5.lib                    $(LIBFLAG)ITKStatistics-4.5.lib \
$(LIBFLAG)itkgdcmCommon-4.5.lib         $(LIBFLAG)ITKIOPNG-4.5.lib                     $(LIBFLAG)itksys-4.5.lib \
$(LIBFLAG)itkgdcmDICT-4.5.lib           $(LIBFLAG)ITKIOSiemens-4.5.lib                 \
$(LIBFLAG)itkgdcmDSED-4.5.lib           $(LIBFLAG)ITKIOSpatialObjects-4.5.lib          $(LIBFLAG)itkTestDriver.lib \
$(LIBFLAG)itkgdcmIOD-4.5.lib            $(LIBFLAG)ITKIOStimulate-4.5.lib               $(LIBFLAG)itktestlib-4.5.lib \
$(LIBFLAG)itkgdcmjpeg12-4.5.lib         $(LIBFLAG)ITKIOTIFF-4.5.lib                    $(LIBFLAG)itktiff-4.5.lib \
$(LIBFLAG)itkgdcmjpeg16-4.5.lib         $(LIBFLAG)ITKIOTransformBase-4.5.lib           $(LIBFLAG)itkv3p_lsqr-4.5.lib \
$(LIBFLAG)itkgdcmjpeg8-4.5.lib          $(LIBFLAG)ITKIOTransformHDF5-4.5.lib           $(LIBFLAG)itkv3p_netlib-4.5.lib \
$(LIBFLAG)itkgdcmMSFF-4.5.lib           $(LIBFLAG)ITKIOTransformInsightLegacy-4.5.lib  $(LIBFLAG)itkvcl-4.5.lib \
$(LIBFLAG)ITKgiftiio-4.5.lib            $(LIBFLAG)ITKIOTransformMatlab-4.5.lib         $(LIBFLAG)ITKVideoCore-4.5.lib \
$(LIBFLAG)itkhdf5_cpp-4.5.lib           $(LIBFLAG)ITKIOVTK-4.5.lib                     $(LIBFLAG)ITKVideoIO-4.5.lib \
$(LIBFLAG)itkhdf5-4.5.lib               $(LIBFLAG)ITKIOXML-4.5.lib                     $(LIBFLAG)itkvnl_algo-4.5.lib \
$(LIBFLAG)ITKIOBioRad-4.5.lib           $(LIBFLAG)itkjpeg-4.5.lib                      $(LIBFLAG)itkvnl-4.5.lib \
$(LIBFLAG)ITKIOBMP-4.5.lib              $(LIBFLAG)ITKKLMRegionGrowing-4.5.lib          $(LIBFLAG)ITKVNLInstantiation-4.5.lib \
$(LIBFLAG)ITKIOCSV-4.5.lib              $(LIBFLAG)ITKLabelMap-4.5.lib                  $(LIBFLAG)ITKVTK-4.5.lib \
$(LIBFLAG)ITKIOGDCM-4.5.lib             $(LIBFLAG)ITKMesh-4.5.lib                      $(LIBFLAG)ITKVtkGlue-4.5.lib \
$(LIBFLAG)ITKIOGE-4.5.lib               $(LIBFLAG)ITKMetaIO-4.5.lib                    $(LIBFLAG)ITKWatersheds-4.5.lib \
$(LIBFLAG)ITKIOGIPL-4.5.lib             $(LIBFLAG)itkNetlibSlatec-4.5.lib              $(LIBFLAG)itkzlib-4.5.lib \
$(LIBFLAG)ITKIOHDF5-4.5.lib             $(LIBFLAG)ITKniftiio-4.5.lib                   $(LIBFLAG)ITKznz-4.5.lib \
$(LIBFLAG)ITKIOImageBase-4.5.lib        $(LIBFLAG)ITKNrrdIO-4.5.lib \
$(LIBFLAG)ITKIOIPL-4.5.lib              $(LIBFLAG)itkopenjpeg-4.5.lib

endif
