ifeq ($(CLUSTER), x64_cygwin)
    ITK_SRC_DIR	= $(OPEN_SOFTWARE_SOURCES_TOPLEVEL)/itk-4.8.0
    ITK_BIN_DIR = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/itk-4.8.0
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
   ITK_LIBS =    /LIBPATH:$(ITK_LIBDIRS) \
ITKBiasCorrection-4.8.lib     ITKIOJPEG-4.8.lib                    ITKOptimizers-4.8.lib \
ITKBioCell-4.8.lib            ITKIOLSM-4.8.lib                     ITKPath-4.8.lib \
ITKCommon-4.8.lib             ITKIOMesh-4.8.lib                    itkpng-4.8.lib \
ITKDICOMParser-4.8.lib        ITKIOMeta-4.8.lib                    ITKPolynomials-4.8.lib \
itkdouble-conversion-4.8.lib  ITKIOMRC-4.8.lib                     ITKQuadEdgeMesh-4.8.lib \
ITKEXPAT-4.8.lib              ITKIONIFTI-4.8.lib                   ITKSpatialObjects-4.8.lib \
ITKFEM-4.8.lib                ITKIONRRD-4.8.lib                    ITKStatistics-4.8.lib \
itkgdcmCommon-4.8.lib         ITKIOPNG-4.8.lib                     itksys-4.8.lib \
itkgdcmDICT-4.8.lib           ITKIOSiemens-4.8.lib                 \
itkgdcmDSED-4.8.lib           ITKIOSpatialObjects-4.8.lib          \
itkgdcmIOD-4.8.lib            ITKIOStimulate-4.8.lib               itktestlib-4.8.lib \
itkgdcmjpeg12-4.8.lib         ITKIOTIFF-4.8.lib                    itktiff-4.8.lib \
itkgdcmjpeg16-4.8.lib         ITKIOTransformBase-4.8.lib           itkv3p_lsqr-4.8.lib \
itkgdcmjpeg8-4.8.lib          ITKIOTransformHDF5-4.8.lib           itkv3p_netlib-4.8.lib \
itkgdcmMSFF-4.8.lib           ITKIOTransformInsightLegacy-4.8.lib  itkvcl-4.8.lib \
ITKgiftiio-4.8.lib            ITKIOTransformMatlab-4.8.lib         ITKVideoCore-4.8.lib \
           ITKIOVTK-4.8.lib                     ITKVideoIO-4.8.lib \
           ITKIOXML-4.8.lib                     itkvnl_algo-4.8.lib \
ITKIOBioRad-4.8.lib           itkjpeg-4.8.lib                      itkvnl-4.8.lib \
ITKIOBMP-4.8.lib              ITKKLMRegionGrowing-4.8.lib          ITKVNLInstantiation-4.8.lib \
ITKIOCSV-4.8.lib              ITKLabelMap-4.8.lib                  ITKVTK-4.8.lib \
ITKIOGDCM-4.8.lib             ITKMesh-4.8.lib                      ITKVtkGlue-4.8.lib \
ITKIOGE-4.8.lib               ITKMetaIO-4.8.lib                    ITKWatersheds-4.8.lib \
ITKIOGIPL-4.8.lib             itkNetlibSlatec-4.8.lib              itkzlib-4.8.lib \
        ITKniftiio-4.8.lib                   ITKznz-4.8.lib \
ITKIOImageBase-4.8.lib        ITKNrrdIO-4.8.lib \
ITKIOIPL-4.8.lib              itkgdcmopenjpeg-4.8.lib	itkgdcmcharls-4.8.lib \
itkhdf5-4.8.lib itkhdf5_cpp-4.8.lib ITKIOHDF5-4.8.lib
endif

#      

# conflicts with vtkhdf5 library itkhdf5-4.8.lib    
