ifeq ($(CLUSTER), x64_cygwin)
    ITK_SRC_DIR	= $(OPEN_SOFTWARE_SOURCES_TOPLEVEL)/itk-4.8.0
    ITK_BIN_DIR = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/itk-4.8.0
    ITK_DEFS = -D ITK_IO_FACTORY_REGISTER_MANAGER -D MSVC
    ITK_LIBDIRS = $(ITK_BIN_DIR)/lib/RelWithDebInfo

    ITK_SYS_LIBS  = $(LIBFLAG)Rpcrt4$(LIBLINKEXT) $(LIBFLAG)Ws2_32$(LIBLINKEXT)

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

   ITK_LIBS =    $(LIBPATH_COMPILER_FLAG)$(ITK_LIBDIRS) \
$(LIBFLAG)ITKBiasCorrection-4.8$(LIBLINKEXT)     $(LIBFLAG)ITKIOJPEG-4.8$(LIBLINKEXT)                    $(LIBFLAG)ITKOptimizers-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKBioCell-4.8$(LIBLINKEXT)            $(LIBFLAG)ITKIOLSM-4.8$(LIBLINKEXT)                     $(LIBFLAG)ITKPath-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKCommon-4.8$(LIBLINKEXT)             $(LIBFLAG)ITKIOMesh-4.8$(LIBLINKEXT)                    $(LIBFLAG)itkpng-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKDICOMParser-4.8$(LIBLINKEXT)        $(LIBFLAG)ITKIOMeta-4.8$(LIBLINKEXT)                    $(LIBFLAG)ITKPolynomials-4.8$(LIBLINKEXT) \
$(LIBFLAG)itkdouble-conversion-4.8$(LIBLINKEXT)  $(LIBFLAG)ITKIOMRC-4.8$(LIBLINKEXT)                     $(LIBFLAG)ITKQuadEdgeMesh-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKEXPAT-4.8$(LIBLINKEXT)              $(LIBFLAG)ITKIONIFTI-4.8$(LIBLINKEXT)                   $(LIBFLAG)ITKSpatialObjects-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKFEM-4.8$(LIBLINKEXT)                $(LIBFLAG)ITKIONRRD-4.8$(LIBLINKEXT)                    $(LIBFLAG)ITKStatistics-4.8$(LIBLINKEXT) \
$(LIBFLAG)itkgdcmCommon-4.8$(LIBLINKEXT)         $(LIBFLAG)ITKIOPNG-4.8$(LIBLINKEXT)                     $(LIBFLAG)itksys-4.8$(LIBLINKEXT) \
$(LIBFLAG)itkgdcmMSFF-4.8$(LIBLINKEXT)        \
$(LIBFLAG)itkgdcmDICT-4.8$(LIBLINKEXT)           $(LIBFLAG)ITKIOSiemens-4.8$(LIBLINKEXT)                 \
$(LIBFLAG)itkgdcmDSED-4.8$(LIBLINKEXT)           $(LIBFLAG)ITKIOSpatialObjects-4.8$(LIBLINKEXT)          \
$(LIBFLAG)itkgdcmIOD-4.8$(LIBLINKEXT)            $(LIBFLAG)ITKIOStimulate-4.8$(LIBLINKEXT)               $(LIBFLAG)itktestlib-4.8$(LIBLINKEXT) \
$(LIBFLAG)itkgdcmjpeg12-4.8$(LIBLINKEXT)         $(LIBFLAG)ITKIOTIFF-4.8$(LIBLINKEXT)                    $(LIBFLAG)itktiff-4.8$(LIBLINKEXT) \
$(LIBFLAG)itkgdcmjpeg16-4.8$(LIBLINKEXT)         $(LIBFLAG)ITKIOTransformBase-4.8$(LIBLINKEXT)           $(LIBFLAG)itkv3p_lsqr-4.8$(LIBLINKEXT) \
$(LIBFLAG)itkgdcmjpeg8-4.8$(LIBLINKEXT)          \
$(LIBFLAG)itkgdcmCommon-4.8$(LIBLINKEXT)         \
$(LIBFLAG)ITKIOTransformHDF5-4.8$(LIBLINKEXT)           $(LIBFLAG)itkv3p_netlib-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKIOTransformInsightLegacy-4.8$(LIBLINKEXT)  $(LIBFLAG)itkvcl-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKgiftiio-4.8$(LIBLINKEXT)            $(LIBFLAG)ITKIOTransformMatlab-4.8$(LIBLINKEXT)         $(LIBFLAG)ITKVideoCore-4.8$(LIBLINKEXT) \
           $(LIBFLAG)ITKIOVTK-4.8$(LIBLINKEXT)                     $(LIBFLAG)ITKVideoIO-4.8$(LIBLINKEXT) \
           $(LIBFLAG)ITKIOXML-4.8$(LIBLINKEXT)                     $(LIBFLAG)itkvnl_algo-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKIOBioRad-4.8$(LIBLINKEXT)           $(LIBFLAG)itkjpeg-4.8$(LIBLINKEXT)                      $(LIBFLAG)itkvnl-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKIOBMP-4.8$(LIBLINKEXT)              $(LIBFLAG)ITKKLMRegionGrowing-4.8$(LIBLINKEXT)          $(LIBFLAG)ITKVNLInstantiation-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKIOCSV-4.8$(LIBLINKEXT)              $(LIBFLAG)ITKLabelMap-4.8$(LIBLINKEXT)                  $(LIBFLAG)ITKVTK-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKIOGDCM-4.8$(LIBLINKEXT)             $(LIBFLAG)ITKMesh-4.8$(LIBLINKEXT)                      $(LIBFLAG)ITKVtkGlue-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKIOGE-4.8$(LIBLINKEXT)               $(LIBFLAG)ITKMetaIO-4.8$(LIBLINKEXT)                    $(LIBFLAG)ITKWatersheds-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKIOGIPL-4.8$(LIBLINKEXT)             $(LIBFLAG)itkNetlibSlatec-4.8$(LIBLINKEXT)              $(LIBFLAG)itkzlib-4.8$(LIBLINKEXT) \
        $(LIBFLAG)ITKniftiio-4.8$(LIBLINKEXT)                   $(LIBFLAG)ITKznz-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKIOImageBase-4.8$(LIBLINKEXT)        $(LIBFLAG)ITKNrrdIO-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKIOIPL-4.8$(LIBLINKEXT)              $(LIBFLAG)itkgdcmopenjpeg-4.8$(LIBLINKEXT)	$(LIBFLAG)itkgdcmcharls-4.8$(LIBLINKEXT) \
$(LIBFLAG)itkhdf5-4.8$(LIBLINKEXT) $(LIBFLAG)itkhdf5_cpp-4.8$(LIBLINKEXT) $(LIBFLAG)ITKIOHDF5-4.8$(LIBLINKEXT) $(LIBFLAG)itkhdf5-4.8$(LIBLINKEXT) \
$(LIBFLAG)ITKEXPAT-4.8$(LIBLINKEXT) \
$(ITK_SYS_LIBS)
endif

#      

# conflicts with vtkhdf5 library itkhdf5-4.8.lib    
