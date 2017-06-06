#ifndef SVSIMULATIONUTILS_H
#define SVSIMULATIONUTILS_H

#include <svSimulationExports.h>

#include "svSimJob.h"

#include <string>
#include <vector>

#include <vtkSmartPointer.h>
#include <vtkPolyData.h>
#include <vtkUnstructuredGrid.h>

class SVSIMULATION_EXPORT svSimulationUtils
{

public:

    static std::string CreatePreSolverFileContent(svSimJob* job, std::string outputDir="");

    static std::string CreateRCRTFileContent(svSimJob* job);

    static std::string CreateCORTFileContent(svSimJob* job);

    static std::string CreateFlowSolverFileContent(svSimJob* job);

    static bool CreateFlowFiles(std::string outFlowFilePath, std::string outPressureFlePath
                                                   , std::string outAverageFilePath, std::string outAverageUnitsFilePath
                                                   , std::string vtxFilePath
                                                   , std::string meshFaceDir, std::vector<std::string> meshFaceFileNames
                                                   , std::string unit, bool skipWalls);

    static void VtpExtractSingleFace(vtkSmartPointer<vtkPolyData> simvtp,vtkSmartPointer<vtkPolyData> facevtp);

    static void VtuExtractSingleFace(vtkSmartPointer<vtkUnstructuredGrid> simug,vtkSmartPointer<vtkPolyData> facevtp);

    static void VtpIntegrateFace(vtkSmartPointer<vtkPolyData>facevtp, std::map<std::string, double>& pmap, std::map<std::string, double>& qmap, std::map<std::string, double>& amap);
};

#endif /* SVSIMULATIONUTILS_H */
