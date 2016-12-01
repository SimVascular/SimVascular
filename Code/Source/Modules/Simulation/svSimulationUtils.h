#ifndef SVSIMULATIONUTILS_H
#define SVSIMULATIONUTILS_H

#include <svSimulationExports.h>

#include "svSimJob.h"

#include <string>

class SVSIMULATION_EXPORT svSimulationUtils
{

public:

    static std::string CreatePreSolverFileContent(svSimJob* job, std::string outputDir="");

    static std::string CreateRCRTFileContent(svSimJob* job);

    static std::string CreateFlowSolverFileContent(svSimJob* job);

};

#endif /* SVSIMULATIONUTILS_H */
