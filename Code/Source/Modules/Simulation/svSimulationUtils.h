#ifndef SVSIMULATIONUTILS_H
#define SVSIMULATIONUTILS_H

#include <svSimulationExports.h>

#include "svSimJob.h"

#include <string>

class SVSIMULATION_EXPORT svSimulationUtils
{

public:

    static std::string CreatePreSolverFileContent(svSimJob* job, std::string flowFilePath);


};

#endif /* SVSIMULATIONUTILS_H */
