#include "svSimulationUtils.h"

#include <sstream>

std::string svSimulationUtils::CreatePreSolverFileContent(svSimJob* job, std::string flowFilePath)
{
    std::stringstream ss;

    ss<<"mesh_and_adjncy_vtu mesh-complete/mesh-complete.mesh.vtu\n";




    return ss.str();
}

