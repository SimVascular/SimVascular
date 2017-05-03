#ifndef SVMESHADAPTOR_H
#define SVMESHADAPTOR_H

#include <svMeshExports.h>

#include "svMesh.h"

class SVMESH_EXPORT svMeshAdaptor
{

public:

    svMeshAdaptor(){m_Type="";}

    virtual ~svMeshAdaptor(){}

    std::string GetType() const {return m_Type;}

    virtual bool SetModelElement(svModelElement *modelElement) = 0;

//    virtual bool SetResultMesh(vtkSmartPointer<vtkUnstructuredGrid> mesh) = 0;
    virtual bool LoadMeshFromResultVTUFile(std::string filePath) = 0;

    virtual bool SetAdaptOptions(std::string flag, double value) = 0;

    virtual bool Adapt() = 0;

    virtual bool WriteAdaptedSolution(std::string filePath) = 0;

    virtual svMesh* GetAdaptedMesh() = 0;

protected:

    std::string m_Type;

  };

#endif // SVMESHADAPTOR_H
