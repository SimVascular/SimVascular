#ifndef SVMITKMESHIO_H
#define SVMITKMESHIO_H

#include <svMeshExports.h>

#include "svMitkMesh.h"

#include <mitkAbstractFileIO.h>

#include <vtkSmartPointer.h>
#include <vtkPolyData.h>
#include <vtkUnstructuredGrid.h>

class SVMESH_EXPORT svMitkMeshIO : public mitk::AbstractFileIO
{
public:

    svMitkMeshIO();

    using mitk::AbstractFileReader::Read;
    std::vector<mitk::BaseData::Pointer> Read() override;
    mitk::IFileIO::ConfidenceLevel GetReaderConfidenceLevel() const override;

    void Write() override;
    mitk::IFileIO::ConfidenceLevel GetWriterConfidenceLevel() const override;

    static svMitkMesh::Pointer ReadFromFile(std::string fileName, bool readSurfaceMesh, bool readVolumeMesh);

private:
    svMitkMeshIO* IOClone() const override;

//    bool m_ReadMeshData;

//    static svMitkMeshIO* m_Singleton;
};

#endif // SVMITKMESHIO_H
