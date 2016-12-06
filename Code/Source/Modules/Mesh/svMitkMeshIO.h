#ifndef SVMITKMESHIO_H
#define SVMITKMESHIO_H

#include <svMeshExports.h>

#include "mitkAbstractFileIO.h"

class SVMESH_EXPORT svMitkMeshIO : public mitk::AbstractFileIO
{
public:

    svMitkMeshIO();

    using mitk::AbstractFileReader::Read;
    std::vector<mitk::BaseData::Pointer> Read() override;
    mitk::IFileIO::ConfidenceLevel GetReaderConfidenceLevel() const override;

    void Write() override;
    mitk::IFileIO::ConfidenceLevel GetWriterConfidenceLevel() const override;

    void SetReadMeshData(bool read);

    static svMitkMeshIO* GetSingleton();

private:
    svMitkMeshIO* IOClone() const override;

    bool m_ReadMeshData;

    static svMitkMeshIO* m_Singleton;
};

#endif // SVMITKMESHIO_H
