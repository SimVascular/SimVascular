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

private:
    svMitkMeshIO* IOClone() const override;
};

#endif // SVMITKMESHIO_H
