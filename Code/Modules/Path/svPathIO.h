#ifndef SVPATHIO_H
#define SVPATHIO_H

#include "SimVascular.h"

#include <svPathExports.h>

#include "mitkAbstractFileIO.h"

class SVPATH_EXPORT svPathIO : public mitk::AbstractFileIO
{
public:

    svPathIO();

    using mitk::AbstractFileReader::Read;
    std::vector<mitk::BaseData::Pointer> Read() override;
    mitk::IFileIO::ConfidenceLevel GetReaderConfidenceLevel() const override;

    void Write() override;
    mitk::IFileIO::ConfidenceLevel GetWriterConfidenceLevel() const override;

private:
    svPathIO* IOClone() const override;
};

#endif // SVPATHIO_H
