#ifndef sv4guiMitksvFSIJOBIO_H
#define sv4guiMitksvFSIJOBIO_H

#include <svFSIExports.h>

#include <mitkAbstractFileIO.h>

class SVFSI_EXPORT sv4guiMitksvFSIJobIO : public mitk::AbstractFileIO
{
public:

    sv4guiMitksvFSIJobIO();

    using mitk::AbstractFileReader::Read;
    std::vector<mitk::BaseData::Pointer> Read() override;
    mitk::IFileIO::ConfidenceLevel GetReaderConfidenceLevel() const override;

    void Write() override;
    mitk::IFileIO::ConfidenceLevel GetWriterConfidenceLevel() const override;

private:
    sv4guiMitksvFSIJobIO* IOClone() const override;
};

#endif // sv4guiMitksvFSIJOBIO_H
