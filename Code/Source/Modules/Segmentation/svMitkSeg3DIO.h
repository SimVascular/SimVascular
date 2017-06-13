#ifndef SVMITKSEG3DIO_H
#define SVMITKSEG3DIO_H

#include <svSegmentationExports.h>

#include "mitkAbstractFileIO.h"

class SVSEGMENTATION_EXPORT svMitkSeg3DIO : public mitk::AbstractFileIO
{
public:

    svMitkSeg3DIO();

    using mitk::AbstractFileReader::Read;
    std::vector<mitk::BaseData::Pointer> Read() override;
    static std::vector<mitk::BaseData::Pointer> ReadFile(std::string fileName);
    mitk::IFileIO::ConfidenceLevel GetReaderConfidenceLevel() const override;

    void Write() override;
    mitk::IFileIO::ConfidenceLevel GetWriterConfidenceLevel() const override;

private:
    svMitkSeg3DIO* IOClone() const override;
};

#endif // SVMITKSEG3DIO_H
