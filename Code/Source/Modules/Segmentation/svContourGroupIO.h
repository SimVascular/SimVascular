#ifndef SVCONTOURGROUPIO_H
#define SVCONTOURGROUPIO_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "mitkAbstractFileIO.h"

class SVSEGMENTATION_EXPORT svContourGroupIO : public mitk::AbstractFileIO
{
public:

    svContourGroupIO();

    using mitk::AbstractFileReader::Read;
    std::vector<mitk::BaseData::Pointer> Read() override;
    mitk::IFileIO::ConfidenceLevel GetReaderConfidenceLevel() const override;

    void Write() override;
    mitk::IFileIO::ConfidenceLevel GetWriterConfidenceLevel() const override;

private:
    svContourGroupIO* IOClone() const override;
};

#endif // SVCONTOURGROUPIO_H
