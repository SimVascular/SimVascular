#ifndef SVMODELIO_H
#define SVMODELIO_H

#include <svModelExports.h>

#include "mitkAbstractFileIO.h"

class SVMODEL_EXPORT svModelIO : public mitk::AbstractFileIO
{
public:

    svModelIO();

    using mitk::AbstractFileReader::Read;
    std::vector<mitk::BaseData::Pointer> Read() override;
    mitk::IFileIO::ConfidenceLevel GetReaderConfidenceLevel() const override;

    void Write() override;
    mitk::IFileIO::ConfidenceLevel GetWriterConfidenceLevel() const override;

private:
    svModelIO* IOClone() const override;
};

#endif // SVMODELIO_H
