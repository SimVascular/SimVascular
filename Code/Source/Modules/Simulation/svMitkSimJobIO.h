#ifndef SVMITKSIMJOBIO_H
#define SVMITKSIMJOBIO_H

#include <svSimulationExports.h>

#include "mitkAbstractFileIO.h"

class SVSIMULATION_EXPORT svMitkSimJobIO : public mitk::AbstractFileIO
{
public:

    svMitkSimJobIO();

    using mitk::AbstractFileReader::Read;
    std::vector<mitk::BaseData::Pointer> Read() override;
    mitk::IFileIO::ConfidenceLevel GetReaderConfidenceLevel() const override;

    void Write() override;
    mitk::IFileIO::ConfidenceLevel GetWriterConfidenceLevel() const override;

private:
    svMitkSimJobIO* IOClone() const override;
};

#endif // SVMITKSIMJOBIO_H
