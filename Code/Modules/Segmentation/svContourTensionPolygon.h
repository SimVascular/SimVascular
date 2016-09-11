#ifndef SVCONTOURTENSIONPOLYGON_H
#define SVCONTOURTENSIONPOLYGON_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContourPolygon.h"

class SVSEGMENTATION_EXPORT svContourTensionPolygon : public svContourPolygon
{

public:

    svContourTensionPolygon();

    svContourTensionPolygon(const svContourTensionPolygon &other);

    virtual ~svContourTensionPolygon();

    virtual svContourTensionPolygon* Clone() override;

    virtual std::string GetClassName() override;

    int GetSubdivisionRounds();

    void SetSubdivisionRounds(int rounds);

    double GetTensionParameter();

    void SetTensionParameter(double parameter);

    virtual void CreateContourPoints() override;

  protected:

    int m_SubdivisionRounds;

    double m_TensionParameter;

  };


#endif // SVCONTOURTENSIONPOLYGON_H
