/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef SV3_PATHGROUP_H
#define SV3_PATHGROUP_H

#include "SimVascular.h"

#include <sv3PathExports.h>

#include "sv3_PathElement.h"
#include "sv_RepositoryData.h"


#include <map>
#include <sstream>
#include <iostream>
#include <string>

namespace sv3{
class SV_EXPORT_PATH PathGroup : public cvRepositoryData
{
public:

    PathGroup();
    
    PathGroup(const PathGroup &other);
    
    virtual ~PathGroup();

    virtual void Expand( unsigned int timeSteps );
    virtual unsigned int GetTimeSize() const;
    virtual int GetSize( unsigned int t = 0 ) const;

    PathElement* GetPathElement(unsigned int t = 0) const;
    void SetPathElement(PathElement* pathElement, unsigned int t = 0);

    void CalculateBoundingBox(double *bounds,unsigned int t = 0 );

    std::string GetName() const;
    void SetName(const std::string& name);

    int GetPathID() const;
    void SetPathID(int pathID);

    void SetSpacing(double spacing);
    double GetSpacing() const;

    void SetMethod(sv3::PathElement::CalculationMethod method = sv3::PathElement::CONSTANT_TOTAL_NUMBER );
    sv3::PathElement::CalculationMethod GetMethod() const;

    void SetCalculationNumber(int number);
    int GetCalculationNumber() const;

  protected:
    
    virtual void ClearData();

    virtual void InitializeEmpty();

    std::vector< sv3::PathElement* > m_PathElementSet;

    bool m_CalculateBoundingBox;

    int m_PathID;

    double m_Spacing;

    sv3::PathElement::CalculationMethod m_Method;

    int m_CalculationNumber;

    // Name is only set when reading in legacy paths.
    std::string m_Name;

  };

}


#endif // SV3_PATHGROUP_H
