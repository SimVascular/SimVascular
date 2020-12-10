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

#ifndef SV4GUI_IMAGE_PATHS_CONTAINER_H
#define SV4GUI_IMAGE_PATHS_CONTAINER_H 

#include <sv3_PathElement.h>

#include <iostream>
#include <vector>
#include "mitkBaseData.h"
#include "vtkPolyData.h"

class sv4guiImagePathsContainer : public mitk::BaseData {

  public:

    mitkClassMacro(sv4guiImagePathsContainer, mitk::BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual void UpdateOutputInformation() {};
    virtual void SetRequestedRegionToLargestPossibleRegion() {};
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() { return false;};
    virtual bool VerifyRequestedRegion() { return true;};
    virtual void SetRequestedRegion(const itk::DataObject *data) {};

    void AddPathElement(sv3::PathElement* pathElement);
    void ClearPathElements();
    const std::vector<sv3::PathElement> &GetPathElements() const;
    void ComputeDistanceMeasure();
    std::array<double,3> GetImageSpacing();
    void SetImageSpacing(std::array<double,3>& imageSpacing);
    double GetDistanceMeasure() const;

  protected:
    mitkCloneMacro(Self);
    sv4guiImagePathsContainer();
    sv4guiImagePathsContainer(const sv4guiImagePathsContainer& other);
    virtual ~sv4guiImagePathsContainer();

  private:
    std::vector<sv3::PathElement> pathElements_;
    std::array<double,3> imageSpacing_;
    double distMeasure_;

};

#endif 
