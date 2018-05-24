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

/*===================================================================

The Medical Imaging Interaction Toolkit (MITK)

Copyright (c) German Cancer Research Center,
Division of Medical and Biological Informatics.
All rights reserved.

This software is distributed WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.

See LICENSE.txt or http://www.mitk.org for details.

===================================================================*/

#ifndef SV4GUI_SURFACE_H
#define SV4GUI_SURFACE_H

#include "SimVascular.h"
#include <sv4guiModuleSegmentationExports.h>

#include "mitkBaseData.h"
#include "itkImageRegion.h"
#include <vtkSmartPointer.h>

class vtkPolyData;

namespace mitk
{
  /**
    * \brief Class for storing surfaces (vtkPolyData).
    * \ingroup Data
    */
  class SV4GUIMODULESEGMENTATION_EXPORT sv4guiSurface : public BaseData
  {
  public:
    typedef itk::ImageRegion<5> RegionType;

    mitkClassMacro(sv4guiSurface, BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    void CalculateBoundingBox();
    virtual void CopyInformation(const itk::DataObject *data) override;
    virtual void ExecuteOperation(Operation *operation) override;
    virtual void Expand( unsigned int timeSteps = 1 ) override;
    const RegionType& GetLargestPossibleRegion() const;
    virtual const RegionType& GetRequestedRegion() const;
    unsigned int GetSizeOfPolyDataSeries() const;
    virtual vtkPolyData* GetVtkPolyData(unsigned int t = 0) const;
    virtual void Graft( const DataObject* data ) override;
    virtual bool IsEmptyTimeStep(unsigned int t) const override;
    virtual void PrintSelf( std::ostream& os, itk::Indent indent ) const override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;
    virtual void SetRequestedRegion(sv4guiSurface::RegionType *region);
    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual void SetVtkPolyData(vtkPolyData* polydata, unsigned int t = 0);
    virtual void Swap(sv4guiSurface& other);
    virtual void Update() override;
    virtual void UpdateOutputInformation() override;
    virtual bool VerifyRequestedRegion() override;


    //protected:
    mitkCloneMacro(Self);

    sv4guiSurface();
    virtual ~sv4guiSurface();

    sv4guiSurface(const sv4guiSurface& other);
    sv4guiSurface& operator=(sv4guiSurface other);

    virtual void ClearData() override;
    virtual void InitializeEmpty() override;

  private:
    std::vector< vtkSmartPointer<vtkPolyData> > m_PolyDatas;
    mutable RegionType m_LargestPossibleRegion;
    mutable RegionType m_RequestedRegion;
    bool m_CalculateBoundingBox;
  };

  /**
  * @brief Equal Compare two surfaces for equality, returns true if found equal.
  * @warning This method is deprecated and will not be available in the future. Use the \a bool mitk::Equal(const mitk::sv4guiSurface& s1, const mitk::sv4guiSurface& s2) instead
  * @ingroup MITKTestingAPI
  * @param rightHandSide sv4guiSurface to compare.
  * @param leftHandSide sv4guiSurface to compare.
  * @param eps Epsilon to use for floating point comparison. Most of the time mitk::eps will be sufficient.
  * @param verbose Flag indicating if the method should give a detailed console output.
  * @return True if every comparison is true, false in any other case.
  */
DEPRECATED( MITKCORE_EXPORT bool Equal( mitk::sv4guiSurface* leftHandSide, mitk::sv4guiSurface* rightHandSide, mitk::ScalarType eps, bool verbose));

/**
* @brief Equal Compare two surfaces for equality, returns true if found equal.
* @ingroup MITKTestingAPI
* @param rightHandSide sv4guiSurface to compare.
* @param leftHandSide sv4guiSurface to compare.
* @param eps Epsilon to use for floating point comparison. Most of the time mitk::eps will be sufficient.
* @param verbose Flag indicating if the method should give a detailed console output.
* @return True if every comparison is true, false in any other case.
*/
MITKCORE_EXPORT bool Equal( mitk::sv4guiSurface& leftHandSide, mitk::sv4guiSurface& rightHandSide, mitk::ScalarType eps, bool verbose);

  /**
  * @brief Equal Compare two vtk PolyDatas for equality, returns true if found equal.
  * @warning This method is deprecated and will not be available in the future. Use the \a bool mitk::Equal(const vtkPolyData& p1, const vtkPolyData& p2) instead
  * @ingroup MITKTestingAPI
  * @param rightHandSide sv4guiSurface to compare.
  * @param leftHandSide sv4guiSurface to compare.
  * @param eps Epsilon to use for floating point comparison. Most of the time mitk::eps will be sufficient.
  * @param verbose Flag indicating if the method should give a detailed console output.
  * @return True if every comparison is true, false in any other case.
  *
  * This will only check if the number of cells, vertices, polygons, stripes and lines is the same and whether
  * all the two poly datas have the same number of points with the same coordinates. It is not checked whether
  * all points are correctly connected.
  */
DEPRECATED( MITKCORE_EXPORT bool Equal( vtkPolyData* leftHandSide, vtkPolyData* rightHandSide, mitk::ScalarType eps, bool verbose));

/**
* @brief Equal Compare two vtk PolyDatas for equality, returns true if found equal.
* @ingroup MITKTestingAPI
* @param rightHandSide sv4guiSurface to compare.
* @param leftHandSide sv4guiSurface to compare.
* @param eps Epsilon to use for floating point comparison. Most of the time mitk::eps will be sufficient.
* @param verbose Flag indicating if the method should give a detailed console output.
* @return True if every comparison is true, false in any other case.
*
* This will only check if the number of cells, vertices, polygons, stripes and lines is the same and whether
* all the two poly datas have the same number of points with the same coordinates. It is not checked whether
* all points are correctly connected.
*/
MITKCORE_EXPORT bool Equal( vtkPolyData& leftHandSide, vtkPolyData& rightHandSide, mitk::ScalarType eps, bool verbose);
}

#endif // SV4GUI_SURFACE_H

