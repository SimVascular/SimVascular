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

/**
 *  \class vtkSVControlGrid
 *  \brief This is a class whose parent is vtkStructuredGrid. It is
 *  essentially a structured grid with extra cell and point data
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVControlGrid_h
#define vtkSVControlGrid_h

#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkStructuredGrid.h"

#include "vtkSVNURBSModule.h"

class VTKSVNURBS_EXPORT vtkSVControlGrid : public vtkStructuredGrid
{
public:
  static vtkSVControlGrid *New();

  vtkTypeMacro(vtkSVControlGrid,vtkStructuredGrid);
  void PrintSelf(ostream& os, vtkIndent indent) override;


  /// \brief Copy the geometric and topological structure of an input poly data object.
  void CopyStructure(vtkDataSet *ds) override;

  /// \brief Initialize to empty structured grid
  void Initialize() override;

  /** \brief Set the number of control points, needs to be called before
   *  and SetControlPoint(. */
  int SetNumberOfControlPoints(const int numPoints);

  /** \brief Set a control point using i, j, k location. Space must be pre-allocated with Allocate.
   *  \param i location in first axis of structured grid.
   *  \param j location in second axis of structured grid.
   *  \param k location in third axis of structured grid.
   *  \param p0 x 3d location to set control point to.
   *  \param p1 y 3d location to set control point to.
   *  \param p2 z 3d location to set control point to.
   *  \param w weight to associate with control point. */
  int SetControlPoint(const int i, const int j, const int k, const double p0, const double p1, const double p2, const double w);

  /** \brief Set a control point using i, j, k location. Space must be pre-allocated with Allocate.
   *  \param i location in first axis of structured grid.
   *  \param j location in second axis of structured grid.
   *  \param k location in third axis of structured grid.
   *  \param p 3d location to set control point to.
   *  \param w weight to associate with control point. */
  int SetControlPoint(const int i, const int j, const int k, const double p[3], const double w);

  /** \brief Set a control point using i, j, k location. Space must be pre-allocated with Allocate.
   *  \param i location in first axis of structured grid.
   *  \param j location in second axis of structured grid.
   *  \param k location in third axis of structured grid.
   *  \param pw 3d location to set control point to, fourth index in double is weight. */
  int SetControlPoint(const int i, const int j, const int k, const double pw[4]);

  /** \brief Insert a control point using i, j, k location.
   *  \param i location in first axis of structured grid.
   *  \param j location in second axis of structured grid.
   *  \param k location in third axis of structured grid.
   *  \param p 3d location to set control point to.
   *  \param w weight to associate with control point. */
  int InsertControlPoint(const int i, const int j, const int k, const double p[3], const double w);

  /** \brief Insert a control point using i, j, k location.
   *  \param i location in first axis of structured grid.
   *  \param j location in second axis of structured grid.
   *  \param k location in third axis of structured grid.
   *  \param pw 3d location to set control point to, fourth index in double is weight. */
  int InsertControlPoint(const int i, const int j, const int k, const double pw[4]);

  /** \brief Get a control point at given location
   *  \param i location in first axis of structured grid.
   *  \param j location in second axis of structured grid.
   *  \param k location in third axis of structured grid.
   *  \param p 3d location of control point.
   *  \param w weight associated with control point. */
  int GetControlPoint(const int i, const int j, const int k, double p[3], double &weight);

  /** \brief Get a control point at given location
   *  \param i location in first axis of structured grid.
   *  \param j location in second axis of structured grid.
   *  \param k location in third axis of structured grid.
   *  \param pw 3d location of control point. Fourth index is weight. */
  int GetControlPoint(const int i, const int j, const int k, double pw[4]);

  /** \brief Get point Id in point set of location i,j,k.
   *  Because pointset is treated like a separate object, the point id may
   *  not necessarily be what you expect from i,j,k.
   *  \param i location in first axis of structured grid.
   *  \param j location in second axis of structured grid.
   *  \param k location in third axis of structured grid.
   *  \param ptId Point id at location */
  int GetPointId(const int i, const int j, const int k, int &ptId);

  /** \brief Get point Id in point set of location i,j,k.
   *  Because pointset is treated like a separate object, the point id may
   *  not necessarily be what you expect from i,j,k.
   *  \param i location in first axis of structured grid.
   *  \param j location in second axis of structured grid.
   *  \param k location in third axis of structured grid.
   *  \return ptId Point id at location */
  int GetPointId(const int i, const int j, const int k);

  //@{
  /// \brief Get dimensions of this structured points dataset.
  virtual int *GetDimensions () override {return vtkStructuredGrid::GetDimensions();}
  virtual void GetDimensions (int dim[3]) override {vtkStructuredGrid::GetDimensions(dim);}
  //@}

  //@{
  /// \brief Retrieve an instance of this class from an information object.
  static vtkSVControlGrid* GetData(vtkInformation* info);
  static vtkSVControlGrid* GetData(vtkInformationVector* v, int i=0);
  //@}

protected:
  vtkSVControlGrid();
  virtual ~vtkSVControlGrid();

  virtual void ComputeScalarRange() override {vtkStructuredGrid::GetScalarRange();}

private:
  vtkSVControlGrid(const vtkSVControlGrid&);  // Not implemented.
  void operator=(const vtkSVControlGrid&);  // Not implemented.
};

#endif
