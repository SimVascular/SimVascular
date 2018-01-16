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
 * \class vtkSVGeneralizedPolycube
 *
 * \brief Derived from vtkUnstructuredGrid, this class defines a structure
 * that abstracts a domain to a polycube type structure. Specific point and cell data
 * are defined on the object, which makes it different than a typical
 * unstructured grid. In addition, special function to set cube/grids are
 * provided
 *
 * \details When setting the cube, we need to make sure they are ordered in
 * a certain way for the point data to actually correspond to something in the
 * physical model. This is how we order the nodes.
 * \verbatim
 *       z  y
 *       | /
 *       0 -- x
 *               2---------1
 *              /|        /|
 *             / |       / |
 *            3---------0  |
 *            |  |      |  |
 *            |  6------|--5
 *            | /       | /
 *            |/        |/
 *            7---------4
 * \endverbatim
 *
 * \author Adam Updegrove
 * \author updega2@gmail.com
 * \author UC Berkeley
 * \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVGeneralizedPolycube_h
#define vtkSVGeneralizedPolycube_h

#include "vtkUnstructuredGrid.h"
#include "vtkSVParameterizationModule.h" // For exports

#include "vtkStructuredGrid.h"
#include "vtkDenseArray.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkPolyData.h"

class VTKSVPARAMETERIZATION_EXPORT vtkSVGeneralizedPolycube : public vtkUnstructuredGrid
{
public:
  static vtkSVGeneralizedPolycube *New();
  vtkSVGeneralizedPolycube(int m, vtkPoints *controlPoints, int n, vtkDoubleArray *knotPoints, int deg) {;}
  vtkSVGeneralizedPolycube(int m, vtkPoints *controlPoints, vtkDoubleArray *knotPoints, vtkIntArray *knotMultiplicity, int deg) {;}

  void PrintSelf(ostream& os, vtkIndent indent);

  //@{
  /// \brief Surgerylines that define a more exact path in between individual starting points of the polycube. Do not have to be used
  vtkGetObjectMacro(SurgeryLines, vtkPolyData);
  vtkSetObjectMacro(SurgeryLines, vtkPolyData);
  //@}

  /** \brief Two types of cube that are treated differently when paramterized.
   *  CUBE_BRANCH is essentially closed with a seam in it. All four sides of the
   *  cube need to be parameterized.
   *  CUBE_BIFURCATION is open and only three sides of the cube are paramterized. */
  enum CUBE_TYPE
  {
    CUBE_BRANCH = 0,
    CUBE_BIFURCATION
  };

  // Initialize
  void Initialize();

  // Origin is front, left, bottom corner of cube when axis aligned
  /** \brief insert a grid into the structure. Does not need to be pre-allocated.
   *  \param cellId Cell to insert.
   *  \param origin Origin of cube (front, left, bottom corner).
   *  \param dims The dimensions of the cube, x, y, z.
   *  \param cubetype CUBE_BRANCH or CUBE_BIFURCATION.
   *  \param parentdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping.
   *  \param childdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping. */
  int InsertGridWithOrigin(const int cellId, const double origin[3], const double dims[3],
                        const int cubetype, const int parentdirection, const int childdirection);

  /** \brief Set a grid. Must have alread used SetNumberOfGrids() to allocate.
   *  \param cellId Cell to insert.
   *  \param origin Origin of cube (front, left, bottom corner).
   *  \param dims The dimensions of the cube, x, y, z.
   *  \param cubetype CUBE_BRANCH or CUBE_BIFURCATION.
   *  \param parentdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping.
   *  \param childdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping. */
  int SetGridWithOrigin(const int cellId, const double origin[3], const double dims[3],
                        const int cubetype, const int parentdirection, const int childdirection);

  /** \brief Set a grid. Must have alread used SetNumberOfGrids() to allocate.
   *  \param cellId Cell to insert.
   *  \param origin Origin of cube (front, left, bottom corner).
   *  \param dims The dimensions of the cube, x, y, z.
   *  \param cubetype CUBE_BRANCH or CUBE_BIFURCATION.
   *  \param parentdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping.
   *  \param childdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping.
   *  \param topNormal Vector aligning bottom to top of cube in the true object physical space.
   *  \param rightNormal Vector aligning left to right of cube in the true object physical space.*/
  int SetGridWithOrigin(const int cellId, const double origin[3], const double dims[3],
                        const int cubetype, const int parentdirection, const int childdirection,
                        const double topNormal[3], const double rightNormal[3], const int corners[4]);

  /** \brief insert a grid into the structure. Does not need to be pre-allocated.
   *  \param cellId Cell to insert.
   *  \param center Center of the cube..
   *  \param dims The dimensions of the cube, x, y, z.
   *  \param cubetype CUBE_BRANCH or CUBE_BIFURCATION.
   *  \param parentdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping.
   *  \param childdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping. */
  int InsertGridWithCenter(const int cellId, const double center[3], const double dims[3],
                        const int cubetype, const int parentdirection, const int childdirection);


  /** \brief Set a grid. Must have alread used SetNumberOfGrids() to allocate.
   *  \param cellId Cell to insert.
   *  \param center Center of the cube..
   *  \param dims The dimensions of the cube, x, y, z.
   *  \param cubetype CUBE_BRANCH or CUBE_BIFURCATION.
   *  \param parentdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping.
   *  \param childdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping. */
  int SetGridWithCenter(const int cellId, const double center[3], const double dims[3],
                        const int cubetype, const int parentdirection, const int childdirection);


  /** \brief insert a grid into the structure. Does not need to be pre-allocated.
   *  \param cellId Cell to insert.
   *  \param center Center of the cube.
   *  \param points point set containing 8 points. Need to be ordered according to point
   *  layout to work for parameterization.
   *  \param parentdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping.
   *  \param childdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping. */
  int InsertGrid(const int cellId, vtkPoints *points, const int cubetype, const int parentdirection, const int childdirection);

  /** \brief Set a grid. Must have alread used SetNumberOfGrids() to allocate.
   *  \param cellId Cell to insert.
   *  \param center Center of the cube.
   *  \param points point set containing 8 points. Need to be ordered according to point
   *  layout to work for parameterization.
   *  \param parentdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping.
   *  \param childdirection RIGHT, LEFT, FRONT, BACK, UP, or DOWN. Needed to retrieve
   *  correct node order for mapping. */
  int SetGrid(const int cellId, vtkPoints *points, const int cubetype, const int parentdirection, const int childdirection);

  int GetGrid(const int cellId, const int spacing, vtkStructuredGrid *gridRepresentation) {return 0;} /**< \brief Unimplemented */

  /** \brief Used to allocate space for certain number of grids. */
  void SetNumberOfGrids(const int numberOfGrids);

  /** \brief Return the number of grids. Should be same as GetNumberOfCells(). */
  int GetNumberOfGrids();

protected:
  vtkSVGeneralizedPolycube();
  ~vtkSVGeneralizedPolycube();

  vtkPolyData    *SurgeryLines;

private:
  vtkSVGeneralizedPolycube(const vtkSVGeneralizedPolycube&);  // Not implemented.
  void operator=(const vtkSVGeneralizedPolycube&);  // Not implemented.
};

#endif
