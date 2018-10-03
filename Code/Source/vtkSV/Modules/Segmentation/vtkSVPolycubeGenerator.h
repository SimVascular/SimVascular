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
 *  \class  vtkSVPolycubeGenerator
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVPolycubeGenerator_h
#define vtkSVPolycubeGenerator_h

#include "vtkSVSegmentationModule.h" // For exports

#include "vtkPolyDataAlgorithm.h"

#include "vtkPolyData.h"
#include "vtkStructuredGrid.h"

#include "vtkSVCenterlineGraph.h"
#include "vtkSVCenterlineGCell.h"

class VTKSVSEGMENTATION_EXPORT vtkSVPolycubeGenerator : public  vtkPolyDataAlgorithm
{
public:
  static vtkSVPolycubeGenerator* New();
  vtkTypeMacro(vtkSVPolycubeGenerator, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Get/Set macro for surface polycube
  vtkSetObjectMacro(SurfacePolycubePd,vtkPolyData);
  vtkGetObjectMacro(SurfacePolycubePd,vtkPolyData);
  //@}

  //@{
  /// \brief Get/Set macro for surface polycube
  vtkSetObjectMacro(VolumePolycubeUg,vtkUnstructuredGrid);
  vtkGetObjectMacro(VolumePolycubeUg,vtkUnstructuredGrid);
  //@}

  //@{
  /// \brief Get the graph for the model
  vtkSetObjectMacro(GraphPd,vtkPolyData);
  vtkGetObjectMacro(GraphPd,vtkPolyData);
  //@}

  //@{
  /// \brief Get the graph for the model
  vtkSetObjectMacro(CenterlineGraph,vtkSVCenterlineGraph);
  vtkGetObjectMacro(CenterlineGraph,vtkSVCenterlineGraph);
  //@}

  //@{
  /// \brief Get/Set macro for array name used by the filter. Must
  //  be present on the centerlines.
  vtkSetStringMacro(CenterlineGroupIdsArrayName);
  vtkGetStringMacro(CenterlineGroupIdsArrayName);
  vtkSetStringMacro(CenterlineRadiusArrayName);
  vtkGetStringMacro(CenterlineRadiusArrayName);
  vtkSetStringMacro(GridIdsArrayName);
  vtkGetStringMacro(GridIdsArrayName);
  //@}

  //@{
  /// \brief Get/Set the unit length for each division of the polycube
  vtkSetMacro(PolycubeUnitLength,double);
  vtkGetMacro(PolycubeUnitLength,double);
  //@}

  //@{
  /// \brief Get/Set the number of divisions to use along width and height of polycube
  vtkSetMacro(PolycubeDivisions,int);
  vtkGetMacro(PolycubeDivisions,int);
  //@}

protected:
  vtkSVPolycubeGenerator();
  ~vtkSVPolycubeGenerator();

  // Usual data generation method
  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **,
                          vtkInformationVector *) override;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  // Surface functions
  int GetSurfacePolycube(const double cubeSize);
  int GetCubePoints(vtkSVCenterlineGCell *gCell,
                    const double height, const double width,
                    vtkPoints *allPoints, vtkCellArray *allCells,
                    vtkIntArray *groupIds, vtkIntArray *patchIds);
  int GetTrifurcationType(vtkSVCenterlineGCell *gCell, int &type);
  int GetSquare(const double startPt[3], const double vec0[3],
                const double vec1[3], const double height, const double width,
                vtkPoints *points);
  int GetWedge(const double pt0[3], const double pt1[3],
               const double pt2[3], const double vec0[3], const double height,
               vtkPoints *points);
  int FormBifurcation(vtkSVCenterlineGCell *gCell,
                      const double pt0[3], const double pt1[3],
                      const double pt2[3], const double pt3[3],
                      const double pt4[3], const double pt5[3],
                      const double centerPt[3],
                      const double factor,
                      double vecs[3][3],
                      double returnPts[2][3]);
  int FormTrifurcation(vtkSVCenterlineGCell *gCell,
                       const double pt0[3], const double pt1[3],
                       const double pt2[3], const double pt3[3],
                       const double pt4[3], const double pt5[3],
                       const double centerPt[3],
                       const double factor,
                       double vecs[3][3],
                       double returnPts[2][3]);
  int GetBifurcationPoint(const double startPt[3],
                          const double vec0[3],
                          const double vec1[3],
                          const double vec2[3],
                          const double factor,
                          double returnPt[3]);
  int GetApproximatePolycubeSize(double &polycubeSize);
  int GetPolycubeDivisions(vtkSVCenterlineGCell *gCell,
                           vtkPolyData *polycubePd,
                           int &w_div, int &h_div, int &l_div);
  int SynchronizePolycubeDivisions(std::vector<std::vector<int> > &whl_divs);
  int SynchronizeSide(vtkSVCenterlineGCell *gCell,
                      const int dim, std::vector<std::vector<int> > &whl_divs);

  // Volume functions
  int GetVolumePolycube();
  int FormParametricHexMesh(vtkSVCenterlineGCell *gCell,
                            vtkPolyData *polycubePd,
                            vtkStructuredGrid *paraHexMesh,
                            int w_div, int h_div, int l_div);
  int PushStructuredGridXAxis(vtkStructuredGrid *paraHexMesh,
                              const double midPt0[3],
                              const double midPt1[3],
                              const int isBottom);
  int PushStructuredGridZAxis(vtkStructuredGrid *paraHexMesh,
                              const double midPt0[3],
                              const double midPt1[3],
                              const int isBottom);
  int CheckFace(vtkPolyData *polycubePd, int faceId,
                int &nTopPts, int &nBotPts,
                int &flatTop, int &flatBot);

  int GetMinimumEdgeLength(vtkPolyData *polycubePd,
                           double &minEdgeLength);

private:
  vtkSVPolycubeGenerator(const vtkSVPolycubeGenerator&);  // Not implemented.
  void operator=(const vtkSVPolycubeGenerator&);  // Not implemented.

  char *CenterlineGroupIdsArrayName;
  char *CenterlineRadiusArrayName;
  char *GridIdsArrayName;

  vtkSVCenterlineGraph *CenterlineGraph;
  vtkPolyData *WorkPd;
  vtkPolyData *GraphPd;
  vtkPolyData *SurfacePolycubePd;

  vtkUnstructuredGrid *VolumePolycubeUg;

  int PolycubeDivisions;

  double PolycubeUnitLength;

};

#endif
