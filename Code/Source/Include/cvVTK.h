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

#ifndef __CV_VTK_H
#define __CV_VTK_H

#include "vtkToolkits.h"
#include "vtkAbstractMapper.h"
#include "vtkAbstractTransform.h"
#include "vtkActor2D.h"
#include "vtkActor2DCollection.h"
#include "vtkAssemblyNode.h"
#include "vtkAssemblyPath.h"
#include "vtkAssemblyPaths.h"
#include "vtkBitArray.h"
#include "vtkByteSwap.h"
#include "vtkCell.h"
#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCellLinks.h"
#include "vtkCellType.h"
#include "vtkCellTypes.h"
#include "vtkCharArray.h"
#include "vtkCollection.h"
#include "vtkCommand.h"
#include "vtkContourValues.h"
#include "vtkCoordinate.h"
#include "vtkCriticalSection.h"
#include "vtkDataArray.h"
#include "vtkDataObject.h"
#include "vtkDataObjectCollection.h"
#include "vtkDataSet.h"
#include "vtkDataSetAttributes.h"
#include "vtkDataSetCollection.h"
#include "vtkDebugLeaks.h"
#include "vtkDirectory.h"
#include "vtkDoubleArray.h"
#include "vtkDynamicLoader.h"
#include "vtkEdgeTable.h"
#include "vtkEmptyCell.h"
#include "vtkExtentTranslator.h"
#include "vtkFieldData.h"
#include "vtkFileOutputWindow.h"
#include "vtkFloatArray.h"
#include "vtkFunctionSet.h"
#include "vtkGeneralTransform.h"
#include "vtkGenericCell.h"
#include "vtkHexahedron.h"
#include "vtkHomogeneousTransform.h"
#include "vtkIdList.h"
#include "vtkIdentityTransform.h"
#include "vtkImageData.h"
//#include "vtkImageSource.h"
#include "vtkImageToStructuredPoints.h"
#include "vtkImplicitFunction.h"
#include "vtkImplicitFunctionCollection.h"
#include "vtkIndent.h"
#include "vtkInitialValueProblemSolver.h"
#include "vtkIntArray.h"
#include "vtkInterpolatedVelocityField.h"
#include "vtkLine.h"
#include "vtkLinearTransform.h"
#include "vtkLocator.h"
#include "vtkLogLookupTable.h"
#include "vtkLongArray.h"
#include "vtkLookupTable.h"
#include "vtkMapper2D.h"
//#include "vtkMarchingCubesCases.h"
//#include "vtkMarchingSquaresCases.h"
#include "vtkMath.h"
#include "vtkMatrix4x4.h"
#include "vtkMatrixToHomogeneousTransform.h"
#include "vtkMatrixToLinearTransform.h"
#include "vtkMergePoints.h"
//#include "vtkMultiThreader.h"
#include "vtkMutexLock.h"
#include "vtkObject.h"
#include "vtkObjectFactory.h"
#include "vtkObjectFactoryCollection.h"
#include "vtkOutputWindow.h"
#include "vtkPerspectiveTransform.h"
#include "vtkPixel.h"
#include "vtkPlane.h"
#include "vtkPlaneCollection.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkPointSet.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
//#include "vtkPolyDataSource.h"
#include "vtkPolyLine.h"
#include "vtkPolyVertex.h"
#include "vtkPolygon.h"
#include "vtkPriorityQueue.h"
//#include "vtkProcessObject.h"
#include "vtkProp.h"
#include "vtkPropAssembly.h"
#include "vtkPropCollection.h"
#include "vtkProperty2D.h"
#include "vtkPyramid.h"
#include "vtkQuad.h"
#include "vtkQuadric.h"
#include "vtkRayCastStructures.h"
#include "vtkRectilinearGrid.h"
#include "vtkReferenceCount.h"
#include "vtkRungeKutta2.h"
#include "vtkRungeKutta4.h"
#include "vtkScalarsToColors.h"
#include "vtkSetGet.h"
#include "vtkShortArray.h"
//#include "vtkSource.h"
#include "vtkStructuredData.h"
#include "vtkStructuredGrid.h"
#include "vtkStructuredPoints.h"
#include "vtkSystemIncludes.h"
#include "vtkTetra.h"
#include "vtkTimeStamp.h"
#include "vtkTimerLog.h"
#include "vtkTransform.h"
#include "vtkTransformCollection.h"
#include "vtkTriangle.h"
#include "vtkTriangleStrip.h"
#include "vtkUnsignedCharArray.h"
#include "vtkUnsignedIntArray.h"
#include "vtkUnsignedLongArray.h"
#include "vtkUnsignedShortArray.h"
#include "vtkUnstructuredGrid.h"
#include "vtkVersion.h"
#include "vtkVertex.h"
#include "vtkViewport.h"
#include "vtkVoidArray.h"
#include "vtkVoxel.h"
#include "vtkWarpTransform.h"
#include "vtkWedge.h"
#include "vtkWindow.h"
#include "vtkWindowLevelLookupTable.h"
#include "vtkWindowToImageFilter.h"

// from graphics
#include "vtkCellLocator.h"
#include "vtkCleanPolyData.h"
#include "vtkTriangleFilter.h"
#include "vtkSpline.h"
#include "vtkCardinalSpline.h"
#include "vtkKochanekSpline.h"
#include "vtkPolyDataNormals.h"
#include "vtkContourFilter.h"
#include "vtkExtractEdges.h"
#include "vtkPolyDataWriter.h"
#include "vtkPolyDataReader.h"
#include "vtkStructuredPointsWriter.h"
#include "vtkUnstructuredGridWriter.h"

#include "vtkFeatureEdges.h"
#include "vtkMath.h"
#include "vtkDelaunay3D.h"

#ifndef vtkFloatingPointArrayType
  #define vtkFloatingPointArrayType vtkFloatingPointArrayType
  /*typedef vtkFloatArray vtkFloatingPointArrayType;*/
  typedef vtkDoubleArray vtkFloatingPointArrayType;
#endif

#ifndef vtkFloatingPointType
  #define vtkFloatingPointType double
#endif

#endif




