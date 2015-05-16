/* Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
#include "vtkMarchingCubesCases.h"
#include "vtkMarchingSquaresCases.h"
#include "vtkMath.h"
#include "vtkMatrix4x4.h"
#include "vtkMatrixToHomogeneousTransform.h"
#include "vtkMatrixToLinearTransform.h"
#include "vtkMergePoints.h"
#include "vtkMultiThreader.h"
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
#include "vtkTensor.h"
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

#endif




