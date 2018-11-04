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
 *  \class  vtkSVIOUtils
 *  \brief This is class of useful functions for reading and writing vtk files.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVIOUtils_h
#define vtkSVIOUtils_h

#include "vtkObject.h"
#include "vtkSVIOModule.h" // For export

#include "vtkImageData.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkStructuredGrid.h"
#include "vtkUnstructuredGrid.h"

#include <string>
#include <sstream>
#include <iostream>

class VTKSVIO_EXPORT vtkSVIOUtils : public vtkObject
{
public:
  vtkTypeMacro(vtkSVIOUtils,vtkObject);

  /// \brief Check directory exists
  static int CheckDirectoryExists(std::string dirname);

  /// \brief Check file exists
  static int CheckFileExists(std::string filename);

  // String processing functions
  static std::string IntToString(int i); /**< \brief Converts integer to string. */
  static std::string GetPath(std::string fullName); /**< \brief Gets filename path. */
  static std::string GetRawName(std::string fullName); /**< \brief Gets filename raw name without extension. */
  static std::string GetExt(std::string fullName); /**< \brief Gets filename extension (e.g. vtp). */

  //Read Write functions for stl, vtp
  /** \brief read an stl file. */
  static int ReadSTLFile(std::string inputFilename, vtkPolyData *polydata);

  /** \brief read a polydata file. */
  static int ReadVTPFile(std::string inputFilename, vtkPolyData *polydata);

  /** \brief read a polydata file. */
  static int ReadVTUFile(std::string inputFilename, vtkUnstructuredGrid *grid);

  /** \brief read a raw file. */
  static int ReadPolyDataRawFile(std::string inputFilename, vtkPolyData *polydata);

  /** \brief read a raw file. */
  static int ReadUnstructuredGridRawFile(std::string inputFilename, vtkUnstructuredGrid *unstructuredgrid);
  /** \brief read an stl or polydata file. */
  static int ReadInputFile(std::string inputFilename, vtkPolyData *polydata);

  //@{
  /** \brief write a vtp file. */
  static int WriteVTPFile(std::string outputFilename, vtkPolyData *writePolyData);
  static int WriteVTPFile(std::string inputFilename, vtkPolyData *writePolyData,std::string attachName);
  //@}

  //@{
  /** \brief write a vtu file. */
  static int WriteVTUFile(std::string outputFilename, vtkUnstructuredGrid *writeUnstructuredGrid);
  static int WriteVTUFile(std::string inputFilename, vtkUnstructuredGrid *writeUnstructuredGrid,std::string attachName);
  //@}

  //@{
  /** \brief write a vts file. */
  static int WriteVTSFile(std::string outputFilename, vtkStructuredGrid *writeStructuredGrid);
  static int WriteVTSFile(std::string inputFilename,vtkStructuredGrid *writeStructuredGrid,std::string attachName);
  //@}

  //@{
  /** \brief write a vts file. */
  static int WriteMHDFile(std::string outputFilename, vtkImageData *image);
  //@}
  //

  //@{
  /** \brief write a raw file. */
  static int WriteRawFile(std::string outputFilename, vtkPolyData *writePolyData);
  static int WriteRawFile(std::string inputFilename,vtkPolyData *writePolyData,std::string attachName);
  //@}

protected:
  vtkSVIOUtils();
  ~vtkSVIOUtils();

private:
  vtkSVIOUtils(const vtkSVIOUtils&);  // Not implemented.
  void operator=(const vtkSVIOUtils&);  // Not implemented.
};

#endif
