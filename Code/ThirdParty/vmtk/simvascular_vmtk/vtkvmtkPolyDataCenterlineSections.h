/*=========================================================================

Program:   VMTK
Module:    $RCSfile: vtkvmtkPolyDataCenterlineSections.h,v $
Language:  C++
Date:      $Date: 2006/10/17 15:16:16 $
Version:   $Revision: 1.1 $

  Copyright (c) Luca Antiga, David Steinman. All rights reserved.
  See LICENSE file for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm 
  for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// .NAME vtkvmtkPolyDataCenterlineSections - Indicate centerline branches and bifurcations and calculate cross-sectional area.
// .SECTION Description
// ...

#define SV_OK                 1
#define SV_ERROR              0

#ifndef __vtkvmtkPolyDataCenterlineSections_h
#define __vtkvmtkPolyDataCenterlineSections_h

#include "vtkPolyDataAlgorithm.h"
//#include "vtkvmtkComputationalGeometryWin32Header.h"
#include "vtkvmtkWin32Header.h"
#include "vtkPolyData.h"

class VTK_VMTK_COMPUTATIONAL_GEOMETRY_EXPORT vtkvmtkPolyDataCenterlineSections : public vtkPolyDataAlgorithm
{
  public: 

    //-----------
    // DataArray
    //-----------
    // This class provides an interface to store and access VTK data arrays.
    //
    template<class T>
    class DataArray {
      public:
        DataArray() {
          data = vtkSmartPointer<T>::New();
        }
        std::string name;
        vtkSmartPointer<T> data;
        void initialize(const int size, const double value) {
          data->SetNumberOfTuples(size);
          data->Fill(value);
        }
        void set_name(const std::string& array_name) {
          name = array_name;
          data->SetName(name.c_str());
        }
        const char* get_name() {
          return name.c_str();
        }
        void set_component_size(const int size) {
          data->SetNumberOfComponents(size);
        }
    };

    // Classification of points within a centerline. 
    //
    // Declare using enum to compare with int values from 
    // centerline data arrays.
    //
    enum PointType {
      bifurcation = 1,
      bifurcation_downstream = 2,
      branch = 0,
      undefined = -1
    };
    PointType point_types[2] = { PointType::branch, PointType::bifurcation };

    // Set some of the arrays names used internally.
    class ArrayName {
      public:
        const char* BifurcationId = "BifurcationId";
        const char* BifurcationIdTmp = "BifurcationIdTmp";
        const char* BranchId = "BranchId";
        const char* BranchIdTmp = "BranchIdTmp";
        const char* CenterlineId = "CenterlineId";
        const char* GlobalNodeId = "GlobalNodeId";
        const char* Path = "Path";
        const char* Radius = "MaximumInscribedSphereRadius";
    };

    vtkTypeMacro(vtkvmtkPolyDataCenterlineSections,vtkPolyDataAlgorithm);
    void PrintSelf(ostream& os, vtkIndent indent) override; 

    static vtkvmtkPolyDataCenterlineSections* New();
    vtkSetObjectMacro(Centerlines,vtkPolyData);
    vtkGetObjectMacro(Centerlines,vtkPolyData);
    vtkGetObjectMacro(Surface,vtkPolyData);

  protected:
    vtkvmtkPolyDataCenterlineSections();
    ~vtkvmtkPolyDataCenterlineSections();  

    virtual int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *) override;

    int ComputeCenterlineSections(vtkPolyData* output);
    int CleanBifurcation();
    int ConnectivityCenterline(vtkPolyData* geo, const char* nameThis, const char* nameOther);
    int GroupCenterline();
    int SplitCenterline(vtkPolyData* branches, vtkPolyData* bifurcations);
    void AddBifurcationCellArray();
    int GenerateCleanCenterline();
    int BranchSurface(const char* nameBranch, const char* nameBifurcation);
    int CalculateTangent();
    int RefineCapPoints();
    bool IsOnePiece(vtkPolyData* inp, const std::string& name="");

    // Originally the VMTK centerlines geometry but is later modified.
    vtkPolyData* Centerlines;

    // The vtkPolyData surface from which the VMTK centerlines was computed.
    vtkPolyData* Surface;

    // The number of VMTK continuous line geometry objects representing each 
    // centerline (branch) in the original VMTK centerlines geometry.
    int n_centerlines;

  private:
    vtkvmtkPolyDataCenterlineSections(const vtkvmtkPolyDataCenterlineSections&);  // Not implemented.
    void operator=(const vtkvmtkPolyDataCenterlineSections&);  // Not implemented.

    void initialize_data_arrays();

    ArrayName array_name;

    // Objects used to store VTK data arrays.
    DataArray<vtkDoubleArray> section_area;
    DataArray<vtkIntArray>    section_bifurcation;
    DataArray<vtkIntArray>    section_closed;
    DataArray<vtkIntArray>    section_global_node_ids;
    DataArray<vtkDoubleArray> section_max_size;
    DataArray<vtkDoubleArray> section_min_size;
    DataArray<vtkDoubleArray> section_normal;
    DataArray<vtkDoubleArray> section_shape;

    DataArray<vtkDoubleArray> centerline_area;
    DataArray<vtkIntArray>    centerline_bifurcation;
    DataArray<vtkIntArray>    centerline_closed;
    DataArray<vtkDoubleArray> centerline_normal;
    DataArray<vtkDoubleArray> centerline_max_size;
    DataArray<vtkDoubleArray> centerline_min_size;
    DataArray<vtkDoubleArray> centerline_shape;
};

#endif
