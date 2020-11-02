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

// The functions defined here implement the SV Python API data manager 'dmg' module.
// The module is used to access the SV Data Manager data nodes (e.g. Paths, Segmentations, etc.)
// from a Python script executed in the SV Python console. A project must be opened in order
// it use the 'dmg' module.
//
// A Python exception sv.dmg.DmgError is defined for this module.
// The exception can be used in a Python 'try' statement with an 'except' clause
// like this
//
//    except sv.dmg.DmgError:
//
// Some functions defined here are also visible outside of this module. They are used
// by Python API functions to read files (e.g. .ctgr) created by SV.
//
#include "SimVascular.h"
#include "sv2_globals.h"
#include "SimVascular_python.h"

#include "Dmg_PyModule.h"
#include "Segmentation_PyModule.h"
#include "Meshing_PyModule.h"
#include "PathPlanning_PyModule.h"
#include "Modeling_PyModule.h"

//#include "sv_Repository.h"
#include "sv_PolyData.h"
#include "sv_PolyDataSolid.h"
#include "sv_StrPts.h"
#include "sv_UnstructuredGrid.h"
#include "sv_arg.h"
#include "sv_VTK.h"
#include "vtkTclUtil.h"
#include "vtkPythonUtil.h"
#include "PyUtils.h"
#include "sv4gui_ContourGroupIO.h"
#include "sv4gui_ModelIO.h"

#include <vtkXMLUnstructuredGridWriter.h>

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

#include "Python.h"
#include "sv4gui_ModelElement.h"
#include "sv4gui_ModelElementAnalytic.h"
#include "sv4gui_ModelElementFactory.h"
#include "sv4gui_Model.h"
#include "sv4gui_Mesh.h"
#include "sv4gui_Path.h"
#include "sv3_PathElement.h"
#include "sv3_Contour.h"
#include "sv4gui_Contour.h"
#include "sv4gui_PathElement.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_MeshFactory.h"
#include "sv4gui_PythonDataNodesPluginActivator.h"
#include "sv4gui_ProjectManager.h"
#include "sv4gui_DataNodeOperationInterface.h"
#include "sv4gui_DataNodeOperation.h"
#include "vtkPythonUtil.h"
#include "vtkDataSetSurfaceFilter.h"
#include "sv4gui_ApplicationPluginActivator.h"

#include <mitkNodePredicateDataType.h>
#include <mitkIDataStorageService.h>
#include <mitkDataNode.h>
#include <mitkBaseRenderer.h>
#include <mitkDataStorage.h>
#include <mitkUndoController.h>
#include <mitkIOUtil.h>

#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <array>
#include <vector>
#include <string>

// Exception type used by PyErr_SetString() to set the for the error indicator.
PyObject* PyRunTimeErr;

// Define the SV Data Manager top level folder names.
//
// [TODO:DaveP] These should be globally defined somewhere.
//
namespace SvDataManagerNodes {
  static char* Image = "svImageFolder";
  static char* Mesh = "sv4guiMeshFolder";
  static char* Model = "sv4guiModelFolder";
  static char* Path = "sv4guiPathFolder";
  static char* Project = "sv4guiProjectFolder";
  static char* Segmentation = "sv4guiSegmentationFolder";

  // Map between plugin names used by the API and SV internal folder names.
  std::map<std::string, char*> pluginNameMap = {
      { "Image", Image },
      { "Mesh", Mesh },
      { "Model", Model },
      { "Path", Path },
      { "Segmentation", Segmentation }
  };

  std::string ValidPluginNames = "Valid names: Image, Mesh, Model, Path, or Segmentation.";
};

namespace SvDataManagerErrorMsg {
  static char* DataStorage = "Data Storage is not defined.";
  static char* ProjectFolder = "Project folder is not defined.";
};

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////

//----------------
// BuildModelNode
//----------------
// Create Model node from a vtk polydata object.
//
sv4guiModel::Pointer
BuildModelNode(vtkPolyData* polydata, sv4guiModel::Pointer model)
{
  // Hardcodeed type to be PolyData.
  //
  auto modelElement = sv4guiModelElementFactory::CreateModelElement("PolyData");
  modelElement->SetWholeVtkPolyData(polydata);
  auto analytic = dynamic_cast<sv4guiModelElementAnalytic*>(modelElement);

  if (analytic) {
      analytic->SetWholeVtkPolyData(analytic->CreateWholeVtkPolyData());
  }

  model->SetType(modelElement->GetType());
  model->SetModelElement(modelElement);
  model->SetDataModified();
  return model;
}

//---------------
// BuildMeshNode
//---------------
// Create a TetGen Mesh node from a vtk unstructured mesh object.
//
// The modelName parameter is the name of a solid model under the SV Data Manager Models node.
//
sv4guiMitkMesh::Pointer
BuildMeshNode(vtkUnstructuredGrid* ugrid, sv4guiMitkMesh::Pointer mitkMesh, const char* modelName)
{
  // Get surface polydata from the unstructured grid.
  //
  vtkSmartPointer<vtkDataSetSurfaceFilter> surfaceFilter = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
  surfaceFilter->SetInputData(ugrid);
  surfaceFilter->Update();
  vtkSmartPointer<vtkPolyData> polydata = surfaceFilter->GetOutput();

  // Set the surface and volume mesh to the sv4guiMesh.
  //
  auto mesh = sv4guiMeshFactory::CreateMesh("TetGen");
  mesh->SetVolumeMesh(ugrid);
  mesh->SetSurfaceMesh(polydata);

  // Set mitk mesh.
  mitkMesh->SetMesh(mesh);
  mitkMesh->SetType("TetGen");
  mitkMesh->SetModelName(modelName);
  mitkMesh->SetDataModified();

  return mitkMesh;
}

//---------------
// BuildPathNode
//---------------
// Create a Path node from a PathElement object.
//
sv4guiPath::Pointer BuildPathNode(cvRepositoryData *obj, sv4guiPath::Pointer path)
{
  sv3::PathElement* pathElem = dynamic_cast<sv3::PathElement*> (obj);
  sv4guiPathElement* guiPath = new sv4guiPathElement();

  switch(pathElem->GetMethod()) {
      case sv3::PathElement::CONSTANT_TOTAL_NUMBER:
          guiPath->SetMethod(sv4guiPathElement::CONSTANT_TOTAL_NUMBER);
      break;

      case sv3::PathElement::CONSTANT_SUBDIVISION_NUMBER:
          guiPath->SetMethod(sv4guiPathElement::CONSTANT_SUBDIVISION_NUMBER);
      break;

      case sv3::PathElement::CONSTANT_SPACING:
          guiPath->SetMethod(sv4guiPathElement::CONSTANT_SPACING);
      break;

      default:
      break;
  }

  guiPath->SetCalculationNumber(pathElem->GetCalculationNumber());
  guiPath->SetSpacing(pathElem->GetSpacing());

  // Copy control points.
  std::vector<std::array<double,3> > pts = pathElem->GetControlPoints();
  for (int i=0; i<pts.size();i++)
  {
      mitk::Point3D point;
      point[0] = pts[i][0];
      point[1] = pts[i][1];
      point[2] = pts[i][2];
      guiPath->InsertControlPoint(i,point);
  }

  //create path points
  guiPath->CreatePathPoints();

  path->SetPathElement(guiPath);
  path->SetDataModified();

  return path;
}

//--------------------
// CreateSegmentationGroup
//--------------------
// Create a segmentation contour group from a list of contours.
//
sv4guiContourGroup::Pointer
CreateSegmentationGroup(std::vector<PySegmentation*> contours, sv4guiContourGroup::Pointer group, char* pathName)
{
  std::string str(pathName);
  group->SetPathName(str);

  for (int j = 0; j < contours.size(); j++) {
      sv3::Contour* sv3Contour = contours[j]->contour;
      sv4guiContour* contour = new sv4guiContour();
      sv3::PathElement::PathPoint pathPoint = sv3Contour->GetPathPoint();
      sv4guiPathElement::sv4guiPathPoint pthPt;

      for (int i = 0; i < 3; i++) {
          pthPt.pos[i] = pathPoint.pos[i];
          pthPt.tangent[i] = pathPoint.tangent[i];
          pthPt.rotation[i] = pathPoint.rotation[i];
      }

      pthPt.id = pathPoint.id;
      contour->SetPathPoint(pthPt);
      contour->SetMethod(sv3Contour->GetMethod());
      contour->SetPlaced(true);
      contour->SetClosed(sv3Contour->IsClosed());
      contour->SetContourPoints(sv3Contour->GetContourPoints());

      group->InsertContour(j, contour);
      group->SetDataModified();
  }

  return group;
}

//-------------
// AddDataNode
//-------------
// Add new node to its parent node.
//
// [TODO:DaveP] what in the world is this?
void
AddDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer folderNode, mitk::DataNode::Pointer dataNode)
{
  mitk::OperationEvent::IncCurrObjectEventId();
  auto interface = new sv4guiDataNodeOperationInterface;
  bool undoEnabled = true;
  auto doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE, dataStorage, dataNode, folderNode);

  if (undoEnabled) {
      auto undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE, dataStorage, dataNode,
          folderNode);
      auto operationEvent = new mitk::OperationEvent(interface, doOp, undoOp, "Add DataNode");
      mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent(operationEvent);
  }

  interface->ExecuteOperation(doOp);
}

//----------------
// RemoveDataNode
//----------------
//
int
RemoveDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer folderNode, char* childName)
{
    mitk::DataNode::Pointer childNode =dataStorage->GetNamedDerivedNode(childName,folderNode);

    if (folderNode && childNode) {
        dataStorage->Remove(childNode);
    } else {
        return SV_ERROR;
    }

    mitk::OperationEvent::IncCurrObjectEventId();
    sv4guiDataNodeOperationInterface* interface=new sv4guiDataNodeOperationInterface;
    bool undoEnabled=true;
    sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,dataStorage,childNode,folderNode);

    if(undoEnabled) {
        sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,dataStorage,childNode,folderNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(interface, doOp, undoOp, "Remove DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    interface->ExecuteOperation(doOp);
    return SV_OK;
}

//-------------------------
// AddSegmentationDataNode
//-------------------------
// Add a Segmentaion data node to the SV Data Manager.
int
AddSegmentationDataNode(mitk::DataStorage::Pointer dataStorage, std::vector<PySegmentation*> segmentations,
        mitk::DataNode::Pointer folderNode, char* childName, char* pathName, sv4guiPath::Pointer path)
{
  mitk::DataNode::Pointer node = mitk::DataNode::New();
  sv4guiContourGroup::Pointer contourGroup = sv4guiContourGroup::New();
  contourGroup = CreateSegmentationGroup(segmentations, contourGroup, pathName);

  if (!path.IsNull()) {
      contourGroup->SetPathID(path->GetPathID());
  }
  node->SetData(contourGroup);
  node->SetName(childName);

  // Add new node to its parent node.
  mitk::OperationEvent::IncCurrObjectEventId();
  sv4guiDataNodeOperationInterface* interface=new sv4guiDataNodeOperationInterface;
  bool undoEnabled = true;
  sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,dataStorage,node,folderNode);

  if (undoEnabled) {
      sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,dataStorage,node,folderNode);
      mitk::OperationEvent *operationEvent = new mitk::OperationEvent(interface, doOp, undoOp, "Add DataNode");
      mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
  }
  interface->ExecuteOperation(doOp);

  return SV_OK;
}

//-------------
// GetToolNode
//-------------
// Get the tool data node from the SV Data Manager.
//
// The tool nodes are under the root project node and defined for
// SV tools: Images, Paths, Segmentations, Models and Meshes.
//
mitk::DataNode::Pointer
GetToolNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projectNode, char* toolName)
{
  //std::cout << "========== GetToolNode ==========" << std::endl;
  //std::cout << "toolName: " << toolName << std::endl;
  mitk::NodePredicateDataType::Pointer isTool = mitk::NodePredicateDataType::New(toolName);
  mitk::DataStorage::SetOfObjects::ConstPointer rs = dataStorage->GetDerivations(projectNode, isTool);
  mitk::DataNode::Pointer toolNode;
  //std::cout << "rs->size(): " << rs->size() << std::endl;

  if (rs->size() > 0) {
      toolNode = rs->GetElement(0);

      if (toolNode.IsNull()) {
          //std::cout << "toolNode is null. " << std::endl;
          //MITK_ERROR << "Error getting a pointer to the folderNode.";
          return nullptr;
      }
  } else {
      // MITK_ERROR <<"Error getting a pointer to the folderNode.";
      return nullptr;
  }
  return toolNode;
}

//----------------
// GetProjectNode
//----------------
// Get the SV Data Manger root project node 'sv4guiProjectFolder'.
//
mitk::DataNode::Pointer
GetProjectNode(PyUtilApiFunction& api, mitk::DataStorage::Pointer dataStorage)
{
  mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New(SvDataManagerNodes::Project);
  mitk::DataStorage::SetOfObjects::ConstPointer rs = dataStorage->GetSubset(isProjFolder);
  mitk::DataNode::Pointer projFolderNode;

  if (rs->size()>0) {
      projFolderNode=rs->GetElement(0);
  } else {
      api.error("Could not find a project folder. A project must be active.");
      return nullptr;
  }
  return projFolderNode;
}

//----------------
// GetDataStorage
//----------------
// Get the data storage context from the plugin.
//
static mitk::DataStorage::Pointer
GetDataStorage(PyUtilApiFunction& api)
{
  mitk::DataStorage::Pointer dataStorage;
  ctkServiceReference dsServiceRef;
  ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();

  if (context) {
      dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
  } else {
      api.error("Could not get the active data storgage. A project must be active.");
      return dataStorage;
  }

  mitk::IDataStorageService* dss = nullptr;
  if (dsServiceRef) {
      dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
  }

  if (!dss) {
      api.error("Could not get the active data storgage. A project must be active.");
      return dataStorage;
  }

  // Get the active data storage (or the default one, if none is active)
  mitk::IDataStorageReference::Pointer dsRef;
  dsRef = dss->GetDataStorage();
  context->ungetService(dsServiceRef);

  return dsRef->GetDataStorage();
}

//-------------
// GetDataNode
//-------------
// Get the SV Data Manager node under the given top level data node.
//
// These data nodes contain data for images, paths, contours, models and meshes.
//
mitk::DataNode::Pointer
GetDataNode(mitk::DataStorage::Pointer& dataStorage, mitk::DataNode::Pointer& projFolderNode, char *nodeName, char *childName)
{
  mitk::DataNode::Pointer dataNode;
  mitk::DataNode::Pointer toolNode;
  toolNode = GetToolNode(dataStorage, projFolderNode, nodeName);

  if (toolNode.IsNull()) {
      return dataNode;
  }

  dataNode = dataStorage->GetNamedDerivedNode(childName, toolNode);
  return dataNode;
}

//////////////////////////////////////////////////////
//          M o d u l e  F u n c t i o n s          //
//////////////////////////////////////////////////////
//
// Python API functions.

//--------------
// Dmg_add_mesh
//--------------
//
PyDoc_STRVAR(Dmg_add_mesh_doc,
  "add_mesh(name, mesh, model)  \n\
   \n\
   Add a mesh to the SV Data Manager Meshes node. \n\
   \n\
   Args: \n\
     name (str): The name of the mesh data node.                           \n\
     mesh (vtkUnstructuredGrid object): The vtkUnstructuredGrid object.    \n\
     model (str): The name of the SV Data Manager Models node associated   \n\
        with the mesh.                                                     \n\
");

static PyObject *
Dmg_add_mesh(PyObject* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("sOs", PyRunTimeErr, __func__);
  static char *keywords[] = {"name", "mesh", "model", NULL};
  char* meshName;
  PyObject* ugridArg;
  char* modelName;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &meshName, &ugridArg, &modelName)) {
      return api.argsError();
  }

  // Get the pointer to the vtkUnstructuredGrid object.
  vtkSmartPointer<vtkUnstructuredGrid> ugrid = (vtkUnstructuredGrid*)vtkPythonUtil::GetPointerFromObject(ugridArg, "vtkUnstructuredGrid");
  if (PyErr_Occurred()) {
      api.error("The 'mesh' argument is not a vtkUnstructuredGrid object.");
      return nullptr;
  }

  // Get the Data Storage node.
  auto dataStorage = GetDataStorage(api);
  if (dataStorage.IsNull()) {
      return nullptr;
  }

  // Get project folder.
  auto projFolderNode = GetProjectNode(api, dataStorage);
  if (projFolderNode.IsNull()) {
      return nullptr;
  }

  // Check that the model exists.
  auto modelNode = GetDataNode(dataStorage, projFolderNode, SvDataManagerNodes::Model, modelName);
  if (modelNode.IsNull()) {
      auto nodeName = SvDataManagerNodes::Model;
      api.error("The Model node '" + std::string(modelName) + "' was not found under '" + nodeName + "'.");
      return nullptr;
  }

  // Create a new Mesh node.
  mitk::DataNode::Pointer meshNode = mitk::DataNode::New();
  sv4guiMitkMesh::Pointer mitkMesh = sv4guiMitkMesh::New();
  mitkMesh = BuildMeshNode(ugrid, mitkMesh, modelName);
  meshNode->SetData(mitkMesh);
  meshNode->SetName(meshName);

  // Add the node under the SV Data Manager Mesh folder node.
  auto folderNode = GetToolNode(dataStorage, projFolderNode, SvDataManagerNodes::Mesh);
  AddDataNode(dataStorage, folderNode, meshNode);

  return SV_PYTHON_OK;
}

//---------------
// Dmg_get_model
//---------------
//
PyDoc_STRVAR(Dmg_get_model_doc,
  "get_model(name) \n\
   \n\
   Get a model from the SV Data Manager Models node. \n\
   \n\
   The model returned is an sv.modeling modeler object of the same type used  \n\
   used to create the model in SimVascular. For example, getting a PolyData   \n\
   model returns an sv.modeling.PolyData object.                              \n\
   \n\
   Args: \n\
     name (str): The model node name. \n\
   \n\
   Returns a model object.                                                    \n\
");

static PyObject *
Dmg_get_model(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char* modelName;

  if (!PyArg_ParseTuple(args, api.format, &modelName)) {
      return api.argsError();
  }

  // Get the Data Storage node.
  auto dataStorage = GetDataStorage(api);
  if (dataStorage.IsNull()) {
      return nullptr;
  }

  // Get project folder.
  auto projFolderNode = GetProjectNode(api, dataStorage);
  if (projFolderNode.IsNull()) {
      return nullptr;
  }

  // Get the SV Data Manager Model node or svRepositoryFolder node.
  auto modelNode = GetDataNode(dataStorage, projFolderNode, SvDataManagerNodes::Model, modelName);
  if (modelNode.IsNull()) {
      auto nodeName = SvDataManagerNodes::Model;
      api.error("The Model node '" + std::string(modelName) + "' was not found under '" + nodeName + "'.");
      return nullptr;
  }

  // Get the solid model group.
  auto solidGroup = dynamic_cast<sv4guiModel*>(modelNode->GetData());

  // Get the solid model for time=0.
  //
  int time = 0;
  auto solidModelElement = solidGroup->GetModelElement(time);
  if (solidModelElement == nullptr) {
      api.error("ERROR getting the solid model for the time '" + std::to_string(time) + "'.");
      return nullptr;
  }

  // No inner solid is created for models read from .vtp or .stl files
  // so create a PolyData solid model and set its polydata.
  //
  auto solidModel = solidModelElement->GetInnerSolid();
  if (solidModel == nullptr) {
      auto polydata = solidModelElement->GetWholeVtkPolyData();
      solidModel = new cvPolyDataSolid();
      solidModel->SetVtkPolyDataObject(polydata);
  }

  // Create a PySolidModel object from the SV cvSolidModel
  // object and return it as a PyObject.
  return CreatePyModelingModelObject(solidModel);
}

//--------------
// Dmg_get_mesh
//--------------
//
// [TODO:DaveP] Should this return a sv.meshing.Series?
//
PyDoc_STRVAR(Dmg_get_mesh_doc,
  "get_mesh(name) \n\
   \n\
   Get a mesh from the SV Data Manager Meshes node. \n\
   \n\
   Args: \n\
     name (str): The mesh node name. \n\
   \n\
   Returns a vtkUnstructuredGrid object.  \n\
");

static PyObject *
Dmg_get_mesh(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char* meshName;

  if (!PyArg_ParseTuple(args, api.format, &meshName)) {
      return api.argsError();
  }

  // Get the Data Storage node.
  auto dataStorage = GetDataStorage(api);
  if (dataStorage.IsNull()) {
      return nullptr;
  }

  // Get project folder.
  auto projFolderNode = GetProjectNode(api, dataStorage);
  if (projFolderNode.IsNull()) {
      return nullptr;
  }

  // Get the SV Data Manager Mesh node or svRepositoryFolder node.
  auto node = GetDataNode(dataStorage, projFolderNode, SvDataManagerNodes::Mesh, meshName);
  if (node.IsNull()) {
      auto nodeName = SvDataManagerNodes::Mesh;
      api.error("The Mesh node '" + std::string(meshName) + "' was not found under '" + nodeName + "'.");
      return nullptr;
  }

  auto mitkMesh = dynamic_cast<sv4guiMitkMesh*>(node->GetData());
  if (!mitkMesh) {
      api.error("Unable to get Mesh unstructured grid for '" + std::string(meshName) + "' from the SV Data Manager.");
      return nullptr;
   }
  auto mesh = mitkMesh->GetMesh();
  auto ugrid = mesh->GetVolumeMesh();

  if (ugrid == NULL) {
      api.error("Unable to get Mesh unstructured grid for '" + std::string(meshName) + "' from the SV Data Manager.");
      return nullptr;
  }

  auto ugridCopy = vtkUnstructuredGrid::New();
  ugridCopy->DeepCopy(ugrid);

  return vtkPythonUtil::GetObjectFromPointer(ugrid);
}

//--------------
// Dmg_get_path
//--------------
//
// [TODO:DaveP] Should this return a sv.pathplanning.Series?
//
PyDoc_STRVAR(Dmg_get_path_doc,
  "get_path(name) \n\
   \n\
   Get a path from the SV Data Manager Paths node. \n\
   \n\
   Args: \n\
     name (str): The name of the path data node. \n\
   \n\
   Returns an sv.path.Path object.  \n\
");

static PyObject *
Dmg_get_path(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char* pathName = NULL;

  if (!PyArg_ParseTuple(args, api.format, &pathName)) {
      return api.argsError();
  }
  std::cout << "[Dmg_get_path] Path name: " << pathName << std::endl;

  // Get the Data Storage node.
  auto dataStorage = GetDataStorage(api);
  if (dataStorage.IsNull()) {
      api.error("Data Storage is not defined.");
      return nullptr;
  }

  // Get project folder.
  auto projFolderNode = GetProjectNode(api, dataStorage);
  if (projFolderNode.IsNull()) {
      api.error("Project folder is not defined.");
      return nullptr;
  }

  // Get the SV Data Manager Path node or svRepositoryFolder node.
  auto node = GetDataNode(dataStorage, projFolderNode, SvDataManagerNodes::Path, pathName);
  if (node.IsNull()) {
      auto nodeName = SvDataManagerNodes::Path;
      api.error("The Path node '" + std::string(pathName) + "' was not found under '" + nodeName + "'.");
      return nullptr;
  }

  std::cout << "[Dmg_get_path] Get path from node ... " << std::endl;
  auto path = dynamic_cast<sv4guiPath*>(node->GetData());
  if (path == nullptr) {
      api.error("The Path node '" + std::string(pathName) + "' does not have data.");
      return nullptr;
  }
  std::cout << "[Dmg_get_path] Path: "<< path  << std::endl;

  // Create a copy of the path.
  auto pathElem = path->GetPathElement();
  auto pathElemCopy = new sv4guiPathElement(*pathElem);

  // Create Python Path object.
  return CreatePyPath(pathElemCopy);
}

//--------------
// Dmg_add_path
//--------------
//
PyDoc_STRVAR(Dmg_add_path_doc,
  "add_path(name, path)  \n\
   \n\
   Add a path to the SV Data Manager Paths node. \n\
   \n\
   Args: \n\
     name (str): The name of the path data node.                           \n\
     path (sv.path.Path object): The path object to create the path node from. \n\
");

static PyObject *
Dmg_add_path(PyObject* self, PyObject* args, PyObject* kwargs)
{
    auto api = PyUtilApiFunction("sO!", PyRunTimeErr, __func__);
    static char *keywords[] = {"name", "path", NULL};
    PyObject* pathArg;
    char* pathName = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &pathName, &PyPathType, &pathArg)) {
        return api.argsError();
    }

    auto pyPath = (PyPath*)pathArg;
    auto pathElem = pyPath->path;
    if (pathElem == nullptr) {
        api.error("The path elem data is null.");
        return nullptr;
    }

    // Get the Data Storage node.
    auto dataStorage = GetDataStorage(api);
    if (dataStorage.IsNull()) {
        api.error(SvDataManagerErrorMsg::DataStorage);
        return nullptr;
    }

    // Get project folder.
    auto projFolderNode = GetProjectNode(api, dataStorage);
    if (projFolderNode.IsNull()) {
        return nullptr;
    }

    // Get the SV Data Manager Path data node.
    auto folderNode = GetToolNode(dataStorage, projFolderNode, SvDataManagerNodes::Path);

    // Create new Path.
    sv4guiPath::Pointer path = sv4guiPath::New();
    path = BuildPathNode(pathElem, path);
    int maxPathID = sv4guiPath::GetMaxPathID(dataStorage->GetDerivations(folderNode));
    path->SetPathID(maxPathID+1);

    // Create new Path data node.
    auto pathNode = mitk::DataNode::New();
    pathNode->SetData(path);
    pathNode->SetName(pathName);

    // Add the data node.
    AddDataNode(dataStorage, folderNode, pathNode);

    return SV_PYTHON_OK;
}

//----------------------
// Dmg_add_segmentation
//----------------------
//
PyDoc_STRVAR(Dmg_add_segmentation_doc,
  "add_segmentation(name, path, segmentations) \n\
   \n\
   Add a segmentation to the SV Data Manager Segmentations node. \n\
   \n\
   Args: \n\
     name (str): The name of the segmentations data node to add. \n\
     path (str): The name of the path data node used by the segmentation. \n\
     segmentations (list[Segmentation]): The list of segmentation objects \n\
        defined for a vessel segmentation.                                \n\
");

static PyObject *
Dmg_add_segmentation(PyObject* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("ssO!", PyRunTimeErr, __func__);
  static char *keywords[] = {"name", "path", "segmentations", NULL};
  PyObject* segList;
  char* segName = NULL;
  char* pathName = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &segName, &pathName, &PyList_Type, &segList)) {
      return api.argsError();
  }

  // Get the Data Storage node.
  auto dataStorage = GetDataStorage(api);
  if (dataStorage.IsNull()) {
      api.error(SvDataManagerErrorMsg::DataStorage);
      return nullptr;
  }

  // Get project folder.
  auto projFolderNode = GetProjectNode(api, dataStorage);
  if (projFolderNode.IsNull()) {
      api.error(SvDataManagerErrorMsg::ProjectFolder);
      return nullptr;
  }

  // Get the path node.
  //
  bool useRepository = false;
  auto pathNode = GetDataNode(dataStorage, projFolderNode, SvDataManagerNodes::Path, pathName);
  if (pathNode.IsNull()) {
      auto nodeName = SvDataManagerNodes::Path;
      api.error("The Path node '" + std::string(pathName) + "' was not found under '" + nodeName + "'.");
      return nullptr;
  }

  // Get the Path node associated with the segmentation.
  sv4guiPath::Pointer path = dynamic_cast<sv4guiPath*>(pathNode->GetData());
  if (path.IsNull()) {
      auto nodeName = SvDataManagerNodes::Path;
      api.error("The Path node '" + std::string(pathName) + "' under '" + nodeName + "' has no data.");
      return nullptr;
  }

  // Get the Segmentation node.
  std::cout << "##### GetToolNode" << std::endl;
  auto segNode = GetToolNode(dataStorage, projFolderNode, SvDataManagerNodes::Segmentation);

  // Get a list of segmentation objects.
  std::cout << "##### Get a list of segmentation objects" << std::endl;
  int numSegmentations = PyList_Size(segList);
  std::vector<PySegmentation*> segmentations;
  for (int i = 0; i < numSegmentations; i++) {
      auto seg = (PySegmentation*)PyList_GetItem(segList, i);
      segmentations.push_back(seg);
  }

  // Add the segmentation data node.
  std::cout << "##### AddSegmentationDataNode " << std::endl;
  if (AddSegmentationDataNode(dataStorage, segmentations, segNode, segName, pathName, path) == SV_ERROR) {
      api.error("Error adding the segmentation data node '" + std::string(segName) + "' to the parent node '" +
            segNode->GetName() + "'.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

//------------------
// Dmg_add_geometry
//------------------
//
PyDoc_STRVAR(Dmg_add_geometry_doc,
  "add_geometry(name, geometry, plugin, node) \n\
   \n\
   Add a vtkPolyData object to the SV Data Manager. \n\
   \n\
   The geometry is added to the SV Data Manger under the given plugin and  \n\
   node, and is displayed in the SV graphics window.\n\
   \n\
   Args: \n\
     name (str): The name of the data node. \n\
     geometry (vtkPolyData object): The vtkPolyData to add to the SV Data  \n\
        Manager.                                                           \n\
     plugin (str): The name of the plugin the data node resides under.     \n\
        Valid names: Image, Mesh, Model, Path, or Segmentation.            \n\
     node (str): The date node name to add the geomerty to.                \n\
");

static PyObject *
Dmg_add_geometry(PyObject* self, PyObject* args, PyObject* kwargs)
{
  using namespace SvDataManagerNodes;
  auto api = PyUtilApiFunction("sOss", PyRunTimeErr, __func__);
  static char *keywords[] = {"name", "geometry", "plugin", "node", NULL};
  char* geomName = NULL;
  PyObject* geomObj;
  char* pluginName = NULL;
  char* nodeName = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &geomName, &geomObj, &pluginName, &nodeName)) {
      return api.argsError();
  }

  if (!PyVTKObject_Check(geomObj)) {
      PyErr_SetString(PyExc_ValueError, "The 'geometry' argument is not a vtkPolyData object.");
      return nullptr;
  }

  // Get the vtk polydata from the Python object.
  auto polydata = (vtkPolyData*)vtkPythonUtil::GetPointerFromObject(geomObj, "vtkPolyData");
  if (polydata == nullptr) {
      PyErr_SetString(PyExc_ValueError, "The 'geometry' argument is not a vtkPolyData object.");
      return nullptr;
  }

  if (pluginNameMap.count(std::string(pluginName)) == 0) {
      auto msg = "The 'plugin' argument '" + std::string(pluginName) + "' is not a valid plugin name. " + ValidPluginNames;
      PyErr_SetString(PyExc_ValueError, msg.c_str());
      return nullptr;
  }
  auto svPluginName = pluginNameMap[std::string(pluginName)];

  // Get the Data Storage node.
  auto dataStorage = GetDataStorage(api);
  if (dataStorage.IsNull()) {
      api.error(SvDataManagerErrorMsg::DataStorage);
      return nullptr;
  }

  // Get project folder.
  auto projFolderNode = GetProjectNode(api, dataStorage);
  if (projFolderNode.IsNull()) {
      api.error(SvDataManagerErrorMsg::ProjectFolder);
      return nullptr;
  }

  // Get the SV Data Manager Path data node.
  auto folderNode = GetToolNode(dataStorage, projFolderNode, svPluginName);
  if (folderNode.IsNull()) {
      api.error("No plugin nammed '" + std::string(pluginName) + "'.");
      return nullptr;
  }
  mitk::DataNode::Pointer pathNode = dataStorage->GetNamedDerivedNode(nodeName, folderNode);

  if (pathNode.IsNull()) {
      api.error("No node named '" + std::string(nodeName) + "'.");
      return nullptr;
  }

  mitk::Surface::Pointer surface;
  mitk::DataNode::Pointer geomNode = dataStorage->GetNamedDerivedNode(geomName, pathNode);

  if (geomNode.IsNull()) {
      surface = mitk::Surface::New();
      geomNode = mitk::DataNode::New();
      geomNode->SetData(surface);
      geomNode->SetName(geomName);
      AddDataNode(dataStorage, pathNode, geomNode);
  } else {
      surface = dynamic_cast<mitk::Surface*>(geomNode->GetData());
  }
  surface->SetVtkPolyData(polydata, 0);
  surface->Update();
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();

  return SV_PYTHON_OK;
}

//---------------
// Dmg_add_model
//---------------
//
PyDoc_STRVAR(Dmg_add_model_doc,
  "add_model(name, model) \n\
   \n\
   Add a model to the SV Data Manager Models node.                            \n\
   \n\
   Args: \n\
     name (str): The name for the model data node.                            \n\
     model (sv.modeling.Modeler): The model object representing a solid model.\n\
");

static PyObject *
Dmg_add_model(PyObject* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("sO", PyRunTimeErr, __func__);
  static char *keywords[] = {"name", "model", NULL};
  char* modelName = NULL;
  PyObject* modelArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &modelName, &modelArg)) {
      return api.argsError();
  }

  if (!PyObject_TypeCheck(modelArg, &PyModelingModelType)) {
      api.error("The 'model' argument is not an 'sv.modeling.Model' object.");
      return nullptr;
  }

  // Get the model polydata.
  // 
  // [TODO:DaveP] useMaxDist and maxDist are used as tolerances when discretizing
  // NURBS surfaces. They are not used in cvPolyDataSolid::GetPolyData() 
  // but are in cvOCCTSolidModel::GetPolyData() and cvParasolidSolidModel::GetPolyData() 
  // but with different meanings.
  //
  int useMaxDist = 0; 
  double maxDist = 0.0;
  auto solidModel = (cvSolidModel*)((PyModelingModel*)modelArg)->solidModel;
  auto cvPolyData = solidModel->GetPolyData(useMaxDist, maxDist);
  vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata->DeepCopy(cvPolyData->GetVtkPolyData());

  // Get the Data Storage node.
  auto dataStorage = GetDataStorage(api);
  if (dataStorage.IsNull()) {
      api.error(SvDataManagerErrorMsg::DataStorage);
      return nullptr;
  }

  // Get project folder.
  auto projFolderNode = GetProjectNode(api, dataStorage);
  if (projFolderNode.IsNull()) {
      api.error(SvDataManagerErrorMsg::ProjectFolder);
      return nullptr;
  }

  // Get the model node.
  //
  /* [TODO:DaveP] should we check to overwrite existing node? have a flag to allow this?
  auto modelNode = GetDataNode(dataStorage, projFolderNode, SvDataManagerNodes::Model, modelName);
  if (!modelNode.IsNull()) {
      auto nodeName = SvDataManagerNodes::Model;
      api.error("The Model node '" + std::string(modelName) + "' was not found under '" + nodeName + "'.");
      return nullptr;
  }
  */

  // Get the SV Data Manager Path data node.
  auto folderNode = GetToolNode(dataStorage, projFolderNode, SvDataManagerNodes::Model);

  // Create new model.
  sv4guiModel::Pointer model = sv4guiModel::New();
  model  = BuildModelNode(polydata, model);

  // Create new Model data node.
  auto modelNode = mitk::DataNode::New();
  modelNode->SetData(model);
  modelNode->SetName(modelName);

  // Add the data node.
  AddDataNode(dataStorage, folderNode, modelNode);

  return SV_PYTHON_OK;
}

//----------------------
// Dmg_get_segmentation
//----------------------
//

PyDoc_STRVAR(Dmg_get_segmentations_doc,
  "get_segmentations(name) \n\
   \n\
   Get the segmentations from an SV Data Manager Segmentations node.          \n\
   \n\
   Args: \n\
     name (str): The segmentation node name. \n\
   \n\
   Returns a list of sv.segmentation.Segmentation objects.                    \n\
");

static PyObject * 
Dmg_get_segmentations(PyObject* self, PyObject* args)

{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char* segName = NULL;

  if (!PyArg_ParseTuple(args, api.format, &segName)) {
      return api.argsError();
  }

  // Get the Data Storage node.
  auto dataStorage = GetDataStorage(api);
  if (dataStorage.IsNull()) {
      return nullptr;
  }

  // Get project folder.
  auto projFolderNode = GetProjectNode(api, dataStorage);
  if (projFolderNode.IsNull()) {
      return nullptr;
  }

  // Get the Segmentation data node.
  auto node = GetDataNode(dataStorage, projFolderNode, SvDataManagerNodes::Segmentation, segName);
  if (node.IsNull()) {
      auto nodeName = SvDataManagerNodes::Segmentation;
      api.error("The Segmentation node '" + std::string(segName) + "' was not found under '" + nodeName + "'.");
      return nullptr;
  }

  // Get the contour group from the data node.
  sv4guiContourGroup* group = dynamic_cast<sv4guiContourGroup*>(node->GetData());
  if (group == NULL) {
      api.error("Unable to get the segmentations for the node named '" + std::string(segName) + "' from the SV Data Manager.");
      return nullptr;
  }

  // Create a list of segmentations.
  //
  int time = 0;
  //std::vector<sv4guiContour*> contours = group->GetContourSet(time);
  auto contours = group->GetContourSet(time);
  PyObject* segmentations = PyList_New(contours.size());

  int n = 0;
  for (auto const& contour : contours) {
      auto seg = PyCreateSegmentation(contour);
      PyList_SetItem(segmentations, n, seg);
      n += 1;
  }

  return segmentations; 
}

//----------------------
// Dmg_remove_data_node
//----------------------
// [TODO:DaveP] Not sure to expose this or not.
//
PyDoc_STRVAR(Dmg_remove_data_node_doc,
  "remove_data_node(folder, node) \n\
   \n\
   Remove a node from under an SV Data Manger folder nodes (Paths, Segmentations, Models or Meshes). \n\
   \n\
   Args: \n\
     folder (str): Name of the SV Data Manger folder node. Valid folder: Paths, Segmentations, Models or Meshes. \n\
");

static PyObject *
Dmg_remove_data_node(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss", PyRunTimeErr, __func__);
  char* folderName = NULL;
  char* nodeName = NULL;

  if (!PyArg_ParseTuple(args, api.format, &folderName, &nodeName)) {
      return api.argsError();
  }

  // Get the Data Storage node.
  auto dataStorage = GetDataStorage(api);
  if (dataStorage.IsNull()) {
      return nullptr;
  }

  // Get project folder.
  auto projFolderNode = GetProjectNode(api, dataStorage);
  if (projFolderNode.IsNull()) {
      return nullptr;
  }

  mitk::DataNode::Pointer folderNode = dataStorage->GetNamedDerivedNode(folderName,projFolderNode);
  if(!folderNode) {
      api.error("The data node '" + std::string(folderName) + "' was not found.");
      return nullptr;
  }

  if (RemoveDataNode(dataStorage, folderNode, nodeName) == SV_ERROR) {
      api.error("Error removing the data node '" + std::string(nodeName) + "' under '" + std::string(folderName) + "'.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

////////////////////////////////////////////////////////
//          M o d u l e  D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* DMG_MODULE = "dmg";
static char* DMG_EXCEPTION = "dmg.Error";
static char* DMG_EXCEPTION_OBJECT = "Error";

//---------------
// DmgModule_doc
//---------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(DmgModule_doc,
  "SimVascular dmg module. \n\
   \n\
   The dmg (data manager) module provides an interface for accessing the   \n\
   SV Data Manager, the panel located on the left side of the main window  \n\
   that organizes the data defined for a SimVascular project as a          \n\
   hierarchical tree of data nodes. The Data Manager organizes data nodes  \n\
   into predefined primary data node types according to the tools that     \n\
   create them                                                             \n\
   \n\
       1) Images                                                           \n\
       2) Meshes                                                           \n\
       3) Models                                                           \n\
       4) Paths                                                            \n\
       5) Segmentations                                                    \n\
       6) Simulations                                                      \n\
       7) Simulations1d                                                    \n\
       8) svFSI                                                            \n\
   \n\
   When new data is added to the project a new data node is added under the\n\
   appropriate data node type with a user-defined name.                    \n\
   \n\
   IMPORTANT Scripts accessing a project's data nodes shown in the         \n\
   SV Data Manager must use the GUI Python Console because data nodes are  \n\
   only defined when SimVascular is executed with the GUI.                 \n\
   \n\
   The dmg module does not define any classes. All access to the           \n\
   SV Data Manager though functions defined for the module.                \n\
   \n\
   Example: Getting data from the 'aorta' data node                        \n\
   \n\
      >>> path = sv.dmg.get_path('aorta')                                  \n\
   \n\
");

//--------------
// PyDmgMethods
//---------------
//
PyMethodDef PyDmgMethods[] =
{
    {"add_segmentation", (PyCFunction)Dmg_add_segmentation, METH_VARARGS|METH_KEYWORDS, Dmg_add_segmentation_doc},

    {"add_geometry", (PyCFunction)Dmg_add_geometry, METH_VARARGS|METH_KEYWORDS, Dmg_add_geometry_doc},

    {"add_mesh", (PyCFunction)Dmg_add_mesh, METH_VARARGS|METH_KEYWORDS, Dmg_add_mesh_doc},

    {"add_model", (PyCFunction)Dmg_add_model, METH_VARARGS|METH_KEYWORDS, Dmg_add_model_doc},

    {"add_path", (PyCFunction)Dmg_add_path, METH_VARARGS|METH_KEYWORDS, Dmg_add_path_doc},

    {"get_segmentations", Dmg_get_segmentations, METH_VARARGS, Dmg_get_segmentations_doc},

    {"get_mesh", Dmg_get_mesh, METH_VARARGS , Dmg_get_mesh_doc},

    {"get_model", Dmg_get_model, METH_VARARGS, Dmg_get_model_doc},

    {"get_path", Dmg_get_path, METH_VARARGS, Dmg_get_path_doc},

    // [TODO:DaveP] not sure to expose this or not, a bit dangerous.
    // {"remove_data_node", Dmg_remove_data_node, METH_VARARGS, Dmg_remove_data_node_doc},

    {NULL, NULL,0,NULL},
};

//-----------------------
// Initialize the module
//-----------------------
// Define the initialization function called by the Python
// interpreter when the module is loaded.

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 3
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 3

// Size of per-interpreter state of the module.
// Set to -1 if the module keeps state in global variables.
static int perInterpreterStateSize = -1;

// Always initialize this to PyModuleDef_HEAD_INIT.
static PyModuleDef_Base m_base = PyModuleDef_HEAD_INIT;

// Define the module definition struct which holds all information
// needed to create a module object.

static struct PyModuleDef PyDmgModule = {
   m_base,
   DMG_MODULE,
   DmgModule_doc,
   perInterpreterStateSize,
   PyDmgMethods
};

//--------------
// PyInit_pyDmg
//--------------
//
PyMODINIT_FUNC
PyInit_PyDmg(void)
{
  // Create the dmg module.
  auto module = PyModule_Create(&PyDmgModule);
  if (module == NULL) {
    fprintf(stdout, "Error initializing the dmg module.\n");
    return SV_PYTHON_ERROR;
  }

  // Add dmg.DmgError exception.
  PyRunTimeErr = PyErr_NewException(DMG_EXCEPTION, NULL, NULL);
  Py_INCREF(PyRunTimeErr);
  PyModule_AddObject(module, DMG_EXCEPTION_OBJECT, PyRunTimeErr);

  return module;
}

#endif

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 2
//---------------------------------------------------------------------------

//------------------
//  initpyDmg
//------------------
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyDmg(void)

{
  PyObject *pyDmg;

  if ( gRepository == NULL ) {
    gRepository = new cvRepository();
    fprintf( stdout, "gRepository created from pyDmg\n" );
    return;
  }

  pyDmg = Py_InitModule("pyDmg", PyDmgMethods);

  PyRunTimeErr = PyErr_NewException("dmg.DmgError",NULL,NULL);
  Py_INCREF(PyRunTimeErr);
  PyModule_AddObject(pyDmg,"DmgError",PyRunTimeErr);

}

#endif

