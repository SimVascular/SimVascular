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
#include "SimVascular.h"
#include "SimVascular_python.h"

#include "sv4gui_Vis_init_py.h"
#include "sv_Repository.h"
#include "sv_PolyData.h"
#include "sv_StrPts.h"
#include "sv_UnstructuredGrid.h"
#include "sv_arg.h"
#include "sv_VTK.h"
#include "vtkTclUtil.h"
#include "vtkPythonUtil.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// The global cvRepository object should be allocated in the file where
// main() lives.

#include "sv2_globals.h"

#include "SimVascular.h"
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

#include <mitkNodePredicateDataType.h>
#include <mitkIDataStorageService.h>
#include <mitkDataNode.h>
#include <mitkBaseRenderer.h>
#include <mitkDataStorage.h>
#include <mitkUndoController.h>
#include <mitkIOUtil.h>
#include <array>
#include <vector>
#include <string>

PyObject* PyRunTimeErr;

PyObject* GUI_ImportPolyDataFromRepos( PyObject* self, PyObject* args);

PyObject* GUI_ImportUnstructuredGridFromRepos( PyObject* self, PyObject* args);

PyObject* GUI_ImportPathFromRepos( PyObject* self, PyObject* args);

PyObject* GUI_ExportModelToRepos( PyObject* self, PyObject* args);

PyObject* GUI_ExportMeshToRepos( PyObject* self, PyObject* args);

PyObject* GUI_ExportImageToRepos( PyObject* self, PyObject* args);

PyObject* GUI_ExportPathToRepos( PyObject* self, PyObject* args);

PyObject* GUI_ImportImageFromFile( PyObject* self, PyObject* args);

PyObject* GUI_ImportContourFromRepos( PyObject* self, PyObject* args);

PyObject* GUI_ExportContourToRepos(PyObject* self, PyObject* args);

PyObject* GUI_RemoveDataNode(PyObject* self, PyObject* args);

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyGUI();
#elif PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyGUI();
#endif

//------------------
//  pyGUI_methods
//------------------

PyMethodDef pyGUI_methods[] =
{
    {"ImportImg",GUI_ImportImageFromFile,METH_VARARGS,NULL},
    {"ImportPolyDataFromRepos",GUI_ImportPolyDataFromRepos,METH_VARARGS,NULL},
    {"ImportUnstructedGridFromRepos",GUI_ImportUnstructuredGridFromRepos,METH_VARARGS,NULL},
    {"ImportPathFromRepos", GUI_ImportPathFromRepos, METH_VARARGS,NULL},
    {"ImportContoursFromRepos", GUI_ImportContourFromRepos, METH_VARARGS, NULL},
    {"ExportModelToRepos",GUI_ExportModelToRepos,METH_VARARGS,NULL},
    {"ExportMeshToRepos",GUI_ExportMeshToRepos,METH_VARARGS,NULL},
    {"ExportImageToRepos",GUI_ExportImageToRepos,METH_VARARGS,NULL},
    {"ExportPathToRepos",GUI_ExportPathToRepos,METH_VARARGS,NULL},
    {"ExportContourToRepos", GUI_ExportContourToRepos, METH_VARARGS, NULL},
    {"RemoveDataNode", GUI_RemoveDataNode, METH_VARARGS, NULL},
    {NULL, NULL,0,NULL},
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyGUImodule = {
   PyModuleDef_HEAD_INIT,
   "pyGUI",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pyGUI_methods
};
#endif

//------------------
//  initpyGUI
//------------------
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyGUI(void)

{

  PyObject *pyGUI;

  if ( gRepository == NULL ) {
    gRepository = new cvRepository();
    fprintf( stdout, "gRepository created from pyGUI\n" );
    return;
  }

  pyGUI = Py_InitModule("pyGUI",pyGUI_methods);

  PyRunTimeErr = PyErr_NewException("pyGUI.error",NULL,NULL);
  Py_INCREF(PyRunTimeErr);
  PyModule_AddObject(pyGUI,"error",PyRunTimeErr);

}
#endif

//------------------
//  PyInit_pyGUI
//------------------
#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyGUI(void)

{

  PyObject *pyGUI;
  pyGUI = PyModule_Create(&pyGUImodule);

  if ( gRepository == NULL ) {
    gRepository = new cvRepository();
    fprintf( stdout, "gRepository created from pyGUI\n" );
  }

  PyRunTimeErr = PyErr_NewException("pyGUI.error",NULL,NULL);
  Py_INCREF(PyRunTimeErr);
  PyModule_AddObject(pyGUI,"error",PyRunTimeErr);

  return pyGUI;

}
#endif

//------------------
//  GUI_pyInit
//------------------
int GUI_pyInit()

{
#if PYTHON_MAJOR_VERSION == 2
    initpyGUI();
#elif PYTHON_MAJOR_VERSION == 3
    PyInit_pyGUI();
#endif
  return SV_OK;

}

//------------------xc
//  buildModelNode
//------------------
sv4guiModel::Pointer buildModelNode(cvRepositoryData *poly,sv4guiModel::Pointer model)
{
    vtkSmartPointer<vtkPolyData>
    polydataObj = (vtkPolyData*)((cvDataObject *)poly)->GetVtkPtr();
    //hardcode type to be PolyData
    sv4guiModelElement* me=sv4guiModelElementFactory::CreateModelElement("PolyData");
    me->SetWholeVtkPolyData(polydataObj);
    sv4guiModelElementAnalytic* meAnalytic=dynamic_cast<sv4guiModelElementAnalytic*>(me);
    if(meAnalytic)
        meAnalytic->SetWholeVtkPolyData(meAnalytic->CreateWholeVtkPolyData());

    model->SetType(me->GetType());
    model->SetModelElement(me);
    model->SetDataModified();
    return model;
}

//------------------
//  buildMeshNode
//------------------

sv4guiMitkMesh::Pointer buildMeshNode(cvRepositoryData *obj, sv4guiMesh* mesh,  sv4guiMitkMesh::Pointer mitkMesh)
{
    vtkSmartPointer<vtkUnstructuredGrid>
    meshObj = (vtkUnstructuredGrid*)((cvDataObject *)obj)->GetVtkPtr();
    meshObj -> Print(std::cout);
    //get surface polydata from the unstructured grid
    vtkSmartPointer<vtkDataSetSurfaceFilter> surfaceFilter =
    vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
    surfaceFilter->SetInputData(meshObj);
    surfaceFilter->Update();
    vtkSmartPointer<vtkPolyData> polydata = surfaceFilter->GetOutput();
    //set surface and volume mesh to the sv4guiMesh
    mesh = sv4guiMeshFactory::CreateMesh("TetGen");
    mesh -> SetVolumeMesh(meshObj);
    mesh -> SetSurfaceMesh(polydata);
    //set mitk mesh
    mitkMesh -> SetMesh(mesh); //try 0 for now
    //mitkMesh ->SetModelName("resultCyl");
    mitkMesh -> SetType("TetGen");
    mitkMesh -> SetDataModified();
    return mitkMesh;
}

//------------------
//  buildPathNode
//------------------

sv4guiPath::Pointer buildPathNode(cvRepositoryData *obj, sv4guiPath::Pointer path)
{
    sv3::PathElement* pathElem = dynamic_cast<sv3::PathElement*> (obj);
    sv4guiPathElement* guiPath = new sv4guiPathElement();

    switch(pathElem->GetMethod())
    {
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

    //copy control points
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

//------------------------
//   buildContourNode
//------------------------
sv4guiContourGroup::Pointer buildContourNode(std::vector<cvRepositoryData *> objs, sv4guiContourGroup::Pointer group, char* pathName)
{
    std::string str(pathName);
    group->SetPathName(str);
    for (int j = 0; j<objs.size(); j++)
    {

        sv3::Contour* sv3contour = dynamic_cast<sv3::Contour*> (objs[j]);
        sv4guiContour* contour = new sv4guiContour();
        sv3::PathElement::PathPoint pathPoint=sv3contour->GetPathPoint();
        sv4guiPathElement::sv4guiPathPoint pthPt;
        for (int i = 0; i<3; i++)
        {
            pthPt.pos[i]=pathPoint.pos[i];
            pthPt.tangent[i] = pathPoint.tangent[i];
            pthPt.rotation[i] = pathPoint.rotation[i];
        }
            pthPt.id = pathPoint.id;
        contour->SetPathPoint(pthPt);
        contour->SetMethod(sv3contour->GetMethod());
        contour->SetPlaced(true);
        contour->SetClosed(sv3contour->IsClosed());
        contour->SetContourPoints(sv3contour->GetContourPoints());

        group->InsertContour(j, contour);
        group->SetDataModified();

    }

    return group;
}
//------------------
//  AddDataNode
//------------------

int AddDataNode(mitk::DataStorage::Pointer dataStorage,
        cvRepositoryData *rd, mitk::DataNode::Pointer folderNode, char* childName)
{
    char r[2048];
    RepositoryDataT type = rd->GetType();

    mitk::DataNode::Pointer Node = mitk::DataNode::New();
    if ( type == POLY_DATA_T )
    {
        sv4guiModel::Pointer model = sv4guiModel::New();
        model = buildModelNode(rd,model);
        Node->SetData(model);
        Node->SetName(childName);
    }
    else if ( type == UNSTRUCTURED_GRID_T)
    {
        sv4guiMesh* mesh;
        sv4guiMitkMesh::Pointer mitkMesh = sv4guiMitkMesh::New();
        mitkMesh = buildMeshNode(rd, mesh, mitkMesh);
        //vtkSmartPointer<vtkUnstructuredGrid> tmp = (mitkMesh->GetMesh())->GetVolumeMesh();
        Node -> SetData(mitkMesh);
        Node ->SetName(childName);
    }
    else if (type ==PATH_T)
    {
        sv4guiPath::Pointer path = sv4guiPath::New();
        path = buildPathNode(rd,path);
        int maxPathID=sv4guiPath::GetMaxPathID(dataStorage->GetDerivations(folderNode));
        path->SetPathID(maxPathID+1);
        Node -> SetData(path);
        Node -> SetName(childName);
    }
    else
    {
        printf("Data object is not supported.\n");
        return SV_ERROR;
    }
    //add new node to its parent node
    mitk::OperationEvent::IncCurrObjectEventId();
    sv4guiDataNodeOperationInterface* interface=new sv4guiDataNodeOperationInterface;
    bool undoEnabled=true;
    sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,dataStorage,Node,folderNode);
    if(undoEnabled)
    {
        sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,dataStorage,Node,folderNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(interface, doOp, undoOp, "Add DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    interface->ExecuteOperation(doOp);

    return SV_OK;

}

//------------------
//  RemoveDataNode
//------------------

int RemoveDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer folderNode, char* childName)
{
    mitk::DataNode::Pointer childNode =dataStorage->GetNamedDerivedNode(childName,folderNode);

    if (folderNode && childNode)
    {
        dataStorage->Remove(childNode);
    }
    else
    {
        return SV_ERROR;
    }

    mitk::OperationEvent::IncCurrObjectEventId();
    sv4guiDataNodeOperationInterface* interface=new sv4guiDataNodeOperationInterface;
    bool undoEnabled=true;
    sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,dataStorage,childNode,folderNode);
    if(undoEnabled)
    {
        sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,dataStorage,childNode,folderNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(interface, doOp, undoOp, "Remove DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    interface->ExecuteOperation(doOp);

    return SV_OK;

}

//------------------
//  AddContourDataNode
//------------------

int AddContourDataNode(mitk::DataStorage::Pointer dataStorage,
        std::vector<cvRepositoryData *>rd, mitk::DataNode::Pointer folderNode,
        char* childName, char* pathName, sv4guiPath::Pointer path)
{

    mitk::DataNode::Pointer Node = mitk::DataNode::New();

    sv4guiContourGroup::Pointer contourGroup = sv4guiContourGroup::New();
    contourGroup = buildContourNode(rd,contourGroup,pathName);
    if(!path.IsNull())
    {
        contourGroup->SetPathID(path->GetPathID());
    }
    Node -> SetData(contourGroup);
    Node -> SetName(childName);

    //add new node to its parent node
    mitk::OperationEvent::IncCurrObjectEventId();
    sv4guiDataNodeOperationInterface* interface=new sv4guiDataNodeOperationInterface;
    bool undoEnabled=true;
    sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,dataStorage,Node,folderNode);
    if(undoEnabled)
    {
        sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,dataStorage,Node,folderNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(interface, doOp, undoOp, "Add DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    interface->ExecuteOperation(doOp);

    return SV_OK;

}

//------------------
//  getFolderNode
//------------------

mitk::DataNode::Pointer getFolderNode(mitk::DataStorage::Pointer dataStorage,
        mitk::DataNode::Pointer projFolderNode, char* folderName)
{
    mitk::DataStorage::SetOfObjects::ConstPointer rs=
    dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New(folderName));
    mitk::DataNode::Pointer folderNode;
    if (rs->size()>0)
    {
        folderNode=rs->GetElement(0);

        if (folderNode.IsNull())
        {
            MITK_ERROR << "Error getting a pointer to the folderNode.";
            return nullptr;

        }
    }
    else
    {
        MITK_ERROR <<"Error getting a pointer to the folderNode.";
        return nullptr;

    }

    return folderNode;

}

//---------------------
// getProjectFolderNode
//---------------------
mitk::DataNode::Pointer getProjectFolderNode(mitk::DataStorage::Pointer dataStorage)
{
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSubset(isProjFolder);
    mitk::DataNode::Pointer projFolderNode;
    if(rs->size()>0)
    {
        projFolderNode=rs->GetElement(0);
    }
    else
    {

        MITK_ERROR <<"No project has been created.";
        return nullptr;

    }
    return projFolderNode;
}
//------------------
//  searchDataNode
//------------------

mitk::DataNode::Pointer searchDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode,
    char* nodeName, char* folderName)
{

    mitk::DataNode::Pointer pathFolderNode =
            getFolderNode(dataStorage,projFolderNode,folderName);
    mitk::DataNode::Pointer exitingNode=NULL;
    if(pathFolderNode.IsNull())
        exitingNode=dataStorage->GetNamedNode(nodeName);
    else
        exitingNode=dataStorage->GetNamedDerivedNode(nodeName,pathFolderNode);

    return exitingNode;

}

//-----------------
// AddImageFromFile
//-----------------
int AddImageFromFile(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer folderNode,
        char* fileName, char*childName, bool copy, double scaleFactor)
{
    try
    {

        mitk::DataNode::Pointer imageNode=sv4guiProjectManager::LoadDataNode(std::string(fileName));

        mitk::NodePredicateDataType::Pointer isImage = mitk::NodePredicateDataType::New("Image");
        if(imageNode.IsNull() || !isImage->CheckNode(imageNode))
        {
            fprintf(stderr, "Not Image!", "Please add an image.");
            return SV_ERROR;
        }

        mitk::BaseData::Pointer mimage = imageNode->GetData();
        if(mimage.IsNull() || !mimage->GetTimeGeometry()->IsValid())
        {
            fprintf(stderr,"Not Valid!", "Please add a valid image.");
            return SV_ERROR;
        }


        sv4guiProjectManager::AddImage(dataStorage, fileName, imageNode, folderNode, copy, scaleFactor, childName);

        return SV_OK;
    }
    catch(...)
    {
        fprintf(stderr,"Error adding image.");
        return SV_ERROR;
    }
}

//---------------------
// MitkImage2VtkImage
//---------------------
//copied from svVTKUtils.h - linker command failed to link with this file?
vtkImageData* MitkImage2VtkImage(mitk::Image* image)
{
    vtkImageData* vtkImg=image->GetVtkImageData();
    mitk::Point3D org = image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetOrigin();
    mitk::BaseGeometry::BoundsArrayType extent=image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetBounds();

    vtkImageData* newVtkImg = vtkImageData::New();
    newVtkImg->ShallowCopy(vtkImg);

    int whole[6];
    double *spacing, origin[3];

    whole[0]=extent[0];
    whole[1]=extent[1]-1;
    whole[2]=extent[2];
    whole[3]=extent[3]-1;
    whole[4]=extent[4];
    whole[5]=extent[5]-1;

    spacing = vtkImg->GetSpacing();

    origin[0] = spacing[0] * whole[0] +org[0];
    origin[1] = spacing[1] * whole[2] +org[1];
    whole[1] -= whole[0];
    whole[3] -= whole[2];
    whole[0] = 0;
    whole[2] = 0;
    origin[2] = spacing[2] * whole[4]+org[2];
    whole[5] -= whole[4];
    whole[4] = 0;

    newVtkImg->SetExtent(whole);
    newVtkImg->SetOrigin(origin);
    newVtkImg->SetSpacing(spacing);

    return newVtkImg;
}
//Python bindings to add data nodes to the data manager and export data to repository
//-------------------------
// GUI_ImportImageFromFile
//-------------------------
PyObject* GUI_ImportImageFromFile( PyObject* self, PyObject* args)
{
    char* fileName;
    char* childName;
    char* parentName;
    int copy = 0;
    double factor = 0.;
    mitk::DataNode::Pointer folderNode;


    if(!PyArg_ParseTuple(args,"ss|sid", &fileName, &childName, &parentName, &copy, &factor))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: fileName, childName and optional char, int, double parentName, copy, factor");

    }

        //get active data storage
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();
    mitk::IDataStorageService* dss = 0;
    ctkServiceReference dsServiceRef;
    if (context)
        dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    else
        printf("Error getting plugin context\n");
    if (dsServiceRef)
    {
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
    }


    if (!dss)
    {
        PyErr_SetString(PyRunTimeErr,"IDataStorageService service not available.");

    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");

    }

    //get project folder
    mitk::DataNode::Pointer projFolderNode = getProjectFolderNode(dataStorage);
    if (projFolderNode.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error finding project folder node; project may not be created");
        return SV_PYTHON_ERROR;
    }
    //default adds to the repository folder
    if(parentName)
    {
    if(strcmp(parentName, "Images")==0)
        folderNode = getFolderNode(dataStorage,projFolderNode,"svImageFolder");
    else
    {
        printf(parentName);
        PyErr_SetString(PyRunTimeErr, "Not a valid folder name for images");

    }
    mitk::DataNode::Pointer existingNode=dataStorage->GetNamedDerivedNode(childName,folderNode);
    if (existingNode)
    {
        PyErr_SetString(PyRunTimeErr, "Image with the same name alreay exists");

    }
    }
    else
        folderNode = getFolderNode(dataStorage,projFolderNode,"svRepositoryFolder");

    if(AddImageFromFile(dataStorage,folderNode,fileName,childName,copy,factor)==SV_ERROR)
    {
        PyErr_SetString(PyRunTimeErr, "Error adding data nodes");

    }

    return SV_PYTHON_OK;
}
// ---------------------
//  GUI_ImportPolyDataFromRepos
// ---------------------
PyObject* GUI_ImportPolyDataFromRepos( PyObject* self, PyObject* args)
{
    char* childName;
    char* parentName=NULL;
    cvRepositoryData *obj;
    mitk::DataNode::Pointer folderNode;

    if(!PyArg_ParseTuple(args,"s|s", &childName, &parentName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: childName, parentName");

    }

    obj = gRepository->GetObject( childName );
    if ( obj == NULL )
    {
        PyErr_SetString(PyRunTimeErr, "couldn't find PolyData" );

    }


    //get active data storage
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();
    mitk::IDataStorageService* dss = 0;
    ctkServiceReference dsServiceRef;
    if (context)
        dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    else
        printf("Error getting plugin context\n");
    if (dsServiceRef)
    {
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
    }


    if (!dss)
    {
        PyErr_SetString(PyRunTimeErr,"IDataStorageService service not available.");

    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");

    }


    //get project folder
    mitk::DataNode::Pointer projFolderNode = getProjectFolderNode(dataStorage);
    if (projFolderNode.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error finding project folder node; project may not be created");
        return SV_PYTHON_ERROR;
    }
    //default adds to the repository folder
    if(parentName)
    {
    if(strcmp(parentName, "Models")==0)
        folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiModelFolder");
    else
    {
        PyErr_SetString(PyRunTimeErr, "No folder with specified name.");

    }
    mitk::DataNode::Pointer existingNode=dataStorage->GetNamedDerivedNode(childName,folderNode);
    if (existingNode)
    {
        PyErr_SetString(PyRunTimeErr, "Object with the same name alreay exists");

    }
    }
    else
    {
        folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiRepositoryFolder");
    }
    if(AddDataNode(dataStorage, obj,folderNode,childName)==SV_ERROR)
    {
        PyErr_SetString(PyRunTimeErr, "Error adding data nodes");

    }


    return SV_PYTHON_OK;


}
// ---------------------
//  GUI_ImportUnstructuredGridFromRepos
// ---------------------
PyObject* GUI_ImportUnstructuredGridFromRepos( PyObject* self, PyObject* args)
{
    char* childName;
    char* parentName=NULL;
    cvRepositoryData *obj;
    mitk::DataNode::Pointer folderNode;

    if(!PyArg_ParseTuple(args,"s|s", &childName, &parentName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: childName, parentName");

    }

    obj = gRepository->GetObject( childName );
    if ( obj == NULL )
    {
        PyErr_SetString(PyRunTimeErr, "couldn't find unstructured grid" );

    }

    //get active data storage
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();
    mitk::IDataStorageService* dss = 0;
    ctkServiceReference dsServiceRef;
    if (context)
        dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    else
        printf("Error getting plugin context\n");
    if (dsServiceRef)
    {
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
    }

    if (!dss)
    {
        PyErr_SetString(PyRunTimeErr,"IDataStorageService service not available.");

    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");

    }

    //get project folder
    mitk::DataNode::Pointer projFolderNode = getProjectFolderNode(dataStorage);
    if (projFolderNode.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error finding project folder node; project may not be created");
        return SV_PYTHON_ERROR;
    }
      //default adds to the repository folder
    if(parentName)
    {
    if(strcmp(parentName, "Meshes")==0)
        folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiMeshFolder");
    else
    {
        PyErr_SetString(PyRunTimeErr, "No folder with specified name.");

    }
    mitk::DataNode::Pointer existingNode=dataStorage->GetNamedDerivedNode(childName,folderNode);
    if (existingNode)
    {
        PyErr_SetString(PyRunTimeErr, "Object with the same name alreay exists");

    }
    }
    else
    {
        folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiRepositoryFolder");
    }
    if(AddDataNode(dataStorage, obj,folderNode,childName)==SV_ERROR)
    {
        PyErr_SetString(PyRunTimeErr, "Error adding data nodes");

    }


    return SV_PYTHON_OK;


}
// ------------------
//  GUI_ExportModelToRepos
// ------------------
PyObject* GUI_ExportModelToRepos( PyObject* self, PyObject* args)
{
    char* childName;
    char* reposName;

    if(!PyArg_ParseTuple(args,"ss", &childName,&reposName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 3 chars: nodeName, reposName");

    }

    if(gRepository->Exists( reposName ))
    {
        PyErr_SetString(PyRunTimeErr, "Name already exists in the repository");

    }

    //get active data storage
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();
    mitk::IDataStorageService* dss = 0;
    ctkServiceReference dsServiceRef;
    if (context)
        dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    else
        printf("Error getting plugin context\n");
    if (dsServiceRef)
    {
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
    }


    if (!dss)
    {
        PyErr_SetString(PyRunTimeErr,"IDataStorageService service not available.");

    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");

    }
    mitk::DataNode::Pointer projFolderNode = getProjectFolderNode(dataStorage);
    if (projFolderNode.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error finding project folder node; project may not be created");
        return SV_PYTHON_ERROR;
    }
    mitk::DataNode::Pointer folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiModelFolder");
    mitk::DataNode::Pointer node = dataStorage->GetNamedDerivedNode(childName,folderNode);
    if(node.IsNull())
    {
        folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiRepositoryFolder");
        node = dataStorage->GetNamedDerivedNode(childName,folderNode);
        if (node.IsNull())
        {
            PyErr_SetString(PyRunTimeErr, "Data node does not exist.");

        }
    }
    if(node.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Data node does not exist.");

    }

    sv4guiModel* model = dynamic_cast<sv4guiModel*> (node->GetData());
    sv4guiModelElement* me = model->GetModelElement();
    vtkSmartPointer<vtkPolyData> pd = me->GetWholeVtkPolyData();
    if (pd == NULL)
    {
        PyErr_SetString(PyRunTimeErr, "Error getting polydata from data storage");

    }

    cvPolyData* cvpd = new cvPolyData( pd );
    if ( !( gRepository->Register( reposName, cvpd ) ) )
    {
        PyErr_SetString(PyRunTimeErr, "error registering object in repository");
        delete cvpd;

    }

    return SV_PYTHON_OK;

}


// ------------------
//  GUI_ExportMeshToRepos
// ------------------
PyObject* GUI_ExportMeshToRepos( PyObject* self, PyObject* args)
{
    char* childName;
    char* reposName;

    if(!PyArg_ParseTuple(args,"ss", &childName,&reposName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 3 chars: nodeName, reposName");

    }

    if(gRepository->Exists( reposName ))
    {
        PyErr_SetString(PyRunTimeErr, "Name already exists in the repository");

    }

    //get active data storage
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();
    mitk::IDataStorageService* dss = 0;
    ctkServiceReference dsServiceRef;
    if (context)
        dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    else
        printf("Error getting plugin context\n");
    if (dsServiceRef)
    {
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
    }


    if (!dss)
    {
        PyErr_SetString(PyRunTimeErr,"IDataStorageService service not available.");

    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");

    }
    mitk::DataNode::Pointer projFolderNode = getProjectFolderNode(dataStorage);
    if (projFolderNode.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error finding project folder node; project may not be created");
        return SV_PYTHON_ERROR;
    }
    mitk::DataNode::Pointer folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiMeshFolder");
    mitk::DataNode::Pointer node = dataStorage->GetNamedDerivedNode(childName,folderNode);
    if(node.IsNull())
    {
        folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiRepositoryFolder");
        node = dataStorage->GetNamedDerivedNode(childName,folderNode);
        if (node.IsNull())
        {
            PyErr_SetString(PyRunTimeErr, "Data node does not exist.");

        }
    }
    if(node.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Data node does not exist.");
    }

    cvUnstructuredGrid *cvug;
    sv4guiMitkMesh* mitkMesh = dynamic_cast<sv4guiMitkMesh*> (node->GetData());
    if (!mitkMesh) return SV_PYTHON_ERROR;
    sv4guiMesh* mesh = mitkMesh->GetMesh();
    vtkSmartPointer<vtkUnstructuredGrid> ug = mesh->GetVolumeMesh();
    if(ug == NULL)
    {
        PyErr_SetString(PyRunTimeErr, "Error getting data from data storage");

    }
    cvug = new cvUnstructuredGrid(ug);
    if ( !( gRepository->Register( reposName, cvug ) ) )
    {
        PyErr_SetString(PyRunTimeErr, "error registering object in repository");
        delete cvug;

    }

    return SV_PYTHON_OK;

}

// ------------------
//  GUI_ExportImageToRepos
// ------------------
PyObject* GUI_ExportImageToRepos( PyObject* self, PyObject* args)
{
    char* childName;
    char* reposName;

    if(!PyArg_ParseTuple(args,"ss", &childName,&reposName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 3 chars: nodeName, reposName");

    }

    if(gRepository->Exists( reposName ))
    {
        PyErr_SetString(PyRunTimeErr, "Name already exists in the repository");

    }

    //get active data storage
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();
    mitk::IDataStorageService* dss = 0;
    ctkServiceReference dsServiceRef;
    if (context)
        dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    else
        printf("Error getting plugin context\n");
    if (dsServiceRef)
    {
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
    }


    if (!dss)
    {
        PyErr_SetString(PyRunTimeErr,"IDataStorageService service not available.");

    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");

    }
    mitk::DataNode::Pointer projFolderNode = getProjectFolderNode(dataStorage);
    if (projFolderNode.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error finding project folder node; project may not be created");
        return SV_PYTHON_ERROR;
    }
    mitk::DataNode::Pointer folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiImageFolder");
    mitk::DataNode::Pointer node = dataStorage->GetNamedDerivedNode(childName,folderNode);
    if(node.IsNull())
    {
        folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiRepositoryFolder");
        node = dataStorage->GetNamedDerivedNode(childName,folderNode);
        if (node.IsNull())
        {
            PyErr_SetString(PyRunTimeErr, "Data node does not exist.");

        }
    }
    if(node.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Data node does not exist.");

    }

    mitk::Image* image=dynamic_cast<mitk::Image*>(node->GetData());
    if (!image) return SV_PYTHON_ERROR;
    vtkImageData* vtkObj=MitkImage2VtkImage(image);
    if (vtkObj)
    {
        cvStrPts *sp;
        vtkStructuredPoints *mysp = vtkStructuredPoints::New();
        mysp->ShallowCopy(vtkObj);
        // need to shift the origin like what used to be done
        // in vtkImageToStructuredPoints class

        int whole[6];
        int extent[6];
        double *spacing, origin[3];

        vtkObj->GetExtent(whole);
        spacing = vtkObj->GetSpacing();
        vtkObj->GetOrigin(origin);

        origin[0] += spacing[0] * whole[0];
        origin[1] += spacing[1] * whole[2];
        whole[1] -= whole[0];
        whole[3] -= whole[2];
        whole[0] = 0;
        whole[2] = 0;
        // shift Z origin for 3-D images
        if (whole[4] > 0 && whole[5] > 0) {
            origin[2] += spacing[2] * whole[4];
            whole[5] -= whole[4];
            whole[4] = 0;
        }
        mysp->SetExtent(whole);

        mysp->SetOrigin(origin);
        mysp->SetSpacing(spacing);
        sp = new cvStrPts (mysp);
        mysp->Delete();
        sp->SetName( reposName );
        if ( !( gRepository->Register( reposName, sp ) ) )
        {
            PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
            delete sp;

        }
    }
    else
    {
        PyErr_SetString(PyRunTimeErr, "Error getting vtk object from mitk image.");

    }


    return SV_PYTHON_OK;

}

// -----------------------
//  GUI_ExportPathToRepos
// -----------------------
PyObject* GUI_ExportPathToRepos( PyObject* self, PyObject* args)
{
    char* childName=NULL;
    char* reposName=NULL;

    if(!PyArg_ParseTuple(args,"ss", &childName,&reposName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: nodeName, reposName");

    }

    if(gRepository->Exists( reposName ))
    {
        PyErr_SetString(PyRunTimeErr, "Name already exists in the repository");

    }

    //get active data storage
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();
    mitk::IDataStorageService* dss = 0;
    ctkServiceReference dsServiceRef;
    if (context)
        dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    else
        printf("Error getting plugin context\n");
    if (dsServiceRef)
    {
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
    }


    if (!dss)
    {
        PyErr_SetString(PyRunTimeErr,"IDataStorageService service not available.");

    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);
    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");

    }
    mitk::DataNode::Pointer projFolderNode = getProjectFolderNode(dataStorage);
    if (projFolderNode.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error finding project folder node; project may not be created");
        return SV_PYTHON_ERROR;
    }
    mitk::DataNode::Pointer folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiPathFolder");
    mitk::DataNode::Pointer node = dataStorage->GetNamedDerivedNode(childName,folderNode);
    if(node.IsNull())
    {
        folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiRepositoryFolder");
        node = dataStorage->GetNamedDerivedNode(childName,folderNode);
        if (node.IsNull())
        {
            PyErr_SetString(PyRunTimeErr, "Data node does not exist.");

        }
    }
    sv4guiPath* path = dynamic_cast<sv4guiPath*> (node->GetData());
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr, "Error getting path from data storage");

    }
    sv4guiPathElement* pathElem = path->GetPathElement();


    sv3::PathElement* corePath = new sv3::PathElement();
    switch(pathElem->GetMethod())
    {
    case sv4guiPathElement::CONSTANT_TOTAL_NUMBER:
        corePath->SetMethod(sv3::PathElement::CONSTANT_TOTAL_NUMBER);
        break;
    case sv4guiPathElement::CONSTANT_SUBDIVISION_NUMBER:
        corePath->SetMethod(sv3::PathElement::CONSTANT_SUBDIVISION_NUMBER);
        break;
    case sv4guiPathElement::CONSTANT_SPACING:
        corePath->SetMethod(sv3::PathElement::CONSTANT_SPACING);
        break;
    default:
        break;
    }
    corePath->SetCalculationNumber(pathElem->GetCalculationNumber());
    corePath->SetSpacing(pathElem->GetSpacing());
    //copy control points
    std::vector<mitk::Point3D> pts = pathElem->GetControlPoints();
    for (int i=0; i<pts.size();i++)
    {
        std::array<double,3> point;
        point[0] = pts[i][0];
        point[1] = pts[i][1];
        point[2] = pts[i][2];
        corePath->InsertControlPoint(i,point);
    }
    //create path points
    corePath->CreatePathPoints();

    if ( !( gRepository->Register( reposName, corePath ) ) )
    {
        PyErr_SetString(PyRunTimeErr, "error registering object in repository");
        delete corePath;

    }
    return SV_PYTHON_OK;
}

// -----------------------
//  GUI_ImportPathFromRepos
// -----------------------

PyObject* GUI_ImportPathFromRepos( PyObject* self, PyObject* args)
{
    char* childName;
    char* parentName=NULL;
    RepositoryDataT type;
    cvRepositoryData *obj;
    mitk::DataNode::Pointer folderNode;

    if(!PyArg_ParseTuple(args,"s|s", &childName, &parentName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: childName, parentName");

    }

    obj = gRepository->GetObject( childName );
    if ( obj == NULL )
    {
        PyErr_SetString(PyRunTimeErr, "couldn't find path" );

    }

    type = obj->GetType();

    if ( type != PATH_T )
    {
        PyErr_SetString(PyRunTimeErr,"not a path object");

    }

    //get active data storage
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();
    mitk::IDataStorageService* dss = 0;
    ctkServiceReference dsServiceRef;
    if (context)
        dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    else
        printf("Error getting plugin context\n");
    if (dsServiceRef)
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);

    if (!dss)
    {
        PyErr_SetString(PyRunTimeErr,"IDataStorageService service not available.");

    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");

    }


    //get project folder
    mitk::DataNode::Pointer projFolderNode = getProjectFolderNode(dataStorage);
    if (projFolderNode.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error finding project folder node; project may not be created");
        return SV_PYTHON_ERROR;
    }
    //default adds to the repository folder
    if(parentName)
    {
        if(strcmp(parentName, "Paths")==0)
        {
            folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiPathFolder");
            mitk::DataNode::Pointer existingNode=dataStorage->GetNamedDerivedNode(childName,folderNode);
            if (existingNode)
            {
                PyErr_SetString(PyRunTimeErr, "Path with the same name alreay exists");

            }
        }
        else
        {
            PyErr_SetString(PyRunTimeErr, "No folder with specified name.");

        }
    }
    else
    {
        folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiRepositoryFolder");
    }

    if(AddDataNode(dataStorage, obj,folderNode,childName)==SV_ERROR)
    {
        PyErr_SetString(PyRunTimeErr, "Error adding data nodes");

    }

    return SV_PYTHON_OK;


}

// -----------------------
//  GUI_ImportContourFromRepos
// -----------------------

PyObject* GUI_ImportContourFromRepos( PyObject* self, PyObject* args)
{
    char* childName;
    char* pathName;
    char* parentName=NULL;
    PyObject* srcList=NULL;
    RepositoryDataT type;

    mitk::DataNode::Pointer folderNode;

    if(!PyArg_ParseTuple(args,"sOs|s", &childName, &srcList, &pathName, &parentName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 2 chars and one list: childName, srcList and parentName");

    }

    int numsrc = PyList_Size(srcList);
    std::vector<cvRepositoryData *> objs(numsrc);
    char r[2048];
    for (int i=0; i<numsrc; i++)
    {
        r[0] = '\0';
#if PYTHON_MAJOR_VERSION == 2
        char* srcName = PyString_AsString(PyList_GetItem(srcList,i));
#endif
#if PYTHON_MAJOR_VERSION == 3
        char* srcName = PyBytes_AsString(PyUnicode_AsUTF8String(PyList_GetItem(srcList,i)));
#endif
        objs[i] = gRepository->GetObject( srcName );
        if ( objs[i] == NULL )
        {
            sprintf(r, "Couldn't find contour %s", srcName);
            PyErr_SetString(PyRunTimeErr, r );

        }

        type = objs[i]->GetType();
        r[0] = '\0';
        if ( type != CONTOUR_T )
        {
            sprintf(r, "Not a contour object: %s", srcName);
            PyErr_SetString(PyRunTimeErr,r);

        }
    }
       //get active data storage
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();
    mitk::IDataStorageService* dss = 0;
    ctkServiceReference dsServiceRef;
    if (context)
        dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    else
        printf("Error getting plugin context\n");
    if (dsServiceRef)
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);

    if (!dss)
    {
        PyErr_SetString(PyRunTimeErr,"IDataStorageService service not available.");

    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");

    }

    //get project folder
    mitk::DataNode::Pointer projFolderNode = getProjectFolderNode(dataStorage);
    if (projFolderNode.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error finding project folder node; project may not be created");
        return SV_PYTHON_ERROR;
    }
    mitk::DataNode::Pointer pathNode = searchDataNode(dataStorage, projFolderNode, pathName,"sv4guiPathFolder");
    sv4guiPath::Pointer path;
    if (pathNode.IsNull())
    {
        pathNode = searchDataNode(dataStorage, projFolderNode, pathName,"sv4guiRepositoryFolder");
        if (!pathNode.IsNull())
            path = dynamic_cast<sv4guiPath*> (pathNode->GetData());
    }
    else
        path = dynamic_cast<sv4guiPath*> (pathNode->GetData());

    if (path.IsNull())
    {
        printf("Segmentations require path; no path was found");
    }
    //default adds to the repository folder
    if(parentName)
    {
        if(strcmp(parentName, "Segmentations")==0)
        {
            folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiSegmentationFolder");
            mitk::DataNode::Pointer existingNode=dataStorage->GetNamedDerivedNode(childName,folderNode);
            if (existingNode)
            {
                PyErr_SetString(PyRunTimeErr, "Segmentation with the same name alreay exists");

            }
        }
        else
        {
            PyErr_SetString(PyRunTimeErr, "No folder with specified name.");

        }
    }
    else
    {
        folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiRepositoryFolder");
    }
    if(AddContourDataNode(dataStorage, objs,folderNode,childName,pathName,path)==SV_ERROR)
    {
        PyErr_SetString(PyRunTimeErr, "Error adding data nodes");

    }

    return SV_PYTHON_OK;

}

// -----------------------
//  GUI_ExportContourToRepos
// -----------------------
PyObject* GUI_ExportContourToRepos( PyObject* self, PyObject* args)
{
    char* childName=NULL;
    PyObject* dstList=NULL;


    if(!PyArg_ParseTuple(args,"sO", &childName,&dstList))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import one char and one list: nodeName, dstList");

    }

    int num = PyList_Size(dstList);
    std::vector<char*> strList(num);
    char r[2048];
    for (int i=0; i<num; i++)
    {
#if PYTHON_MAJOR_VERSION == 2
        strList[i] = PyString_AsString(PyList_GetItem(dstList,i));
#endif
#if PYTHON_MAJOR_VERSION == 3
        strList[i] = PyBytes_AsString(PyUnicode_AsUTF8String(PyList_GetItem(dstList,i)));
#endif
        r[0] = '\0';
        if(gRepository->Exists( strList[i] ))
        {
            sprintf(r, "Name %s already exists in the repository", strList[i]);
            PyErr_SetString(PyRunTimeErr, "Name already exists in the repository");

        }
    }
    //get active data storage
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();
    mitk::IDataStorageService* dss = 0;
    ctkServiceReference dsServiceRef;
    if (context)
        dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    else
        printf("Error getting plugin context\n");
    if (dsServiceRef)
    {
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
    }


    if (!dss)
    {
        PyErr_SetString(PyRunTimeErr,"IDataStorageService service not available.");

    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);
    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");

    }
    mitk::DataNode::Pointer projFolderNode = getProjectFolderNode(dataStorage);
    if (projFolderNode.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error finding project folder node; project may not be created");
        return SV_PYTHON_ERROR;
    }
    mitk::DataNode::Pointer folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiSegmentationFolder");
    mitk::DataNode::Pointer node = dataStorage->GetNamedDerivedNode(childName,folderNode);
    if(node.IsNull())
    {
        folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiRepositoryFolder");
        node = dataStorage->GetNamedDerivedNode(childName,folderNode);
        if (node.IsNull())
        {
            PyErr_SetString(PyRunTimeErr, "Data node does not exist.");

        }
    }
    sv4guiContourGroup* group = dynamic_cast<sv4guiContourGroup*> (node->GetData());
    if (group==NULL)
    {
        PyErr_SetString(PyRunTimeErr, "Error getting contour group from data storage");

    }
    for (int i=0; i<num; i++)
    {
        cKernelType kernel;
        sv4guiContour* contour = group->GetContour(i);
        vtkSmartPointer<vtkPolyData> contourPd = contour->CreateVtkPolyDataFromContour();
        r[0] = '\0';
        if (contourPd==NULL)
        {
            sprintf(r, "Error getting polydata from contour %s", r);
            PyErr_SetString(PyRunTimeErr, r);

        }
        cvPolyData* cvpd = new cvPolyData( contourPd );
        if ( !( gRepository->Register( strList[i], cvpd ) ) )
        {
            PyErr_SetString(PyRunTimeErr, "error registering object in repository");

        }
    }
    return SV_PYTHON_OK;
}


// -----------------------
//  GUI_RemoveDataNode
// -----------------------
PyObject* GUI_RemoveDataNode( PyObject* self, PyObject* args)
{
    char* childName=NULL;
    char* parentName=NULL;


    if(!PyArg_ParseTuple(args,"ss", &childName,&parentName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import two chars, childName, and folderName");
        return SV_PYTHON_ERROR;
    }

    //get active data storage
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context = sv4guiPythonDataNodesPluginActivator::GetContext();
    mitk::IDataStorageService* dss = 0;
    ctkServiceReference dsServiceRef;
    if (context)
        dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    else
        printf("Error getting plugin context\n");
    if (dsServiceRef)
    {
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
    }


    if (!dss)
    {
        PyErr_SetString(PyRunTimeErr,"IDataStorageService service not available.");

    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");
        return SV_PYTHON_ERROR;
    }


    //get project folder
    mitk::DataNode::Pointer projFolderNode = getProjectFolderNode(dataStorage);
    if (projFolderNode.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error finding project folder node; project may not be created");
        return SV_PYTHON_ERROR;
    }

    mitk::DataNode::Pointer parentNode=dataStorage->GetNamedDerivedNode(parentName,projFolderNode);
    if(!parentNode)
    {
        PyErr_SetString(PyRunTimeErr, "Folder node not found");
        return SV_PYTHON_ERROR;
    }
    if(RemoveDataNode(dataStorage, parentNode,childName)==SV_ERROR)
    {
        PyErr_SetString(PyRunTimeErr, "Error removing data nodes");
        return SV_PYTHON_ERROR;
    }


    return SV_PYTHON_OK;


}

