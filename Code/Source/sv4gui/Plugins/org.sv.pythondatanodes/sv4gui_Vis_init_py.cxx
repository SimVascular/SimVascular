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

PyObject* PyRunTimeErr;

PyObject* GUI_ImportFromRepos( PyObject* self, PyObject* args);

PyObject* GUI_ImportPathFromRepos( PyObject* self, PyObject* args);

PyObject* GUI_ExportToRepos( PyObject* self, PyObject* args);

PyObject* GUI_ExportPathToRepos( PyObject* self, PyObject* args);

PyObject* GUI_ImportImageFromFile( PyObject* self, PyObject* args);

PyMODINIT_FUNC initpyGUI(void);

//------------------
//  pyGUI_methods
//------------------

PyMethodDef pyGUI_methods[] =
{
    {"gui_importImg",GUI_ImportImageFromFile,METH_VARARGS,NULL},
    {"gui_importFromRepos",GUI_ImportFromRepos,METH_VARARGS,NULL},
    {"gui_importPathFromRepos", GUI_ImportPathFromRepos, METH_VARARGS,NULL},
    {"gui_exportToRepos",GUI_ExportToRepos,METH_VARARGS,NULL},
    {"gui_exportPathToRepos",GUI_ExportPathToRepos,METH_VARARGS,NULL},
    {NULL, NULL,0,NULL},
};

//------------------
//  initpyGUI
//------------------

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


//------------------
//  GUI_pyInit
//------------------
int GUI_pyInit()

{

  Py_Initialize();
  initpyGUI();
  return Py_OK;

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
        //tmp -> Print(std::cout);
        Node -> SetData(mitkMesh);
        Node ->SetName(childName);
    }
    else if (type ==PATH_T)
    {
        sv4guiPath::Pointer path = sv4guiPath::New();
        path = buildPathNode(rd,path);
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
//  getFolderNode
//------------------

mitk::DataNode::Pointer getFolderNode(mitk::DataStorage::Pointer dataStorage, 
        mitk::DataNode::Pointer projFolderNode, char* folderName, mitk::DataNode::Pointer folderNode)
{
    mitk::DataStorage::SetOfObjects::ConstPointer rs=
    dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New(folderName));
    
        if (rs->size()>0)
    {
        folderNode=rs->GetElement(0);

        if (folderNode.IsNull())
        {
            PyErr_SetString(PyRunTimeErr, "Error getting a pointer to the folderNode.");
            return Py_ERROR;
        }
    }
    else
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to the folderNode.");
        return Py_ERROR;
    }
    
    return folderNode;
    
}

//-----------------
// AddImageFromFile
//-----------------
int AddImageFromFile(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer folderNode,
        char* fileName, char*childName, bool copy, double scaleFactor)
{
    try
    {
        
        mitk::DataNode::Pointer imageNode=mitk::IOUtil::LoadDataNode(std::string(fileName));

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
        return Py_ERROR;
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
        return Py_ERROR;
    }
    
    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");
        return Py_ERROR;
    }
    
    //get project folder
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSubset(isProjFolder);
    
    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);
      //default adds to the repository folder
      if(parentName)
      {
        if(strcmp(parentName, "Images")==0)
            folderNode = getFolderNode(dataStorage,projFolderNode,"svImageFolder", folderNode);
        else
        {
            printf(parentName);
            PyErr_SetString(PyRunTimeErr, "Not a valid folder name for images");
            return Py_ERROR;
        }
      }
      else
          folderNode = getFolderNode(dataStorage,projFolderNode,"svRepositoryFolder", folderNode);
      
      if(AddImageFromFile(dataStorage,folderNode,fileName,childName,copy,factor)==Py_ERROR)
      {
          PyErr_SetString(PyRunTimeErr, "Error adding data nodes");
          return Py_ERROR;
      }

    }
    else
    {
            PyErr_SetString(PyRunTimeErr, "No project has been created.");
            return Py_ERROR;
    }
    return Py_BuildValue("s","success");
}
// ---------------------
//  GUI_ImportFromRepos
// ---------------------
PyObject* GUI_ImportFromRepos( PyObject* self, PyObject* args)
{
    char* childName;
    char* parentName=NULL;
    cvRepositoryData *obj;
    mitk::DataNode::Pointer folderNode;
    
    if(!PyArg_ParseTuple(args,"s|s", &childName, &parentName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: childName, parentName");
        return Py_ERROR;
    }
            
    obj = gRepository->GetObject( childName );
    if ( obj == NULL )
    {
        PyErr_SetString(PyRunTimeErr, "couldn't find model" );
        return Py_ERROR;
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
        return Py_ERROR;
    }
    
    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");
        return Py_ERROR;
    }
    
    
    //get project folder
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSubset(isProjFolder);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);
      //default adds to the repository folder
      if(parentName)
      {
        if(strcmp(parentName, "Models")==0)
            folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiModelFolder", folderNode);
        else if(strcmp(parentName, "Meshes")==0)
            folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiMeshFolder", folderNode);
        else
        {
            PyErr_SetString(PyRunTimeErr, "No folder with specified name.");
            return Py_ERROR;
        }
      }
      else
      {
          folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiRepositoryFolder", folderNode);
      }
      if(AddDataNode(dataStorage, obj,folderNode,childName)==Py_ERROR)
      {
          PyErr_SetString(PyRunTimeErr, "Error adding data nodes");
          return Py_ERROR;
      }
    }
    else
    {
            PyErr_SetString(PyRunTimeErr, "No project has been created.");
            return Py_ERROR;
    }
    
    return Py_BuildValue("s","success");
    

}

// ------------------
//  GUI_ExportToRepos
// ------------------
PyObject* GUI_ExportToRepos( PyObject* self, PyObject* args)
{
    char* childName;
    char* parentName;
    char* reposName;
    
    if(!PyArg_ParseTuple(args,"sss", &childName,&reposName, &parentName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 3 chars: nodeName, reposName, dataType");
        return Py_ERROR;
    }
    
    if(gRepository->Exists( reposName ))
    {
        PyErr_SetString(PyRunTimeErr, "Name already exists in the repository");
        return Py_ERROR;
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
        return Py_ERROR;
    }
    
    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");
        return Py_ERROR;
    }
    
    mitk::DataNode::Pointer node = dataStorage->GetNamedNode(childName);
    if(node.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Data node does not exist.");
        return Py_ERROR;
    }
    
    if (strcmp(parentName, "Models")==0)
    {
        sv4guiModel* model = dynamic_cast<sv4guiModel*> (node->GetData());
        sv4guiModelElement* me = model->GetModelElement();
        vtkSmartPointer<vtkPolyData> pd = me->GetWholeVtkPolyData();
        if (pd == NULL)
        {
            PyErr_SetString(PyRunTimeErr, "Error getting polydata from data storage");
            return Py_ERROR;
        }

        cvPolyData* cvpd = new cvPolyData( pd );
        if ( !( gRepository->Register( reposName, cvpd ) ) )
        {
            PyErr_SetString(PyRunTimeErr, "error registering object in repository");
            delete cvpd;
            return Py_ERROR;
        }
    }
    else if (strcmp(parentName, "Meshes")==0)
    {
        cvUnstructuredGrid *cvug;
        sv4guiMitkMesh* mitkMesh = dynamic_cast<sv4guiMitkMesh*> (node->GetData());
        if (!mitkMesh) return Py_ERROR;
        sv4guiMesh* mesh = mitkMesh->GetMesh();
        vtkSmartPointer<vtkUnstructuredGrid> ug = mesh->GetVolumeMesh();
        if(ug == NULL)
        {
            PyErr_SetString(PyRunTimeErr, "Error getting data from data storage");
            return Py_ERROR;
        }
        cvug = new cvUnstructuredGrid(ug);
        if ( !( gRepository->Register( reposName, cvug ) ) )
        {
            PyErr_SetString(PyRunTimeErr, "error registering object in repository");
            delete cvug;
            return Py_ERROR;
        }
    }
    else if (strcmp(parentName, "Images")==0)
    {
        mitk::Image* image=dynamic_cast<mitk::Image*>(node->GetData());
        if (!image) return Py_ERROR;
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
                return Py_ERROR;
            }
        }
        else
        {
            PyErr_SetString(PyRunTimeErr, "Error getting vtk object from mitk image.");
            return Py_ERROR;
        }
    }
    else
    {
        PyErr_SetString(PyRunTimeErr, "Object type is not supported");
        return Py_ERROR;
    }
    
    return Py_BuildValue("s","success");
    
}

// -----------------------
//  GUI_ExportPathToRepos
// -----------------------
PyObject* GUI_ExportPathToRepos( PyObject* self, PyObject* args)
{
    char* childName;
    char* reposName;
    
    if(!PyArg_ParseTuple(args,"ss", &childName,&reposName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: nodeName, reposName");
        return Py_ERROR;
    }
    
    if(gRepository->Exists( reposName ))
    {
        PyErr_SetString(PyRunTimeErr, "Name already exists in the repository");
        return Py_ERROR;
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
        return Py_ERROR;
    }
    
    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");
        return Py_ERROR;
    }
    
    mitk::DataNode::Pointer node = dataStorage->GetNamedNode(childName);
    if(node.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Data node does not exist.");
        return Py_ERROR;
    }
    
    sv4guiPath* path = dynamic_cast<sv4guiPath*> (node->GetData());
    sv4guiPathElement* pathElem = path->GetPathElement();
    
    if (pathElem == NULL)
    {
        PyErr_SetString(PyRunTimeErr, "Error getting path from data storage");
        return Py_ERROR;
    }

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
        return Py_ERROR;
    }
    
    return Py_BuildValue("s","success");
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
        return Py_ERROR;
    }
            
    obj = gRepository->GetObject( childName );
    if ( obj == NULL )
    {
        PyErr_SetString(PyRunTimeErr, "couldn't find path" );
        return Py_ERROR;
    }
    
    type = obj->GetType();

    if ( type != PATH_T )
    {
        PyErr_SetString(PyRunTimeErr,"not a path object");
        return Py_ERROR;
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
        return Py_ERROR;
    }
    
    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();
    if (dataStorage.IsNull())
    {
        PyErr_SetString(PyRunTimeErr, "Error getting a pointer to dataStorage.");
        return Py_ERROR;
    }
    
    
    //get project folder
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSubset(isProjFolder);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);
      //default adds to the repository folder
      if(parentName)
      {
        if(strcmp(parentName, "Paths")==0)
            folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiPathFolder", folderNode);
        else
        {
            PyErr_SetString(PyRunTimeErr, "No folder with specified name.");
            return Py_ERROR;
        }
      }
      else
      {
          folderNode = getFolderNode(dataStorage,projFolderNode,"sv4guiRepositoryFolder", folderNode);
      }

      if(AddDataNode(dataStorage, obj,folderNode,childName)==Py_ERROR)
      {
          PyErr_SetString(PyRunTimeErr, "Error adding data nodes");
          return Py_ERROR;
      }
    }
    else
    {
            PyErr_SetString(PyRunTimeErr, "No project has been created.");
            return Py_ERROR;
    }
    
    return Py_BuildValue("s","success");
    

}