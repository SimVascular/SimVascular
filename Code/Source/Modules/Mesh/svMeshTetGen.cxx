#include "svMeshTetGen.h"

//#include "svMath3.h"
//#include "svModelUtils.h"
#include "svStringUtils.h"


//#include "cv_polydatasolid_utils.h"
//#include "cv_VMTK_utils.h"

#include "cvSolidModel.h"

#include <iostream>
using namespace std;

svMeshTetGen::svMeshTetGen()
{
    m_Type="TetGen";
    m_cvTetGetMesh=new cvTetGenMeshObject(NULL);
}

svMeshTetGen::svMeshTetGen(const svMeshTetGen &other)
    : svMesh(other)
{
}

svMeshTetGen::~svMeshTetGen()
{
    if(m_cvTetGetMesh!=NULL)
        delete m_cvTetGetMesh;
}

svMeshTetGen* svMeshTetGen::Clone()
{
    return new svMeshTetGen(*this);
}

bool svMeshTetGen::SetModelElement(svModelElement* modelElement)
{
    if(!svMesh::SetModelElement(modelElement))
        return false;

    if(modelElement==NULL||modelElement->GetWholeVtkPolyData()==NULL)
        return false;

    std::string modelType=modelElement->GetType();

    SolidModel_KernelT kernel= SolidModel_KernelT_StrToEnum( modelType.c_str() );

    m_cvTetGetMesh->SetSolidModelKernel(kernel);

    if(m_cvTetGetMesh->LoadModel(modelElement->GetWholeVtkPolyData())!=CV_OK)
        return false;

    //set walls
    //m_cvTetGetMesh->SetMeshOptions("MeshWallFirst",0,NULL) //not nessary, because in "SetWalls" it'll be set at 1 again.
    std::vector<int> wallFaceIDs=modelElement->GetWallFaceIDs();
    if(m_cvTetGetMesh->SetWalls(wallFaceIDs.size(),&wallFaceIDs[0])!=CV_OK)
        return false;

    return true;
}

bool svMeshTetGen::ExecuteCommand(std::string cmd)
{
    svStringUtils::Trim(cmd);
    if(cmd=="")
        return false;

    std::vector<std::string> eles=svStringUtils::Split(cmd);
    int cmdSize=eles.size();
    if(cmdSize==0)
        return false;

//    std::string flag=eles[0];
    int numValues=cmdSize-1;
    double *values=new double[20]();

    try{

        if(eles[0]=="SurfaceMeshFlag")
        {
            if(numValues<1)
                return false;

            values[0]=std::stod(eles[1]);
            if(m_cvTetGetMesh->SetMeshOptions("SurfaceMeshFlag",numValues,values)!=CV_OK)
                return false;
        }
        else if(eles[0]=="" )
        {

        }

    }catch (...){
        delete[] values;
        return false;
    }

    delete[] values;
    return true;
}
