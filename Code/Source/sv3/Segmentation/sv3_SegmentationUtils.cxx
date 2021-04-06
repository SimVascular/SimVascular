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
#include "sv3_SegmentationUtils.h"
#include "sv_StrPts.h"

#include <vtkImageData.h>
#include <vtkStructuredPoints.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkImageReslice.h>

using sv3::SegmentationUtils;

double SV_PI=3.1415926535;

double SegmentationUtils::math_radToDeg(double rad){
    return rad*180.0/SV_PI;
}

double SegmentationUtils::math_dot(double vecA[3], double vecB[3])
{
    return vecA[0]*vecB[0]+vecA[1]*vecB[1]+vecA[2]*vecB[2];
}

void SegmentationUtils::math_cross(double cross[3], double vecA[3], double vecB[3])
{
    cross[0]=vecA[1]*vecB[2]-vecA[2]*vecB[1];
    cross[1]=vecA[2]*vecB[0]-vecA[0]*vecB[2];
    cross[2]=vecA[0]*vecB[1]-vecA[1]*vecB[0];
}

double SegmentationUtils::math_magnitude(double vecA[3])
{
    return sqrt(math_dot(vecA,vecA));
}

double SegmentationUtils::math_angleBtw3DVectors(double vecA[3], double vecB[3])
{
    double dot=math_dot(vecA, vecB);
    double magA=math_magnitude(vecA);
    double magB=math_magnitude(vecB);
    double cosTheta=dot / (magA * magB);
    if (cosTheta >= 1) {
        cosTheta=1;
    }
    return acos(cosTheta);
}
cvStrPts* SegmentationUtils::vtkImageData2cvStrPts(vtkImageData* vtkImg)
{
    vtkStructuredPoints *mysp = vtkStructuredPoints::New();
    mysp->ShallowCopy(vtkImg);

    int whole[6];
    //    int extent[6];
    double *spacing, origin[3];

    vtkImg->GetExtent(whole);

    spacing = vtkImg->GetSpacing();
    vtkImg->GetOrigin(origin);

    origin[0] += spacing[0] * whole[0];
    origin[1] += spacing[1] * whole[2];
    whole[1] -= whole[0];
    whole[3] -= whole[2];
    whole[0] = 0;
    whole[2] = 0;
    // shift Z origin for 3-D images
    //    if (whole[4] > 0 && whole[5] > 0) {
    origin[2] += spacing[2] * whole[4];
    whole[5] -= whole[4];
    whole[4] = 0;
    //    }
    mysp->SetExtent(whole);
    mysp->SetOrigin(origin);
    mysp->SetSpacing(spacing);

    cvStrPts *sp;
    sp = new cvStrPts (mysp);

    //    mysp->Delete();

    return sp;
}

std::deque<int> SegmentationUtils::GetOrderedPtIDs(vtkCellArray* lines, bool& ifClosed)
{

    vtkIdType *ptIds;
    vtkIdType npts;

    lines->InitTraversal();

    std::vector<std::vector<int>> lineList;

    std::deque<int> linkedPtList;

    while ( lines->GetNextCell( npts, ptIds ) ) {

        if(npts!=2) break;

        std::vector<int> ids;
        ids.push_back(ptIds[0]);
        ids.push_back(ptIds[1]);

        lineList.push_back(ids);
    }

    bool firstTime=true;

    while(lineList.size()>0)
    {
        if(firstTime)
        {
            linkedPtList.push_back(lineList[0][0]);
            linkedPtList.push_back(lineList[0][1]);

            lineList.erase(lineList.begin()+0);
            firstTime=false;
        }
        else
        {

            int first=linkedPtList.front();
            int last=linkedPtList.back();

            bool firstLinked=false;
            bool lastLinked=false;

            for(int i=0;i<lineList.size();i++){

                if(firstLinked&&lastLinked) break;

                int id1=lineList[i][0];
                int id2=lineList[i][1];

                if(!firstLinked&&(first==id1||first==id2))
                {
                    if(first==id1){
                        linkedPtList.push_front(id2);
                    }else{
                        linkedPtList.push_front(id1);
                    }
                    firstLinked=true;
                    lineList.erase(lineList.begin()+i);
                    i--;
                    continue;
                } else if(!lastLinked&&(last==id1||last==id2))
                {
                    if(last==id1){
                        linkedPtList.push_back(id2);
                    }else{
                        linkedPtList.push_back(id1);
                    }
                    lastLinked=true;
                    lineList.erase(lineList.begin()+i);
                    i--;
                    continue;
                }

            }

            if(!firstLinked&&!lastLinked) break;
        }

    }

    ifClosed=false;
    if(linkedPtList.size()>0&&linkedPtList.front()==linkedPtList.back())
    {
        ifClosed=true;
        linkedPtList.pop_back();
    }

    return linkedPtList;

}

vtkTransform* SegmentationUtils::GetvtkTransform(sv3::PathElement::PathPoint pathPoint)
{
    double pos[3],nrm[3],xhat[3];

    pos[0]=pathPoint.pos[0];
    pos[1]=pathPoint.pos[1];
    pos[2]=pathPoint.pos[2];

    nrm[0]=pathPoint.tangent[0];
    nrm[1]=pathPoint.tangent[1];
    nrm[2]=pathPoint.tangent[2];

    xhat[0]=pathPoint.rotation[0];
    xhat[1]=pathPoint.rotation[1];
    xhat[2]=pathPoint.rotation[2];

    double zhat[3]={0,0,1};
    double theta=math_radToDeg(math_angleBtw3DVectors(zhat,nrm));
    double axis[3];
    math_cross(axis,zhat,nrm);

    vtkTransform* tmpTr=vtkTransform::New();
    tmpTr->Identity();
    tmpTr->RotateWXYZ(theta,axis);

    vtkPoints* tmpPt=vtkPoints::New();
    tmpPt->InsertNextPoint(1, 0, 0);

    vtkPolyData* tmpPd=vtkPolyData::New();
    tmpPd->SetPoints(tmpPt);

    vtkTransformPolyDataFilter* tmpTf=vtkTransformPolyDataFilter::New();
    tmpTf->SetInputDataObject(tmpPd);
    tmpTf->SetTransform(tmpTr);
    tmpTf->Update();
    double pt[3];
    tmpTf->GetOutput()->GetPoint(0,pt);

    tmpTr->Delete();
    tmpPt->Delete();
    tmpPd->Delete();
    tmpTf->Delete();

    double rot=math_radToDeg(math_angleBtw3DVectors(pt,xhat));

    double x[3];
    math_cross(x,pt,xhat);
    double d=math_dot(x,nrm);
    if (d < 0.0) {
        rot=-rot;
    }

    vtkTransform* tr=vtkTransform::New();
    tr->Identity();
    tr->Translate(pos);
    tr->RotateWXYZ(rot,nrm);
    tr->RotateWXYZ(theta,axis);
    return tr;
}

vtkImageData* SegmentationUtils::GetSlicevtkImage(sv3::PathElement::PathPoint pathPoint, vtkImageData* volumeimage, double size)
{
    vtkTransform* tr =GetvtkTransform(pathPoint);
    vtkImageReslice* rs=vtkImageReslice::New();

    double spacing[3];
    volumeimage->GetSpacing(spacing);
    double vmin=std::min(spacing[0],std::min(spacing[0],spacing[1]));

    int width=size/vmin;
    int height=size/vmin;
    double pdimx=width*vmin;
    double pdimy=height*vmin;

    double ors[3];
    ors[0]=-0.5*pdimx;
    ors[1]=-0.5*pdimy;
    ors[2]=0.0;

    rs->SetInputDataObject(volumeimage);

    rs->SetResliceTransform(tr);
    rs->SetOutputSpacing(vmin,vmin,vmin);
    rs->SetOutputOrigin(ors);
    rs->SetOutputExtent(0,width-1,0,height-1,0,0);
    rs->InterpolateOn();
    rs->Update();

    return rs->GetOutput();
}

vtkPlane* SegmentationUtils::CreatePlaneGeometry(sv3::PathElement::PathPoint pathPoint, std::array<double,3> spacing, double size)
{
    vtkTransform* tr=GetvtkTransform(pathPoint);
    vtkPlane* planegeometry = vtkPlane::New();

    planegeometry->SetOrigin(pathPoint.pos[0],pathPoint.pos[1],pathPoint.pos[2]);
    planegeometry->SetNormal(pathPoint.tangent[0],pathPoint.tangent[1],pathPoint.tangent[2]);

    return planegeometry;
}


void SegmentationUtils::getOrthogonalVector(double normal[3], double vec[3])
{
    if(normal[2]!=0.)
    {
        vec[0] = 1.;
        vec[1] = 1.;
        vec[2] = (0.-1.*normal[0]-1.*normal[1])/normal[2];
    }
    else if (normal[1]!=0.)
    {
        vec[0] = 1.;
        vec[1] = (0.-1.*normal[0]-1.*normal[2])/normal[1];
        vec[2] = 1.;
    }
    else if (normal[0]!=0.)
    {
        vec[0] = (0.-1.*normal[1]-1.*normal[2])/normal[0];
        vec[1] = 1.;
        vec[2] = 1.;
    }
    else
        return;
        
    double lth = sqrt(pow(vec[0],2)+pow(vec[1],2)+pow(vec[2],2));
    vec[0]/=lth; vec[1]/=lth; vec[2]/=lth; 
}
