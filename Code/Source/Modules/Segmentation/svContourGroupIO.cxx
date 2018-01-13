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

#include "svContourGroupIO.h"
#include "svContourGroup.h"
#include "svContourCircle.h"
#include "svContourEllipse.h"
#include "svContourPolygon.h"
#include "svContourSplinePolygon.h"
#include "svContourTensionPolygon.h"
#include "svXmlIOUtil.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

static mitk::CustomMimeType CreatesvContourGroupMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svcontourgroup");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("ctgr");
    mimeType.SetComment("SimVascular ContourGroup");

    return mimeType;
}

svContourGroupIO::svContourGroupIO()
    : mitk::AbstractFileIO(svContourGroup::GetStaticNameOfClass(), CreatesvContourGroupMimeType(), "SimVascular ContourGroup")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> svContourGroupIO::Read()
{
    std::string fileName=GetInputLocation();
    return ReadFile(fileName);
}

std::vector<mitk::BaseData::Pointer> svContourGroupIO::ReadFile(std::string fileName)
{
    TiXmlDocument document;

    if (!document.LoadFile(fileName))
    {
        mitkThrow() << "Could not open/read/parse " << fileName;
        //        MITK_ERROR << "Could not open/read/parse " << fileName;
        std::vector<mitk::BaseData::Pointer> empty;
        return empty;
    }

    //    TiXmlElement* version = document.FirstChildElement("format");

    TiXmlElement* groupElement = document.FirstChildElement("contourgroup");

    if(!groupElement){
//        MITK_ERROR << "No ContourGroup data in "<< fileName;
        mitkThrow() << "No ContourGroup data in "<< fileName;
    }

    svContourGroup::Pointer group = svContourGroup::New();

    group->SetPathName(groupElement->Attribute("path_name"));
    int pathID=0;
    groupElement->QueryIntAttribute("path_id",&pathID);
    group->SetPathID(pathID);

    double resliceSize=5.0;
    groupElement->QueryDoubleAttribute("reslice_size", &resliceSize);
    group->SetResliceSize(resliceSize);

    std::string point2dsize="",point3dsize="";
    groupElement->QueryStringAttribute("point_2D_display_size", &point2dsize);
    groupElement->QueryStringAttribute("point_size", &point3dsize);
    group->SetProp("point 2D display size",point2dsize);
    group->SetProp("point size",point3dsize);

    int timestep=-1;
    for( TiXmlElement* timestepElement = groupElement->FirstChildElement("timestep");
         timestepElement != nullptr;
         timestepElement = timestepElement->NextSiblingElement("timestep") )
    {
        if (timestepElement == nullptr)
            continue;

//        timestepElement->QueryIntAttribute("id",&timestep);
        timestep++;
        group->Expand(timestep+1);

        //lofting parameters
        if(timestep==0)
        {
            TiXmlElement* loftParamElement = timestepElement->FirstChildElement("lofting_parameters");
            if(loftParamElement!=nullptr)
            {
                svLoftingParam* param=group->GetLoftingParam();

                loftParamElement->QueryStringAttribute("method", &param->method);

                loftParamElement->QueryIntAttribute("sampling", &param->numOutPtsInSegs);
                loftParamElement->QueryIntAttribute("sample_per_seg",&param->samplePerSegment);
                loftParamElement->QueryIntAttribute("use_linear_sample",&param->useLinearSampleAlongLength);
                loftParamElement->QueryIntAttribute("linear_multiplier",&param->linearMuliplier);
                loftParamElement->QueryIntAttribute("use_fft",&param->useFFT);
                loftParamElement->QueryIntAttribute("num_modes",&param->numModes);

                loftParamElement->QueryIntAttribute("u_degree",&param->uDegree);
                loftParamElement->QueryIntAttribute("v_degree",&param->vDegree);
                loftParamElement->QueryStringAttribute("u_knot_type",&param->uKnotSpanType);
                loftParamElement->QueryStringAttribute("v_knot_type",&param->vKnotSpanType);
                loftParamElement->QueryStringAttribute("u_parametric_type",&param->uParametricSpanType);
                loftParamElement->QueryStringAttribute("v_parametric_type",&param->vParametricSpanType);
            }
        }

        for( TiXmlElement* contourElement = timestepElement->FirstChildElement("contour");
             contourElement != nullptr;
             contourElement = contourElement->NextSiblingElement("contour") )
        {
            if (contourElement == nullptr)
                continue;

            std::string type;
            contourElement->QueryStringAttribute("type", &type);

            svContour* contour;

            if(type=="Circle")
            {
                contour=new svContourCircle();
            }
            else if(type=="Ellipse")
            {
                contour=new svContourEllipse();
            }
            else if(type=="Polygon")
            {
                contour=new svContourPolygon();
            }
            else if(type=="SplinePolygon")
            {
                contour=new svContourSplinePolygon();
            }
            else if(type=="TensionPolygon")
            {
                contour=new svContourTensionPolygon();
            }
            else
            {
                contour=new svContour();
            }

            svContourEllipse* ce=dynamic_cast<svContourEllipse*>(contour);
            if(ce)
            {
                std::string asCircle;
                contourElement->QueryStringAttribute("as_circle", &asCircle);
                ce->SetAsCircle(asCircle=="true"?true:false);
            }

            svContourTensionPolygon* ct=dynamic_cast<svContourTensionPolygon*>(contour);
            if(ct)
            {
                int subdivisionRounds=0;
                contourElement->QueryIntAttribute("subdivision_rounds", &subdivisionRounds);
                double tensionParam=0.0;
                contourElement->QueryDoubleAttribute("tension_param", &tensionParam);
                ct->SetSubdivisionRounds(subdivisionRounds);
                ct->SetTensionParameter(tensionParam);
            }

            std::string method;
            contourElement->QueryStringAttribute("method", &method);
            contour->SetMethod(method);
            std::string closed;
            contourElement->QueryStringAttribute("closed", &closed);
            contour->SetClosed(closed=="false"?false:true);
            int minControlNumber,maxControlNumber;
            contourElement->QueryIntAttribute("min_control_number", &minControlNumber);
            contour->SetMinControlPointNumber(minControlNumber);
            contourElement->QueryIntAttribute("max_control_number", &maxControlNumber);
            contour->SetMaxControlPointNumber(maxControlNumber);
            int subdivisionType=0;
            contourElement->QueryIntAttribute("subdivision_type", &subdivisionType);
            int subdivisionNumber=0;
            contourElement->QueryIntAttribute("subdivision_number", &subdivisionNumber);
            double spacing=1.0;
            contourElement->QueryDoubleAttribute("subdivision_spacing", &spacing);
            contour->SetSubdivisionType( (svContour::SubdivisionType) subdivisionType);
            contour->SetSubdivisionNumber(subdivisionNumber);
            contour->SetSubdivisionSpacing(spacing);

            contour->SetPlaced();

            //path point
            TiXmlElement* pathpointElement = contourElement->FirstChildElement("path_point");
            if(pathpointElement!=nullptr)
            {
                svPathElement::svPathPoint pathPoint;
                int id=0;
                pathpointElement->QueryIntAttribute("id", &id);
                pathPoint.id=id;
                pathPoint.pos=svXmlIOUtil::GetPoint(pathpointElement->FirstChildElement("pos"));
                pathPoint.tangent=svXmlIOUtil::GetVector(pathpointElement->FirstChildElement("tangent"));
                pathPoint.rotation=svXmlIOUtil::GetVector(pathpointElement->FirstChildElement("rotation"));

                contour->SetPathPoint(pathPoint);
            }

            //control points without updating contour points
            TiXmlElement* controlpointsElement = contourElement->FirstChildElement("control_points");
            if(controlpointsElement!=nullptr)
            {
                std::vector<mitk::Point3D> controlPoints;
                for( TiXmlElement* pointElement = controlpointsElement->FirstChildElement("point");
                     pointElement != nullptr;
                     pointElement = pointElement->NextSiblingElement("point") )
                {
                    if (pointElement == nullptr)
                        continue;

                    controlPoints.push_back(svXmlIOUtil::GetPoint(pointElement));
                }
                contour->SetControlPoints(controlPoints,false);
            }

            //contour points
            TiXmlElement* contourpointsElement = contourElement->FirstChildElement("contour_points");
            if(contourpointsElement!=nullptr)
            {
                std::vector<mitk::Point3D> contourPoints;
                for( TiXmlElement* pointElement = contourpointsElement->FirstChildElement("point");
                     pointElement != nullptr;
                     pointElement = pointElement->NextSiblingElement("point") )
                {
                    if (pointElement == nullptr)
                        continue;

                    contourPoints.push_back(svXmlIOUtil::GetPoint(pointElement));
                }
                contour->SetContourPoints(contourPoints,false);
            }

            group->InsertContour(-1,contour,timestep);
        } //contour

    }//timestep

    std::vector<mitk::BaseData::Pointer> result;
    result.push_back(group.GetPointer());
    return result;
}

mitk::IFileIO::ConfidenceLevel svContourGroupIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void svContourGroupIO::Write()
{
    ValidateOutputLocation();

    const svContourGroup* group = dynamic_cast<const svContourGroup*>(this->GetInput());
    if(!group) return;

    TiXmlDocument document;
    auto  decl = new TiXmlDeclaration( "1.0", "UTF-8", "" );
    document.LinkEndChild( decl );

    auto  version = new TiXmlElement("format");
    version->SetAttribute("version",  "1.0" );
    document.LinkEndChild(version);

    auto  groupElement = new TiXmlElement("contourgroup");
    groupElement->SetAttribute("path_name", group->GetPathName());
    groupElement->SetAttribute("path_id", group->GetPathID());
    groupElement->SetDoubleAttribute("reslice_size", group->GetResliceSize());
    groupElement->SetAttribute("point_2D_display_size",group->GetProp("point 2D display size"));
    groupElement->SetAttribute("point_size",group->GetProp("point size"));
    document.LinkEndChild(groupElement);

    for(int t=0;t<group->GetTimeSize();t++)
    {
        auto  timestepElement = new TiXmlElement("timestep");
        timestepElement->SetAttribute("id",t);
        groupElement->LinkEndChild(timestepElement);

        //lofting parameters
        if(t==0)
        {
            auto loftParamElement = new TiXmlElement("lofting_parameters");
            timestepElement->LinkEndChild(loftParamElement);
            svLoftingParam* param=group->GetLoftingParam();
            loftParamElement->SetAttribute("method",param->method);

            loftParamElement->SetAttribute("sampling",param->numOutPtsInSegs);
            loftParamElement->SetAttribute("sample_per_seg",param->samplePerSegment);
            loftParamElement->SetAttribute("use_linear_sample",param->useLinearSampleAlongLength);
            loftParamElement->SetAttribute("linear_multiplier",param->linearMuliplier);
            loftParamElement->SetAttribute("use_fft",param->useFFT);
            loftParamElement->SetAttribute("num_modes",param->numModes);

            loftParamElement->SetAttribute("u_degree",param->uDegree);
            loftParamElement->SetAttribute("v_degree",param->vDegree);
            loftParamElement->SetAttribute("u_knot_type",param->uKnotSpanType);
            loftParamElement->SetAttribute("v_knot_type",param->vKnotSpanType);
            loftParamElement->SetAttribute("u_parametric_type",param->uParametricSpanType);
            loftParamElement->SetAttribute("v_parametric_type",param->vParametricSpanType);
        }

        for(int i=0;i<group->GetSize(t);i++)
        {
            svContour* contour=group->GetContour(i,t);
            if(!contour) continue;

            auto  contourElement = new TiXmlElement("contour");
            timestepElement->LinkEndChild(contourElement);
            std::string type=contour->GetType();
            contourElement->SetAttribute("id",i);
            contourElement->SetAttribute("type",type);
            contourElement->SetAttribute("method",contour->GetMethod());
            contourElement->SetAttribute("closed",contour->IsClosed()?"true":"false");
            contourElement->SetAttribute("min_control_number",contour->GetMinControlPointNumber());
            contourElement->SetAttribute("max_control_number",contour->GetMaxControlPointNumber());
            contourElement->SetAttribute("subdivision_type",contour->GetSubdivisionType());
            contourElement->SetAttribute("subdivision_number",contour->GetSubdivisionNumber());
            contourElement->SetDoubleAttribute("subdivision_spacing",contour->GetSubdivisionSpacing());

            svContourEllipse* ce=dynamic_cast<svContourEllipse*>(contour);
            if(ce)
            {
                contourElement->SetAttribute("as_circle",ce->AsCircle()?"true":"false");
            }

            svContourTensionPolygon* ct=dynamic_cast<svContourTensionPolygon*>(contour);
            if(ct)
            {
                contourElement->SetAttribute("subdivision_rounds",ct->GetSubdivisionRounds());
                contourElement->SetDoubleAttribute("tension_param",ct->GetTensionParameter());
            }

            //path point
            auto  pathpointElement = new TiXmlElement("path_point");
            contourElement->LinkEndChild(pathpointElement);
            pathpointElement->SetAttribute("id",contour->GetPathPoint().id);

            pathpointElement->LinkEndChild(svXmlIOUtil::CreateXMLPointElement("pos",contour->GetPathPoint().pos));
            pathpointElement->LinkEndChild(svXmlIOUtil::CreateXMLVectorElement("tangent",contour->GetPathPoint().tangent));
            pathpointElement->LinkEndChild(svXmlIOUtil::CreateXMLVectorElement("rotation",contour->GetPathPoint().rotation));

            //control points
            auto  controlpointsElement = new TiXmlElement("control_points");
            contourElement->LinkEndChild(controlpointsElement);
            for(int j=0;j<contour->GetControlPointNumber();j++)
            {
                controlpointsElement->LinkEndChild(svXmlIOUtil::CreateXMLPointElement("point",j,contour->GetControlPoint(j)));
            }

            //contour points
            auto  contourpointsElement = new TiXmlElement("contour_points");
            contourElement->LinkEndChild(contourpointsElement);
            for(int j=0;j<contour->GetContourPointNumber();j++)
            {
                contourpointsElement->LinkEndChild(svXmlIOUtil::CreateXMLPointElement("point",j,contour->GetContourPoint(j)));
            }

        }

    }

    std::string fileName=GetOutputLocation();
    if (document.SaveFile(fileName) == false)
    {
        mitkThrow() << "Could not write contourgroup to " << fileName;

    }
}

mitk::IFileIO::ConfidenceLevel svContourGroupIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const svContourGroup* input = dynamic_cast<const svContourGroup*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

svContourGroupIO* svContourGroupIO::IOClone() const
{
    return new svContourGroupIO(*this);
}

