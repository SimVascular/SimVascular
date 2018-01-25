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

#include "svPathIO.h"
#include "svPath.h"
#include "svXmlIOUtil.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

static mitk::CustomMimeType CreatesvPathMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svpath");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("pth");
    mimeType.SetComment("SimVascular Path");

    return mimeType;
}

svPathIO::svPathIO()
    : mitk::AbstractFileIO(svPath::GetStaticNameOfClass(), CreatesvPathMimeType(), "SimVascular Path")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> svPathIO::Read()
{
    std::string fileName=GetInputLocation();

    return ReadFile(fileName);
}

std::vector<mitk::BaseData::Pointer> svPathIO::ReadFile(std::string fileName)
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

    TiXmlElement* pathElement = document.FirstChildElement("path");

    if(!pathElement){
//        MITK_ERROR << "No path data in "<< fileName;
        mitkThrow() << "No path data in "<< fileName;
    }

    svPath::Pointer path = svPath::New();
    int pathID=0;
    pathElement->QueryIntAttribute("id",&pathID);
    path->SetPathID(pathID);
    int method=0;
    pathElement->QueryIntAttribute("method", &method);
    int calculationNumber=0;
    pathElement->QueryIntAttribute("calculation_number", &calculationNumber);
    double spacing=0.0;
    pathElement->QueryDoubleAttribute("spacing", &spacing);
    path->SetMethod( (svPathElement::CalculationMethod) method);
    path->SetCalculationNumber(calculationNumber);
    path->SetSpacing(spacing);

    double resliceSize=5.0;
    pathElement->QueryDoubleAttribute("reslice_size", &resliceSize);
    path->SetResliceSize(resliceSize);

    std::string point2dsize="",point3dsize="";
    pathElement->QueryStringAttribute("point_2D_display_size", &point2dsize);
    pathElement->QueryStringAttribute("point_size", &point3dsize);
    path->SetProp("point 2D display size",point2dsize);
    path->SetProp("point size",point3dsize);

    int timestep=-1;
    for( TiXmlElement* timestepElement = pathElement->FirstChildElement("timestep");
         timestepElement != nullptr;
         timestepElement = timestepElement->NextSiblingElement("timestep") )
    {
        if (timestepElement == nullptr)
            continue;

//        timestepElement->QueryIntAttribute("id",&timestep);
        timestep++;
        path->Expand(timestep+1);

        TiXmlElement* peElement=timestepElement->FirstChildElement("path_element");

        if (peElement == nullptr)
            continue;

        svPathElement* pe=new svPathElement();

        int method=0;
        peElement->QueryIntAttribute("method", &method);
        int calculationNumber=0;
        peElement->QueryIntAttribute("calculation_number", &calculationNumber);
        double spacing=0.0;
        peElement->QueryDoubleAttribute("spacing", &spacing);
        pe->SetMethod( (svPathElement::CalculationMethod) method);
        pe->SetCalculationNumber(calculationNumber);
        pe->SetSpacing(spacing);

        //control points without updating contour points
        TiXmlElement* controlpointsElement = peElement->FirstChildElement("control_points");
        std::vector<mitk::Point3D> controlPoints;
        for( TiXmlElement* pointElement = controlpointsElement->FirstChildElement("point");
             pointElement != nullptr;
             pointElement = pointElement->NextSiblingElement("point") )
        {
            if (pointElement == nullptr)
                continue;

            controlPoints.push_back(svXmlIOUtil::GetPoint(pointElement));
        }
        pe->SetControlPoints(controlPoints,false);

        //path points
        TiXmlElement* pathpointsElement = peElement->FirstChildElement("path_points");
        std::vector<svPathElement::svPathPoint> pathPoints;
        for( TiXmlElement* pointElement = pathpointsElement->FirstChildElement("path_point");
             pointElement != nullptr;
             pointElement = pointElement->NextSiblingElement("path_point") )
        {
            if (pointElement == nullptr)
                continue;

            svPathElement::svPathPoint pathPoint;
            int id=0;
            pointElement->QueryIntAttribute("id", &id);
            pathPoint.id=id;
            pathPoint.pos=svXmlIOUtil::GetPoint(pointElement->FirstChildElement("pos"));
            pathPoint.tangent=svXmlIOUtil::GetVector(pointElement->FirstChildElement("tangent"));
            pathPoint.rotation=svXmlIOUtil::GetVector(pointElement->FirstChildElement("rotation"));

            pathPoints.push_back(pathPoint);
        }
        pe->SetPathPoints(pathPoints);

        path->SetPathElement(pe,timestep);

    }//timestep

    std::vector<mitk::BaseData::Pointer> result;
    result.push_back(path.GetPointer());
    return result;
}

mitk::IFileIO::ConfidenceLevel svPathIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void svPathIO::Write()
{
    ValidateOutputLocation();

    const svPath* path = dynamic_cast<const svPath*>(this->GetInput());
    if(!path) return;

    TiXmlDocument document;
    auto  decl = new TiXmlDeclaration( "1.0", "UTF-8", "" );
    document.LinkEndChild( decl );

    auto  version = new TiXmlElement("format");
    version->SetAttribute("version",  "1.0" );
    document.LinkEndChild(version);

    auto  pathElement = new TiXmlElement("path");
    pathElement->SetAttribute("id", path->GetPathID());
    pathElement->SetAttribute("method", path->GetMethod());
    pathElement->SetAttribute("calculation_number", path->GetCalculationNumber());
    pathElement->SetDoubleAttribute("spacing", path->GetSpacing());
    pathElement->SetDoubleAttribute("reslice_size", path->GetResliceSize());
    pathElement->SetAttribute("point_2D_display_size",path->GetProp("point 2D display size"));
    pathElement->SetAttribute("point_size",path->GetProp("point size"));
    document.LinkEndChild(pathElement);

    for(int t=0;t<path->GetTimeSize();t++)
    {
        auto  timestepElement = new TiXmlElement("timestep");
        timestepElement->SetAttribute("id",t);
        pathElement->LinkEndChild(timestepElement);

        svPathElement* pe=path->GetPathElement(t);
        if(!pe) continue;

        auto  peElement = new TiXmlElement("path_element");
        timestepElement->LinkEndChild(peElement);

        peElement->SetAttribute("id",0);
        peElement->SetAttribute("method", pe->GetMethod());
        peElement->SetAttribute("calculation_number", pe->GetCalculationNumber());
        peElement->SetDoubleAttribute("spacing", pe->GetSpacing());

        auto  controlpointsElement = new TiXmlElement("control_points");
        peElement->LinkEndChild(controlpointsElement);
        for(int i=0;i<pe->GetControlPointNumber();i++)
        {
            controlpointsElement->LinkEndChild(svXmlIOUtil::CreateXMLPointElement("point",i,pe->GetControlPoint(i)));
        }

        auto  pathpointsElement = new TiXmlElement("path_points");
        peElement->LinkEndChild(pathpointsElement);
        for(int i=0;i<pe->GetPathPointNumber();i++)
        {
            auto  pathpointElement = new TiXmlElement("path_point");
            pathpointsElement->LinkEndChild(pathpointElement);

            svPathElement::svPathPoint pathPoint=pe->GetPathPoint(i);
            pathpointElement->SetAttribute("id",pathPoint.id);
            pathpointElement->LinkEndChild(svXmlIOUtil::CreateXMLPointElement("pos", pathPoint.pos));
            pathpointElement->LinkEndChild(svXmlIOUtil::CreateXMLVectorElement("tangent", pathPoint.tangent));
            pathpointElement->LinkEndChild(svXmlIOUtil::CreateXMLVectorElement("rotation", pathPoint.rotation));
        }

    }

    std::string fileName=GetOutputLocation();
    if (document.SaveFile(fileName) == false)
    {
        mitkThrow() << "Could not write path to " << fileName;

    }
}

mitk::IFileIO::ConfidenceLevel svPathIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const svPath* input = dynamic_cast<const svPath*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

svPathIO* svPathIO::IOClone() const
{
    return new svPathIO(*this);
}
