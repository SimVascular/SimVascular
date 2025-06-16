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

#include "sv3_PathIO.h"
#include "sv3_PathElement.h"
#include "sv3_XmlIOUtil.h"
#include <iostream>
#include <array>
#include <vector>
using sv3::PathIO;
using sv3::PathElement;
using sv3::PathGroup;
using sv3::XmlIOUtil;

PathGroup* PathIO::ReadFile(std::string fileName)
{
    //std::string msg("PathIO::ReadFile");
    //std::cout << msg << "=================== PathIO::ReadFile =========" << std::endl;
    //std::cout << msg << "fileName: " << fileName << std::endl;
    tinyxml2::XMLDocument document;

    if (document.LoadFile(fileName.c_str()) != tinyxml2::XML_SUCCESS)
    {
        std::cout<<"Could not open/read/parse " << fileName<<std::endl;
        return nullptr;
    }

    auto path = document.FirstChildElement("path");

    if(!path){
        std::cout<< "No path data in "<< fileName <<std::endl;
        return nullptr;
    }
    
    PathGroup* pathGrp = new PathGroup();
    int pathID=0;
    path->QueryIntAttribute("id",&pathID);
    pathGrp->SetPathID(pathID);
    int method=0;
    path->QueryIntAttribute("method", &method);
    int calculationNumber=0;
    path->QueryIntAttribute("calculation_number", &calculationNumber);
    double spacing=0.0;
    path->QueryDoubleAttribute("spacing", &spacing);
    pathGrp->SetMethod( (sv3::PathElement::CalculationMethod) method);
    pathGrp->SetCalculationNumber(calculationNumber);
    pathGrp->SetSpacing(spacing);
   
        
    int timestep=-1;
    for( auto timestepElement = path->FirstChildElement("timestep"); timestepElement != nullptr;
         timestepElement = timestepElement->NextSiblingElement("timestep") )
    {
        if (timestepElement == nullptr)
            continue;

        timestep++;
        pathGrp->Expand(timestep+1);

        auto peElement = timestepElement->FirstChildElement("path_element");

        if (peElement == nullptr)
            continue;

        PathElement* pe = new PathElement();
        if(ReadPath(document, pe, peElement)==SV_ERROR)
            return nullptr;
        
        pathGrp->SetPathElement(pe,timestep);

    }//timestep
    
    return pathGrp;
}

int PathIO::ReadPath(tinyxml2::XMLDocument& document, PathElement* path, tinyxml2::XMLElement* pathXml)
{
        
    if(!pathXml){
        std::cout << "No path element data"<<std::endl;
        return SV_ERROR;
    }

    int method=0;
    pathXml->QueryIntAttribute("method", &method);
    int calculationNumber=0;
    pathXml->QueryIntAttribute("calculation_number", &calculationNumber);
    double spacing=0.0;
    pathXml->QueryDoubleAttribute("spacing", &spacing);
    path->SetMethod( (sv3::PathElement::CalculationMethod) method);
    path->SetCalculationNumber(calculationNumber);
    path->SetSpacing(spacing);

    //control points without updating contour points
    auto controlpointsElement = pathXml->FirstChildElement("control_points");
    std::vector<std::array<double, 3> > controlPoints;

    XmlIOUtil xml_io(document);

    for( auto pointElement = controlpointsElement->FirstChildElement("point"); pointElement != nullptr;
        pointElement = pointElement->NextSiblingElement("point") )
    {
        if (pointElement == nullptr)
            continue;

        controlPoints.push_back(xml_io.GetPoint(pointElement));
    }
    path->SetControlPoints(controlPoints,false);

    //path points
    auto pathpointsElement = pathXml->FirstChildElement("path_points");
    std::vector<PathElement::PathPoint> pathPoints;

    for( auto pointElement = pathpointsElement->FirstChildElement("path_point"); pointElement != nullptr;
        pointElement = pointElement->NextSiblingElement("path_point") )
    {
        if (pointElement == nullptr)
            continue;

        PathElement::PathPoint pathPoint;
        int id=0;
        pointElement->QueryIntAttribute("id", &id);
        pathPoint.id=id;
        pathPoint.pos = xml_io.GetPoint(pointElement->FirstChildElement("pos"));
        pathPoint.tangent = xml_io.GetVector(pointElement->FirstChildElement("tangent"));
        pathPoint.rotation = xml_io.GetVector(pointElement->FirstChildElement("rotation"));

        pathPoints.push_back(pathPoint);
    }
    path->SetPathPoints(pathPoints);
    
    //if ( !( gRepository->Register( pathName, path ) ) ) {
    //    std::cout<<"error registering path in repository"<<std::endl;
    //    return SV_ERROR;
    //}
    
    return SV_OK;
}

int PathIO::Write(std::string fileName, PathGroup* pathGrp)
{
    if(!pathGrp) return SV_ERROR;

    tinyxml2::XMLDocument document;
    auto decl = document.NewDeclaration();
    document.LinkEndChild( decl );

    auto version = document.NewElement("format");
    version->SetAttribute("version",  "1.0" );
    document.LinkEndChild(version);
    
    auto pathElem = document.NewElement("path");
    //the sv3::PathElement class does not have a GetResliceSize function, temporarily ignoring here.
    pathElem->SetAttribute("id", pathGrp->GetPathID());
    pathElem->SetAttribute("method", pathGrp->GetMethod());
    pathElem->SetAttribute("calculation_number", pathGrp->GetCalculationNumber());
    pathElem->SetAttribute("spacing", pathGrp->GetSpacing());
    
    document.LinkEndChild(pathElem);
    
    for(int t=0;t<pathGrp->GetTimeSize();t++)
    {
        auto  timeStepElem = document.NewElement("timestep");
        timeStepElem->SetAttribute("id",t);
        pathElem->LinkEndChild(timeStepElem);
                
        PathElement* pe=pathGrp->GetPathElement(t);
        if(!pe) 
        {
            std::cout<<"Warning: empty path element" <<std::endl;
            continue;
        }
        
        WritePath(document, pe, timeStepElem);
    }

    if (document.SaveFile(fileName.c_str()) != tinyxml2::XML_SUCCESS)
    {
        std::cout<< "Could not write path to " << fileName<<std::endl;
        return SV_ERROR;
    }
    
    return SV_OK;
}
void PathIO::WritePath(tinyxml2::XMLDocument& document, PathElement* path, tinyxml2::XMLElement* timeStepElem)
{
    auto pathXml = document.NewElement("path_element");
    timeStepElem->LinkEndChild(pathXml);
    pathXml->SetAttribute("method", path->GetMethod());
    pathXml->SetAttribute("calculation_number", path->GetCalculationNumber());
    pathXml->SetAttribute("spacing", path->GetSpacing());

    auto  controlpointsElement = document.NewElement("control_points");
    pathXml->LinkEndChild(controlpointsElement);
    XmlIOUtil xml_io(document);

    for(int i=0;i<path->GetControlPointNumber();i++)
    {
        controlpointsElement->LinkEndChild(xml_io.CreateXMLPointElement("point",i,path->GetControlPoint(i)));
    }

    auto  pathpointsElement = document.NewElement("path_points");
    pathXml->LinkEndChild(pathpointsElement);

    for(int i=0;i<path->GetPathPointNumber();i++)
    {
        auto  pathpointElement = document.NewElement("path_point");
        pathpointsElement->LinkEndChild(pathpointElement);

        PathElement::PathPoint pathPoint=path->GetPathPoint(i);
        pathpointElement->SetAttribute("id",pathPoint.id);
        pathpointElement->LinkEndChild(xml_io.CreateXMLPointElement("pos", pathPoint.pos));
        pathpointElement->LinkEndChild(xml_io.CreateXMLVectorElement("tangent", pathPoint.tangent));
        pathpointElement->LinkEndChild(xml_io.CreateXMLVectorElement("rotation", pathPoint.rotation));
    }

}

