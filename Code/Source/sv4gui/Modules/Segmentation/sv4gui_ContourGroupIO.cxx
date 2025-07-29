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

#include "sv4gui_ContourGroupIO.h"
#include "sv4gui_ContourGroup.h"
#include "sv4gui_ContourCircle.h"
#include "sv4gui_ContourEllipse.h"
#include "sv4gui_ContourPolygon.h"
#include "sv4gui_ContourSplinePolygon.h"
#include "sv4gui_ContourTensionPolygon.h"
#include "sv4gui_XmlIOUtil.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

static mitk::CustomMimeType Createsv4guiContourGroupMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svcontourgroup");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("ctgr");
    mimeType.SetComment("SimVascular ContourGroup");

    return mimeType;
}

//---------------------------
// set_string_from_attribute
//---------------------------
// Get a string attribute from an XMLElement object.
//
void sv4guiContourGroupIO::set_string_from_attribute(tinyxml2::XMLElement* element, const char* attr_name, 
  std::string& value)
{
  const char* qvalue = "";
  element->QueryStringAttribute(attr_name, &qvalue);
  value = std::string(qvalue);
}

sv4guiContourGroupIO::sv4guiContourGroupIO()
    : mitk::AbstractFileIO(sv4guiContourGroup::GetStaticNameOfClass(), Createsv4guiContourGroupMimeType(), "SimVascular ContourGroup")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> sv4guiContourGroupIO::Read()
{
    std::string fileName = GetInputLocation();
    return ReadFile(fileName);
}

//----------
// ReadFile
//----------
//
std::vector<mitk::BaseData::Pointer> sv4guiContourGroupIO::ReadFile(std::string fileName)
{
    #define n_debug_ReadFile 
    #ifdef debug_ReadFile 
    std::string msg("[sv4guiContourGroupIO::ReadFile] ");
    std::cout << msg << "========== ReadFile ==========" << std::endl;
    std::cout << msg << "fileName: " << fileName << std::endl;
    #endif

    tinyxml2::XMLDocument document;
    auto group = CreateGroupFromFile(fileName);
    std::vector<mitk::BaseData::Pointer> result;
    result.push_back(group.GetPointer());
    return result;
}

//---------------------
// CreateGroupFromFile
//---------------------
// Create a group of contours (segmentations) from an XML format .ctgr file.
//
// This will set svLoftingParam parameters from the .ctgr file. The parameters 
// used for contour spline fitting, not lofting.
//
sv4guiContourGroup::Pointer
sv4guiContourGroupIO::CreateGroupFromFile(std::string fileName)
{
    #define n_debug_CreateGroupFromFile
    #ifdef debug_CreateGroupFromFile
    std::string msg("[sv4guiContourGroupIO::CreateGroupFromFile] ");
    std::cout << msg << "========== CreateGroupFromFile ==========" << std::endl;
    std::cout << msg << "fileName: " << fileName << std::endl;
    #endif

    tinyxml2::XMLDocument document;
    sv4guiContourGroup::Pointer group = sv4guiContourGroup::New();

    if (document.LoadFile(fileName.c_str()) != tinyxml2::XML_SUCCESS)
    {
        mitkThrow() << "Could not open/read/parse " << fileName;
        std::vector<mitk::BaseData::Pointer> empty;
        return group;
    }

    auto groupElement = document.FirstChildElement("contourgroup");

    if(!groupElement){
        mitkThrow() << "No ContourGroup data in "<< fileName;
    }

    group->SetPathName(groupElement->Attribute("path_name"));
    int pathID=0;
    groupElement->QueryIntAttribute("path_id",&pathID);
    group->SetPathID(pathID);

    double resliceSize=5.0;
    groupElement->QueryDoubleAttribute("reslice_size", &resliceSize);
    group->SetResliceSize(resliceSize);

    const char* point2dsize="";
    const char* point3dsize="";
    groupElement->QueryStringAttribute("point_2D_display_size", &point2dsize);
    groupElement->QueryStringAttribute("point_size", &point3dsize);
    group->SetProp("point 2D display size",point2dsize);
    group->SetProp("point size",point3dsize);

    int timestep=-1;

    for( auto timestepElement = groupElement->FirstChildElement("timestep"); timestepElement != nullptr;
         timestepElement = timestepElement->NextSiblingElement("timestep") )
    {
        if (timestepElement == nullptr) {
            continue;
        }

        timestep++;
        group->Expand(timestep+1);
        #ifdef debug_CreateGroupFromFile
        std::cout << msg << "timestep: " << timestep << std::endl;
        #endif

        // Set lofting parameters.
        //
        if(timestep==0)
        {
            auto loftParamElement = timestepElement->FirstChildElement("lofting_parameters");
            if(loftParamElement!=nullptr)
            {
                svLoftingParam* param = group->GetLoftingParam();

                set_string_from_attribute(loftParamElement, "method", param->method);
                // davep loftParamElement->QueryStringAttribute("method", &param->method);

                loftParamElement->QueryIntAttribute("sampling", &param->numOutPtsInSegs);
                loftParamElement->QueryIntAttribute("sample_per_seg",&param->samplePerSegment);
                loftParamElement->QueryIntAttribute("use_linear_sample",&param->useLinearSampleAlongLength);
                loftParamElement->QueryIntAttribute("linear_multiplier",&param->linearMuliplier);
                loftParamElement->QueryIntAttribute("use_fft",&param->useFFT);
                loftParamElement->QueryIntAttribute("num_modes",&param->numModes);

                loftParamElement->QueryIntAttribute("u_degree",&param->uDegree);
                loftParamElement->QueryIntAttribute("v_degree",&param->vDegree);

                set_string_from_attribute(loftParamElement, "u_knot_type", param->uKnotSpanType);
                // davep loftParamElement->QueryStringAttribute("u_knot_type",&param->uKnotSpanType);

                set_string_from_attribute(loftParamElement, "v_knot_type", param->vKnotSpanType);
                // davep loftParamElement->QueryStringAttribute("v_knot_type",&param->vKnotSpanType);

                set_string_from_attribute(loftParamElement, "u_parametric_type", param->uParametricSpanType);
                // davep loftParamElement->QueryStringAttribute("u_parametric_type",&param->uParametricSpanType);

                set_string_from_attribute(loftParamElement, "v_parametric_type", param->vParametricSpanType);
                // davep loftParamElement->QueryStringAttribute("v_parametric_type",&param->vParametricSpanType);
                #ifdef debug_CreateGroupFromFile
                std::cout << msg << "method: " << param->method << std::endl;
                std::cout << msg << "u_knot_type: " << param->uKnotSpanType << std::endl;
                #endif
            }
        }

        for( auto contourElement = timestepElement->FirstChildElement("contour");
             contourElement != nullptr;
             contourElement = contourElement->NextSiblingElement("contour") )
        {
            if (contourElement == nullptr)
                continue;

            std::string type;
            set_string_from_attribute(contourElement, "type", type);
            // davep contourElement->QueryStringAttribute("type", &type);

            sv4guiContour* contour;

            if(type=="Circle")
            {
                contour=new sv4guiContourCircle();
            }
            else if(type=="Ellipse")
            {
                contour=new sv4guiContourEllipse();
            }
            else if(type=="Polygon")
            {
                contour=new sv4guiContourPolygon();
            }
            else if(type=="SplinePolygon")
            {
                contour=new sv4guiContourSplinePolygon();
            }
            else if(type=="TensionPolygon")
            {
                contour=new sv4guiContourTensionPolygon();
            }
            else
            {
                contour=new sv4guiContour();
            }

            sv4guiContourEllipse* ce=dynamic_cast<sv4guiContourEllipse*>(contour);
            if(ce)
            {
                std::string asCircle;
                set_string_from_attribute(contourElement, "as_circle", asCircle);
                // davep contourElement->QueryStringAttribute("as_circle", &asCircle);
                ce->SetAsCircle(asCircle=="true"?true:false);
            }

            sv4guiContourTensionPolygon* ct=dynamic_cast<sv4guiContourTensionPolygon*>(contour);
            if(ct)
            {
                int subdivisionRounds=0;
                contourElement->QueryIntAttribute("subdivision_rounds", &subdivisionRounds);
                double tensionParam=0.0;
                contourElement->QueryDoubleAttribute("tension_param", &tensionParam);
                ct->SetSubdivisionRounds(subdivisionRounds);
                ct->SetTensionParameter(tensionParam);
            }

            int contourID;
            contourElement->QueryIntAttribute("id", &contourID);
            contour->SetContourID(contourID);

            std::string method;
            set_string_from_attribute(contourElement, "method", method);
            //contourElement->QueryStringAttribute("method", &method);

            contour->SetMethod(method);
            std::string closed;
            set_string_from_attribute(contourElement, "closed", closed);
            // davep contourElement->QueryStringAttribute("closed", &closed);
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
            contour->SetSubdivisionType( (sv4guiContour::SubdivisionType) subdivisionType);
            contour->SetSubdivisionNumber(subdivisionNumber);
            contour->SetSubdivisionSpacing(spacing);

            contour->SetPlaced();

            auto xml_io = sv4guiXmlIOUtil(document);

            //path point
            auto pathpointElement = contourElement->FirstChildElement("path_point");
            if(pathpointElement!=nullptr)
            {
                sv4guiPathElement::sv4guiPathPoint pathPoint;
                int id=0;
                pathpointElement->QueryIntAttribute("id", &id);
                pathPoint.id=id;
                pathPoint.pos = xml_io.GetPoint(pathpointElement->FirstChildElement("pos"));
                pathPoint.tangent = xml_io.GetVector(pathpointElement->FirstChildElement("tangent"));
                pathPoint.rotation = xml_io.GetVector(pathpointElement->FirstChildElement("rotation"));

                contour->SetPathPoint(pathPoint);
            }

            //control points without updating contour points
            auto controlpointsElement = contourElement->FirstChildElement("control_points");
            if(controlpointsElement!=nullptr)
            {
                std::vector<mitk::Point3D> controlPoints;
                for( auto pointElement = controlpointsElement->FirstChildElement("point");
                     pointElement != nullptr;
                     pointElement = pointElement->NextSiblingElement("point") )
                {
                    if (pointElement == nullptr)
                        continue;

                    controlPoints.push_back(xml_io.GetPoint(pointElement));
                }
                contour->SetControlPoints(controlPoints,false);
            }

            //contour points
            auto contourpointsElement = contourElement->FirstChildElement("contour_points");
            if(contourpointsElement!=nullptr)
            {
                std::vector<mitk::Point3D> contourPoints;
                for( auto pointElement = contourpointsElement->FirstChildElement("point");
                     pointElement != nullptr;
                     pointElement = pointElement->NextSiblingElement("point") )
                {
                    if (pointElement == nullptr)
                        continue;

                    contourPoints.push_back(xml_io.GetPoint(pointElement));
                }
                contour->SetContourPoints(contourPoints,false);
                contour->ContourPointsChanged(); // Calculate contour center.
            }

            group->InsertContour(-1,contour,timestep);
        } //contour

    }//timestep

    return group;;
}

mitk::IFileIO::ConfidenceLevel sv4guiContourGroupIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

//-------------
// WriteToFile
//-------------
// Write a sv4guiContourGroup to a file.
//
void sv4guiContourGroupIO::WriteToFile(const sv4guiContourGroup* group, const std::string& fileName)
{
    #define n_debug_WriteToFile 
    #ifdef debug_WriteToFile
    std::string msg("[sv4guiContourGroupIO::WriteToFile] ");
    std::cout << msg << "========== WriteToFile ==========" << std::endl;
    #endif

    tinyxml2::XMLDocument document;
    auto decl = document.NewDeclaration();
    document.LinkEndChild( decl );

    auto groupElement = document.NewElement("contourgroup");
    groupElement->SetAttribute("path_name", group->GetPathName().c_str());
    groupElement->SetAttribute("path_id", group->GetPathID());
    groupElement->SetAttribute("reslice_size", group->GetResliceSize());
    groupElement->SetAttribute("point_2D_display_size",group->GetProp("point 2D display size").c_str());
    groupElement->SetAttribute("point_size",group->GetProp("point size").c_str());
    groupElement->SetAttribute("version",  "1.0" );
    document.LinkEndChild(groupElement);

    for(int t=0;t<group->GetTimeSize();t++) {
        auto timestepElement = document.NewElement("timestep");
        timestepElement->SetAttribute("id",t);
        groupElement->LinkEndChild(timestepElement);

        if (t==0) {
            auto loftParamElement = document.NewElement("lofting_parameters");
            timestepElement->LinkEndChild(loftParamElement);
            svLoftingParam* param=group->GetLoftingParam();
            loftParamElement->SetAttribute("method",param->method.c_str());

            loftParamElement->SetAttribute("sampling",param->numOutPtsInSegs);
            loftParamElement->SetAttribute("sample_per_seg",param->samplePerSegment);
            loftParamElement->SetAttribute("use_linear_sample",param->useLinearSampleAlongLength);
            loftParamElement->SetAttribute("linear_multiplier",param->linearMuliplier);
            loftParamElement->SetAttribute("use_fft",param->useFFT);
            loftParamElement->SetAttribute("num_modes",param->numModes);

            loftParamElement->SetAttribute("u_degree",param->uDegree);
            loftParamElement->SetAttribute("v_degree",param->vDegree);
            loftParamElement->SetAttribute("u_knot_type",param->uKnotSpanType.c_str());
            loftParamElement->SetAttribute("v_knot_type",param->vKnotSpanType.c_str());
            loftParamElement->SetAttribute("u_parametric_type",param->uParametricSpanType.c_str());
            loftParamElement->SetAttribute("v_parametric_type",param->vParametricSpanType.c_str());
        }

        auto xml_io = sv4guiXmlIOUtil(document);

        for(int i=0;i<group->GetSize(t);i++) {
            auto contour = group->GetContour(i,t);
            if (!contour) {
                continue;
            }

            auto contourElement = document.NewElement("contour");
            timestepElement->LinkEndChild(contourElement);
            std::string type=contour->GetType();
            contourElement->SetAttribute("id",i);
            contourElement->SetAttribute("type",type.c_str());
            contourElement->SetAttribute("method",contour->GetMethod().c_str());
            contourElement->SetAttribute("closed",contour->IsClosed()?"true":"false");
            contourElement->SetAttribute("min_control_number",contour->GetMinControlPointNumber());
            contourElement->SetAttribute("max_control_number",contour->GetMaxControlPointNumber());
            contourElement->SetAttribute("subdivision_type",contour->GetSubdivisionType());
            contourElement->SetAttribute("subdivision_number",contour->GetSubdivisionNumber());
            contourElement->SetAttribute("subdivision_spacing",contour->GetSubdivisionSpacing());

            auto ce = dynamic_cast<sv4guiContourEllipse*>(contour);
            if(ce) {
                contourElement->SetAttribute("as_circle",ce->AsCircle()?"true":"false");
            }

            auto ct = dynamic_cast<sv4guiContourTensionPolygon*>(contour);
            if(ct) {
                contourElement->SetAttribute("subdivision_rounds",ct->GetSubdivisionRounds());
                contourElement->SetAttribute("tension_param",ct->GetTensionParameter());
            }

            //path point
            auto pathpointElement = document.NewElement("path_point");
            contourElement->LinkEndChild(pathpointElement);
            pathpointElement->SetAttribute("id",contour->GetPathPoint().id);

            // [DaveP] careful with these calls, have CreateXMLPointElement() in sv3_XmlIOUtil.h and sv4gui_XmlIOUtil.h.
            pathpointElement->LinkEndChild(xml_io.CreateXMLPointElement("pos",contour->GetPathPoint().pos));
            pathpointElement->LinkEndChild(xml_io.CreateXMLVectorElement("tangent",contour->GetPathPoint().tangent));
            pathpointElement->LinkEndChild(xml_io.CreateXMLVectorElement("rotation",contour->GetPathPoint().rotation));
            //pathpointElement->LinkEndChild(sv4guiXmlIOUtil::CreateXMLPointElement("pos",contour->GetPathPoint().pos));
            //pathpointElement->LinkEndChild(sv4guiXmlIOUtil::CreateXMLVectorElement("tangent",contour->GetPathPoint().tangent));
            //pathpointElement->LinkEndChild(sv4guiXmlIOUtil::CreateXMLVectorElement("rotation",contour->GetPathPoint().rotation));

            //control points
            auto controlpointsElement = document.NewElement("control_points");
            contourElement->LinkEndChild(controlpointsElement);
            for(int j=0;j<contour->GetControlPointNumber();j++) {
                controlpointsElement->LinkEndChild(xml_io.CreateXMLPointElement("point",j,contour->GetControlPoint(j)));
                //controlpointsElement->LinkEndChild(sv4guiXmlIOUtil::CreateXMLPointElement("point",j,contour->GetControlPoint(j)));
            }

            //contour points
            auto  contourpointsElement = document.NewElement("contour_points");
            contourElement->LinkEndChild(contourpointsElement);
            for(int j=0;j<contour->GetContourPointNumber();j++) {
                contourpointsElement->LinkEndChild(xml_io.CreateXMLPointElement("point",j,contour->GetContourPoint(j)));
                // davep contourpointsElement->LinkEndChild(sv4guiXmlIOUtil::CreateXMLPointElement("point",j,contour->GetContourPoint(j)));
            }

        }
    }

    if (document.SaveFile(fileName.c_str()) != tinyxml2::XML_SUCCESS) {
        mitkThrow() << "Could not write contourgroup to the file " << fileName;
    }
}

//-------
// Write
//-------
// Write the sv4guiContourGroup associiated with the object to a file.
//
void sv4guiContourGroupIO::Write()
{
    ValidateOutputLocation();

    const sv4guiContourGroup* group = dynamic_cast<const sv4guiContourGroup*>(this->GetInput());

    if (!group) {
        return;
    }

    std::string fileName = GetOutputLocation();
    WriteToFile(group, fileName);
}

mitk::IFileIO::ConfidenceLevel sv4guiContourGroupIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const sv4guiContourGroup* input = dynamic_cast<const sv4guiContourGroup*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

sv4guiContourGroupIO* sv4guiContourGroupIO::IOClone() const
{
    return new sv4guiContourGroupIO(*this);
}

