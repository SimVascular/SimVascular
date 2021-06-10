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

#include "sv4gui_PathIO.h"
#include "sv4gui_Path.h"
#include "sv4gui_XmlIOUtil.h"
#include "sv3_PathIO.h"
#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

static mitk::CustomMimeType Createsv4guiPathMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svpath");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("pth");
    mimeType.SetComment("SimVascular Path");

    return mimeType;
}

sv4guiPathIO::sv4guiPathIO()
    : mitk::AbstractFileIO(sv4guiPath::GetStaticNameOfClass(), Createsv4guiPathMimeType(), "SimVascular Path")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> sv4guiPathIO::Read()
{
    std::string fileName=GetInputLocation();

    return ReadFile(fileName);
}

std::vector<mitk::BaseData::Pointer> sv4guiPathIO::ReadFile(std::string fileName)
{
    TiXmlDocument document;

    if (!document.LoadFile(fileName))
    {
        mitkThrow() << "Could not open/read/parse " << fileName;
        std::vector<mitk::BaseData::Pointer> empty;
        return empty;
    }

    TiXmlElement* pathElement = document.FirstChildElement("path");

    if(!pathElement){
        mitkThrow() << "No path data in "<< fileName;
    }

    sv4guiPath::Pointer path = sv4guiPath::New();
    sv3::PathIO* reader = new sv3::PathIO();
    sv3::PathGroup* svPathGrp = reader->ReadFile(fileName);
    delete reader;
    
    path->SetPathID(svPathGrp->GetPathID());
    path->SetMethod(svPathGrp->GetMethod());
    path->SetCalculationNumber(svPathGrp->GetCalculationNumber());
    path->SetSpacing(svPathGrp->GetSpacing());
    
    for (int i=0; i<svPathGrp->GetTimeSize(); i++)
        path->SetPathElement(static_cast<sv4guiPathElement*>(svPathGrp->GetPathElement(i)),i);
        
    //only for GUI
    double resliceSize=5.0;
    pathElement->QueryDoubleAttribute("reslice_size", &resliceSize);
    path->SetResliceSize(resliceSize);

    std::string point2dsize="",point3dsize="";
    pathElement->QueryStringAttribute("point_2D_display_size", &point2dsize);
    pathElement->QueryStringAttribute("point_size", &point3dsize);
    path->SetProp("point 2D display size",point2dsize);
    path->SetProp("point size",point3dsize);

    std::vector<mitk::BaseData::Pointer> result;
    result.push_back(path.GetPointer());
    delete svPathGrp;
    return result;
}

mitk::IFileIO::ConfidenceLevel sv4guiPathIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void sv4guiPathIO::Write()
{
    ValidateOutputLocation();

    const sv4guiPath* path = dynamic_cast<const sv4guiPath*>(this->GetInput());
    if(!path) return;
    
    TiXmlDocument document;
    auto  decl = new TiXmlDeclaration( "1.0", "UTF-8", "" );
    document.LinkEndChild( decl );

    auto  pathElement = new TiXmlElement("path");
    pathElement->SetAttribute("id", path->GetPathID());
    pathElement->SetAttribute("method", path->GetMethod());
    pathElement->SetAttribute("calculation_number", path->GetCalculationNumber());
    pathElement->SetDoubleAttribute("spacing", path->GetSpacing());
    pathElement->SetAttribute("version",  "1.0" );
    
    //only for GUI
    pathElement->SetDoubleAttribute("reslice_size", path->GetResliceSize());
    pathElement->SetAttribute("point_2D_display_size",path->GetProp("point 2D display size"));
    pathElement->SetAttribute("point_size",path->GetProp("point size"));
    document.LinkEndChild(pathElement);

    for(int t=0;t<path->GetTimeSize();t++)
    {
        auto  timestepElement = new TiXmlElement("timestep");
        timestepElement->SetAttribute("id",t);
        pathElement->LinkEndChild(timestepElement);

        sv4guiPathElement* pe=path->GetPathElement(t);
        if(!pe) continue;

        sv3::PathElement* svPe=static_cast<sv3::PathElement*>(pe);

        this->sv3::PathIO::WritePath(svPe,timestepElement); 
    }

    std::string fileName=GetOutputLocation();
    if (document.SaveFile(fileName) == false)
    {
        mitkThrow() << "Could not write path to " << fileName;

    }
}

mitk::IFileIO::ConfidenceLevel sv4guiPathIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const sv4guiPath* input = dynamic_cast<const sv4guiPath*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

sv4guiPathIO* sv4guiPathIO::IOClone() const
{
    return new sv4guiPathIO(*this);
}
