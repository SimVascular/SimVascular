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

#include "sv4gui_MitkSeg3DIO.h"
#include "sv4gui_MitkSeg3D.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include <tinyxml.h>

#include <vtkPolyData.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkErrorCode.h>

static mitk::CustomMimeType Createsv4guiSeg3DMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svseg3d");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("s3d");
    mimeType.SetComment("SimVascular 3D Segmentation");

    return mimeType;
}

sv4guiMitkSeg3DIO::sv4guiMitkSeg3DIO()
    : mitk::AbstractFileIO(sv4guiMitkSeg3D::GetStaticNameOfClass(), Createsv4guiSeg3DMimeType(), "SimVascular 3D Segmentation")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> sv4guiMitkSeg3DIO::Read()
{
    std::string fileName=GetInputLocation();

    return ReadFile(fileName);
}

std::vector<mitk::BaseData::Pointer> sv4guiMitkSeg3DIO::ReadFile(std::string fileName)
{
    std::vector<mitk::BaseData::Pointer> result;

    TiXmlDocument document;

    if (!document.LoadFile(fileName))
    {
        mitkThrow() << "Could not open/read/parse " << fileName;
        //        return result;
    }

    //    TiXmlElement* version = document.FirstChildElement("format");

    TiXmlElement* segElement = document.FirstChildElement("seg3d");

    if(!segElement){
        mitkThrow() << "No 3D seg data in "<< fileName;
        //        return result;
    }

    sv4guiMitkSeg3D::Pointer mitkSeg3D = sv4guiMitkSeg3D::New();
    sv4guiSeg3D* seg3D=new sv4guiSeg3D();
    sv4guiSeg3DParam param;

    //read parameters
    TiXmlElement* paramElement = segElement->FirstChildElement("param");
    if(paramElement)
    {
        paramElement->QueryStringAttribute("method",&param.method);
        if(param.method!="")
        {
            paramElement->QueryDoubleAttribute("lower_threshold",&param.lowerThreshold);
            paramElement->QueryDoubleAttribute("upper_threshold",&param.upperThreshold);

            //read seeds
            TiXmlElement* seedsElement = paramElement->FirstChildElement("seeds");
            if(seedsElement)
            {
                for( TiXmlElement* seedElement = seedsElement->FirstChildElement("seed");
                     seedElement != nullptr;
                     seedElement = seedElement->NextSiblingElement("seed") )
                {
                    if (seedsElement == nullptr)
                        continue;

                    svSeed seed;

                    seedElement->QueryIntAttribute("id", &seed.id);
                    seedElement->QueryStringAttribute("type", &seed.type);
                    seedElement->QueryDoubleAttribute("x", &seed.x);
                    seedElement->QueryDoubleAttribute("y", &seed.y);
                    seedElement->QueryDoubleAttribute("z", &seed.z);
                    seedElement->QueryDoubleAttribute("radius", &seed.radius);

                    param.AddSeed(seed);
                }
            }
        }
    }

    seg3D->SetParam(param);
    std::string dataFileName=fileName.substr(0,fileName.find_last_of("."))+".vtp";
    std::ifstream dataFile(dataFileName);
    if (dataFile) {
        vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();

        reader->SetFileName(dataFileName.c_str());
        reader->Update();
        vtkSmartPointer<vtkPolyData> vpd=reader->GetOutput();
        if(vpd)
            seg3D->SetVtkPolyData(vpd);
    }

    mitkSeg3D->SetSeg3D(seg3D);

    result.push_back(mitkSeg3D.GetPointer());
    return result;
}

mitk::IFileIO::ConfidenceLevel sv4guiMitkSeg3DIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void sv4guiMitkSeg3DIO::Write()
{
    ValidateOutputLocation();

    std::string fileName=GetOutputLocation();

    const sv4guiMitkSeg3D* mitkSeg3D = dynamic_cast<const sv4guiMitkSeg3D*>(this->GetInput());
    if(!mitkSeg3D) return;

    TiXmlDocument document;
    auto  decl = new TiXmlDeclaration( "1.0", "UTF-8", "" );
    document.LinkEndChild( decl );

    sv4guiSeg3D* seg3D=mitkSeg3D->GetSeg3D();
    if(seg3D)
    {
        sv4guiSeg3DParam& param=seg3D->GetParam();

        auto  segElement = new TiXmlElement("seg3d");
        segElement->SetAttribute("version",  "1.0" );
        document.LinkEndChild(segElement);

        auto  paramElement = new TiXmlElement("param");
        segElement->LinkEndChild(paramElement);

        if(param.method!="")
        {
            paramElement->SetAttribute("method", param.method);

            paramElement->SetDoubleAttribute("lower_threshold", param.lowerThreshold);
            paramElement->SetDoubleAttribute("upper_threshold", param.upperThreshold);


            auto  seedsElement = new TiXmlElement("seeds");
            paramElement->LinkEndChild(seedsElement);

            std::map<int, svSeed>& seedMap=param.GetSeedMap();
            for(auto s:seedMap)
            {
                auto  seedElement = new TiXmlElement("seed");
                svSeed seed=s.second;
                seedsElement->LinkEndChild(seedElement);
                seedElement->SetAttribute("id",seed.id);
                seedElement->SetAttribute("type", seed.type);
                seedElement->SetDoubleAttribute("x", seed.x);
                seedElement->SetDoubleAttribute("y", seed.y);
                seedElement->SetDoubleAttribute("z", seed.z);
                seedElement->SetDoubleAttribute("radius", seed.radius);
            }
        }

        std::string dataFileName=fileName.substr(0,fileName.find_last_of("."))+".vtp";

        vtkPolyData* vpd=seg3D->GetVtkPolyData();
        if(vpd)
        {
            vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
            writer->SetFileName(dataFileName.c_str());
            writer->SetInputData(vpd);
            if (writer->Write() == 0 || writer->GetErrorCode() != 0 )
            {
                std::cerr << "vtkXMLPolyDataWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode())<<std::endl;
            }
        }
    }

    if (document.SaveFile(fileName) == false)
    {
        mitkThrow() << "Could not write model to " << fileName;

    }
}

mitk::IFileIO::ConfidenceLevel sv4guiMitkSeg3DIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const sv4guiMitkSeg3D* input = dynamic_cast<const sv4guiMitkSeg3D*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

sv4guiMitkSeg3DIO* sv4guiMitkSeg3DIO::IOClone() const
{
    return new sv4guiMitkSeg3DIO(*this);
}

