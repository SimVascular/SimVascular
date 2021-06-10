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

// The methods defined here are used to read/write SV XML FSI project .fsijob files.

#include "sv4gui_MitksvFSIJobIO.h"

#include "sv4gui_MitksvFSIJob.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include <tinyxml.h>

#include <fstream>

static mitk::CustomMimeType CreatesvSimJobMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".fsijob");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("fsijob");
    mimeType.SetComment("svFSI Job");

    return mimeType;
}

sv4guiMitksvFSIJobIO::sv4guiMitksvFSIJobIO()
    : mitk::AbstractFileIO(sv4guiMitksvFSIJob::GetStaticNameOfClass(), CreatesvSimJobMimeType(), "sv4guisvFSI Job")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> sv4guiMitksvFSIJobIO::Read()
{
    std::vector<mitk::BaseData::Pointer> result;

    TiXmlDocument document;

    std::string fileName=GetInputLocation();

    if (!document.LoadFile(fileName))
    {
        mitkThrow() << "Could not open/read/parse " << fileName;
        //        MITK_ERROR << "Could not open/read/parse " << fileName;
        return result;
    }

    //    TiXmlElement* version = document.FirstChildElement("format");

    TiXmlElement* mjElement = document.FirstChildElement("mitk_job");

    if(!mjElement){
        //        MITK_ERROR << "No job data in "<< fileName;
        mitkThrow() << "No job data in "<< fileName;
        return result;
    }

    sv4guiMitksvFSIJob::Pointer mitkSimJob = sv4guiMitksvFSIJob::New();
    std::string modelName="";
    std::string meshName="";
    std::string status="";
    int procNum=1;
    mjElement->QueryStringAttribute("model_name",&modelName);
    mjElement->QueryStringAttribute("mesh_name",&meshName);
    mjElement->QueryStringAttribute("status",&status);
    mjElement->QueryIntAttribute("process_number",&procNum);

    mitkSimJob->SetModelName(modelName);
    mitkSimJob->SetMeshName(meshName);
    mitkSimJob->SetStatus(status);
    mitkSimJob->SetProcessNumber(procNum);

    std::string strValue="";

    TiXmlElement* jobElement = mjElement->FirstChildElement("job");

    if (jobElement != nullptr) {
        sv4guisvFSIJob* job = new sv4guisvFSIJob();

        jobElement->QueryIntAttribute("nsd",&job->nsd);
        jobElement->QueryIntAttribute("timeSteps",&job->timeSteps);
        jobElement->QueryStringAttribute("stepSize",&job->stepSize);
        jobElement->QueryBoolAttribute("continuePrevious",&job->continuePrevious);

        jobElement->QueryStringAttribute("restartFileName", &job->restartFileName);
        jobElement->QueryIntAttribute("restartInc", &job->restartInc);

        jobElement->QueryBoolAttribute("vtkSaveResults", &job->vtkSaveResults);
        jobElement->QueryStringAttribute("vtkFileName", &job->vtkFileName);
        jobElement->QueryIntAttribute("vtkInc", &job->vtkInc);

        jobElement->QueryIntAttribute("startSavingStep",&job->startSavingStep);
        jobElement->QueryBoolAttribute("saveAvgResult",&job->saveAvgResult);
        jobElement->QueryDoubleAttribute("rhoInf",&job->rhoInf);
        jobElement->QueryStringAttribute("stopFileName",&job->stopFileName);
        jobElement->QueryBoolAttribute("remeshing",&job->remeshing);
        jobElement->QueryBoolAttribute("verbose",&job->verbose);
        jobElement->QueryBoolAttribute("warn",&job->warn);
        jobElement->QueryBoolAttribute("debug",&job->debug);

        TiXmlElement* dsElement = jobElement->FirstChildElement("domains");
        if(dsElement != nullptr)
        {
            for( TiXmlElement* domainElement = dsElement->FirstChildElement("domain");
                 domainElement != nullptr;
                 domainElement =domainElement->NextSiblingElement("domain") )
            {
//                if (domainElement == nullptr)
//                    continue;

                sv4guisvFSIDomain domain;
                domainElement->QueryStringAttribute("name", &domain.name);
                domainElement->QueryStringAttribute("type", &domain.type);
                domainElement->QueryStringAttribute("folder_name", &domain.folderName);
                domainElement->QueryStringAttribute("file_name", &domain.fileName);
                domainElement->QueryStringAttribute("surface_name", &domain.surfaceName);
                domainElement->QueryStringAttribute("face_folder_name", &domain.faceFolderName);

                for(TiXmlElement* faceElement=domainElement->FirstChildElement("face");
                    faceElement !=nullptr;
                    faceElement = faceElement->NextSiblingElement("face"))
                {
                    std::string faceName="";
                    faceElement->QueryStringAttribute("name",&faceName);
                    domain.faceNames.push_back(faceName);
                }

                job->m_Domains[domain.name]=domain;

            }
        }

        TiXmlElement* eqsElement = jobElement->FirstChildElement("equations");
        if(eqsElement != nullptr)
        {
            for( TiXmlElement* eqElement = eqsElement->FirstChildElement("equation");
                 eqElement != nullptr;
                 eqElement =eqElement->NextSiblingElement("equation") )
            {
                strValue="";
                eqElement->QueryStringAttribute("physName", &strValue);
                if(strValue=="" || strValue=="none")
                    continue;

                sv4guisvFSIeqClass eq=sv4guisvFSIeqClass(QString::fromStdString(strValue));

                eqElement->QueryBoolAttribute("coupled", &eq.coupled);
                eqElement->QueryIntAttribute("minItr", &eq.minItr);
                eqElement->QueryIntAttribute("maxItr", &eq.maxItr);
                strValue="";
                eqElement->QueryStringAttribute("tol", &strValue);
                eq.tol=QString::fromStdString(strValue);
                eqElement->QueryDoubleAttribute("dBr", &eq.dBr);
                eqElement->QueryDoubleAttribute("backflow", &eq.backflowStab);

                TiXmlElement* propsElement=eqElement->FirstChildElement("props");
                if(propsElement!=nullptr)
                {
                    for(TiXmlElement* propElement=propsElement->FirstChildElement("prop");
                        propElement !=nullptr;
                        propElement = propElement->NextSiblingElement("prop"))
                    {
                        strValue="";
                        propElement->QueryStringAttribute("name",&strValue);
                        std::string strValue2="";
                        propElement->QueryStringAttribute("value",&strValue2);

                        if(strValue=="Constitutive model")
                        {
                            eq.constitutiveModel=QString::fromStdString(strValue2);
                            continue;
                        }

                        eq.setPropValue(QString::fromStdString(strValue2).toDouble(),QString::fromStdString(strValue));
                    }
                }

                TiXmlElement* lsElement=eqElement->FirstChildElement("linear_solver");
                if(lsElement!=nullptr)
                {
                    strValue="";
                    lsElement->QueryStringAttribute("lsType",&strValue);
                    eq.lsType=QString::fromStdString(strValue);

                    strValue="";
                    lsElement->QueryStringAttribute("lsPreconditioner",&strValue);
                    eq.lsPreconditioner=QString::fromStdString(strValue);

                    lsElement->QueryIntAttribute("lsMaxItr",&eq.lsMaxItr);

                    strValue="";
                    lsElement->QueryStringAttribute("lsTol",&strValue);
                    eq.lsTol=QString::fromStdString(strValue);

                    lsElement->QueryIntAttribute("lsNSGMMaxItr",&eq.lsNSGMMaxItr);

                    strValue="";
                    lsElement->QueryStringAttribute("lsNSGMTol",&strValue);
                    eq.lsNSGMTol=QString::fromStdString(strValue);

                    lsElement->QueryIntAttribute("lsNSCGMaxItr",&eq.lsNSCGMaxItr);

                    strValue="";
                    lsElement->QueryStringAttribute("lsNSCGTol",&strValue);
                    eq.lsNSCGTol=QString::fromStdString(strValue);

                    strValue="";
                    lsElement->QueryStringAttribute("lsAbsoluteTol",&strValue);
                    eq.lsAbsoluteTol=QString::fromStdString(strValue);

                    lsElement->QueryIntAttribute("lsKrylovDim",&eq.lsKrylovDim);

                    strValue="";
                    lsElement->QueryStringAttribute("lsPreconditioner",&strValue);
                    eq.lsPreconditioner=QString::fromStdString(strValue);
                }

                TiXmlElement* rmElement=eqElement->FirstChildElement("remesher");
                if(rmElement!=nullptr)
                {
                    strValue="";
                    rmElement->QueryStringAttribute("name",&strValue);
                    eq.remesher=QString::fromStdString(strValue);

                    rmElement->QueryDoubleAttribute("angle",&eq.rmMinAngle);
                    rmElement->QueryDoubleAttribute("ratio",&eq.rmMaxRadiusRatio);
                    rmElement->QueryIntAttribute("frequency",&eq.rmFrequency);
                    rmElement->QueryIntAttribute("copy_frequency",&eq.rmCopyFrequency);

                    for(TiXmlElement* sizeElement=rmElement->FirstChildElement("edge_size");
                        sizeElement !=nullptr;
                        sizeElement = sizeElement->NextSiblingElement("edge_size"))
                    {
                        strValue="";
                        sizeElement->QueryStringAttribute("domain",&strValue);

                        if(job->m_Domains.find(strValue)==job->m_Domains.end())
                            continue;

                        sizeElement->QueryDoubleAttribute("size",&(job->m_Domains[strValue].edgeSize));
                    }
                }

                TiXmlElement* outputsElement=eqElement->FirstChildElement("outputs");
                if(outputsElement!=nullptr)
                {
                    TiXmlElement* spatialElement=outputsElement->FirstChildElement("spatial");
                    if(spatialElement!=nullptr)
                    {
                        for(TiXmlElement* oElement=spatialElement->FirstChildElement("output");
                            oElement !=nullptr;
                            oElement = oElement->NextSiblingElement("output"))
                        {
                            strValue="";
                            oElement->QueryStringAttribute("name",&strValue);
                            QString outputName=QString::fromStdString(strValue);
                            eq.setOutput(outputName,true);
                        }
                    }
                }

                TiXmlElement* bcsElement=eqElement->FirstChildElement("boundary_conditions");
                if(bcsElement!=nullptr)
                {
                    for(TiXmlElement* faceElement=bcsElement->FirstChildElement("face");
                        faceElement!=nullptr;
                        faceElement=faceElement->NextSiblingElement("face"))
                    {
                        sv4guisvFSIbcClass bc;

                        strValue="";
                        faceElement->QueryStringAttribute("name",&strValue);
                        bc.faceName=QString::fromStdString(strValue);

                        strValue="";
                        faceElement->QueryStringAttribute("group",&strValue);
                        bc.bcGrp=QString::fromStdString(strValue);

                        strValue="";
                        faceElement->QueryStringAttribute("type",&strValue);
                        bc.bcType=QString::fromStdString(strValue);

                        if ( bc.bcType == "Steady" )
                            faceElement->QueryDoubleAttribute("value",&bc.g);
                        else if ( bc.bcType == "Unsteady" )
                        {
                            strValue="";
                            faceElement->QueryStringAttribute("file",&strValue);
                            bc.gtFile=QString::fromStdString(strValue);
                        }
                        else if ( bc.bcType == "Resistance" )
                            faceElement->QueryDoubleAttribute("value",&bc.r);
                        else if ( bc.bcType == "General" )
                        {
                            strValue="";
                            faceElement->QueryStringAttribute("file",&strValue);
                            bc.gmFile=QString::fromStdString(strValue);
                        }
                        else if ( bc.bcType == "Projection" )
                        {
                            strValue="";
                            faceElement->QueryStringAttribute("projection",&strValue);
                            bc.projectionFaceName=QString::fromStdString(strValue);
                        }

                        strValue="";
                        faceElement->QueryStringAttribute("profile",&strValue);
                        bc.profile=QString::fromStdString(strValue);

                        if(bc.profile== "User_defined" )
                        {
                            strValue="";
                            faceElement->QueryStringAttribute("profile_file",&strValue);
                            bc.gxFile=QString::fromStdString(strValue);
                        }

                        faceElement->QueryBoolAttribute("zperm",&bc.zperm);
                        faceElement->QueryBoolAttribute("flux",&bc.flux);

                        faceElement->QueryBoolAttribute("impose_integral",&bc.imposeIntegral);
                        strValue="";
                        faceElement->QueryStringAttribute("effective_direction",&strValue);
                        bc.effectiveDirection=QString::fromStdString(strValue);

                        eq.faceBCs[bc.faceName.toStdString()]=bc;
                    }

                }

                job->m_Eqs.push_back(eq);

            }
        }

        mitkSimJob->SetSimJob(job);
        mitkSimJob->SetDataModified(false);//avoid saving again
    } //job

    result.push_back(mitkSimJob.GetPointer());
    return result;
}

mitk::IFileIO::ConfidenceLevel sv4guiMitksvFSIJobIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void sv4guiMitksvFSIJobIO::Write()
{
    ValidateOutputLocation();

    std::string fileName=GetOutputLocation();

    const sv4guiMitksvFSIJob* mitkSimJob = dynamic_cast<const sv4guiMitksvFSIJob*>(this->GetInput());
    if(!mitkSimJob) return;

    TiXmlDocument document;
    auto  decl = new TiXmlDeclaration( "1.0", "UTF-8", "" );
    document.LinkEndChild( decl );

    auto  mjElement = new TiXmlElement("mitk_job");
    mjElement->SetAttribute("model_name", mitkSimJob->GetModelName());
    mjElement->SetAttribute("mesh_name", mitkSimJob->GetMeshName());
    mjElement->SetAttribute("status", mitkSimJob->GetStatus());
    mjElement->SetAttribute("process_number", mitkSimJob->GetProcessNumber());
    mjElement->SetAttribute("version",  "1.0" );
    document.LinkEndChild(mjElement);

    sv4guisvFSIJob* job = mitkSimJob->GetSimJob();

    if (job) {
        auto jobElement = new TiXmlElement("job");
        mjElement->LinkEndChild(jobElement);

        jobElement->SetAttribute("nsd",job->nsd);
        jobElement->SetAttribute("timeSteps",job->timeSteps);
        jobElement->SetAttribute("stepSize",job->stepSize);
        jobElement->SetAttribute("continuePrevious",job->continuePrevious?"true":"false");

        jobElement->SetAttribute("restartFileName", job->restartFileName);
        jobElement->SetAttribute("restartInc", job->restartInc);

        jobElement->SetAttribute("vtkSaveResults", job->vtkSaveResults);
        jobElement->SetAttribute("vtkFileName", job->vtkFileName);
        jobElement->SetAttribute("vtkInc", job->vtkInc);

        jobElement->SetAttribute("startSavingStep",job->startSavingStep);
        jobElement->SetAttribute("saveAvgResult",job->saveAvgResult?"true":"false");
        jobElement->SetDoubleAttribute("rhoInf",job->rhoInf);
        jobElement->SetAttribute("stopFileName",job->stopFileName);
        jobElement->SetAttribute("remeshing",job->remeshing?"true":"false");
        jobElement->SetAttribute("verbose",job->verbose?"true":"false");
        jobElement->SetAttribute("warn",job->warn?"true":"false");
        jobElement->SetAttribute("debug",job->debug?"true":"false");

        auto dsElement =new TiXmlElement("domains");
        jobElement->LinkEndChild(dsElement);

        for(auto& md : job->m_Domains) {
            sv4guisvFSIDomain& domain=md.second;

            auto domainElement = new TiXmlElement("domain");
            dsElement->LinkEndChild(domainElement);

            domainElement->SetAttribute("name", domain.name);
            domainElement->SetAttribute("type", domain.type);
            domainElement->SetAttribute("folder_name", domain.folderName);
            domainElement->SetAttribute("file_name",domain.fileName);
            domainElement->SetAttribute("surface_name",domain.surfaceName);
            domainElement->SetAttribute("face_folder_name",domain.faceFolderName);

            for(std::string faceName : domain.faceNames)
            {
                auto faceElement = new TiXmlElement("face");
                domainElement->LinkEndChild(faceElement);
                faceElement->SetAttribute("name", faceName);
            }
        }

        auto eqsElement =new TiXmlElement("equations");
        jobElement->LinkEndChild(eqsElement);

        for(sv4guisvFSIeqClass & eq : job->m_Eqs) {
            auto eqElement = new TiXmlElement("equation");
            eqsElement->LinkEndChild(eqElement);

            eqElement->SetAttribute("physName",eq.physName.toStdString());
            eqElement->SetAttribute("coupled",eq.coupled?"true":"false");
            eqElement->SetAttribute("minItr",eq.minItr);
            eqElement->SetAttribute("maxItr",eq.maxItr);
            eqElement->SetAttribute("tol",eq.tol.toStdString());
            eqElement->SetAttribute("dBr",eq.dBr);
            eqElement->SetDoubleAttribute("backflow",eq.backflowStab);

            auto propsElement =new TiXmlElement("props");
            eqElement->LinkEndChild(propsElement);
            for(int i=0;i<eq.getPropCount();++i)
            {
                auto propElement =new TiXmlElement("prop");
                propsElement->LinkEndChild(propElement);

                propElement->SetAttribute("name",eq.getPropName(i).toStdString());
                propElement->SetDoubleAttribute("value",eq.getPropValue(i));
            }

            if(eq.physName=="FSI" || eq.physName=="struct")
            {
                auto propElement =new TiXmlElement("prop");
                propsElement->LinkEndChild(propElement);

                propElement->SetAttribute("name","Constitutive model");
                propElement->SetAttribute("value",eq.constitutiveModel.toStdString());
            }

            auto lsElement =new TiXmlElement("linear_solver");
            eqElement->LinkEndChild(lsElement);
            lsElement->SetAttribute("lsType",eq.lsType.toStdString());
            lsElement->SetAttribute("lsMaxItr",eq.lsMaxItr);
            lsElement->SetAttribute("lsTol",eq.lsTol.toStdString());
            lsElement->SetAttribute("lsNSGMMaxItr",eq.lsNSGMMaxItr);
            lsElement->SetAttribute("lsNSGMTol",eq.lsNSGMTol.toStdString());
            lsElement->SetAttribute("lsNSCGMaxItr",eq.lsNSCGMaxItr);
            lsElement->SetAttribute("lsNSCGTol",eq.lsNSCGTol.toStdString());
            lsElement->SetAttribute("lsAbsoluteTol",eq.lsAbsoluteTol.toStdString());
            lsElement->SetAttribute("lsKrylovDim",eq.lsKrylovDim);
            lsElement->SetAttribute("lsPreconditioner",eq.lsPreconditioner.toStdString());

            if(eq.physName=="FSI" && eq.remesher!="None")
            {
                auto rmElement =new TiXmlElement("remesher");
                eqElement->LinkEndChild(rmElement);
                rmElement->SetAttribute("name",eq.remesher.toStdString());
                rmElement->SetDoubleAttribute("angle",eq.rmMinAngle);
                rmElement->SetDoubleAttribute("ratio",eq.rmMaxRadiusRatio);
                rmElement->SetAttribute("frequency",eq.rmFrequency);
                rmElement->SetAttribute("copy_frequency",eq.rmCopyFrequency);

                for(auto& pair : job->m_Domains)
                {
                    std::string domainName=pair.first;
                    sv4guisvFSIDomain& domain=pair.second;

                    auto sizeElement =new TiXmlElement("edge_size");
                    rmElement->LinkEndChild(sizeElement);
                    sizeElement->SetAttribute("domain",domainName);
                    sizeElement->SetDoubleAttribute("size",domain.edgeSize);
                }
            }

            auto outputsElement=new TiXmlElement("outputs");
            eqElement->LinkEndChild(outputsElement);

            auto spatialElement=new TiXmlElement("spatial");
            outputsElement->LinkEndChild(spatialElement);
            foreach ( QString outName , eq.getOutputNames() )
            {
                auto oElement=new TiXmlElement("output");
                spatialElement->LinkEndChild(oElement);
                oElement->SetAttribute("name",outName.toStdString());
            }

            auto bcsElement=new TiXmlElement("boundary_conditions");
            eqElement->LinkEndChild(bcsElement);
            for ( auto& f : eq.faceBCs )
            {
                sv4guisvFSIbcClass& bc=f.second;

                auto faceElement = new TiXmlElement("face");
                bcsElement->LinkEndChild(faceElement);
                faceElement->SetAttribute("name",bc.faceName.toStdString());
                faceElement->SetAttribute("group",bc.bcGrp.toStdString());
                faceElement->SetAttribute("type",bc.bcType.toStdString());

                if ( bc.bcType == "Steady" )
                    faceElement->SetDoubleAttribute("value",bc.g);
                else if ( bc.bcType == "Unsteady" )
                    faceElement->SetAttribute("file",bc.gtFile.toStdString());
                else if ( bc.bcType == "Resistance" )
                    faceElement->SetDoubleAttribute("value",bc.r);
                else if ( bc.bcType == "General" )
                    faceElement->SetAttribute("file",bc.gmFile.toStdString());
                else if ( bc.bcType == "Projection" )
                    faceElement->SetAttribute("projection",bc.projectionFaceName.toStdString());

                faceElement->SetAttribute("profile",bc.profile.toStdString());
                if(bc.profile== "User_defined" )
                    faceElement->SetAttribute("profile_file",bc.gxFile.toStdString());

                faceElement->SetAttribute("zperm",bc.zperm?"true":"false");
                faceElement->SetAttribute("flux",bc.flux?"true":"false");

                faceElement->SetAttribute("impose_integral", bc.imposeIntegral?"true":"false");
                faceElement->SetAttribute("effective_direction", bc.effectiveDirection.toStdString());
            }

        }

    }

    if (document.SaveFile(fileName) == false)
    {
        mitkThrow() << "Could not write model to " << fileName;
    }
}

mitk::IFileIO::ConfidenceLevel sv4guiMitksvFSIJobIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const sv4guiMitksvFSIJob* input = dynamic_cast<const sv4guiMitksvFSIJob*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

sv4guiMitksvFSIJobIO* sv4guiMitksvFSIJobIO::IOClone() const
{
    return new sv4guiMitksvFSIJobIO(*this);
}
