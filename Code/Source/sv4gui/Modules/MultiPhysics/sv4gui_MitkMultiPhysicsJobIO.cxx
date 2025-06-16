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

// The methods defined here are used to read/write MultiPhysics XML project .multiphysicsjob files.

#include "sv4gui_MitkMultiPhysicsJobIO.h"

#include "sv4gui_MitkMultiPhysicsJob.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include <tinyxml2.h>

#include <fstream>

static mitk::CustomMimeType CreatesvSimJobMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".multiphysicsjob");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("multiphysicsjob");
    mimeType.SetComment("MultiPhysics Job");

    return mimeType;
}

sv4guiMitkMultiPhysicsJobIO::sv4guiMitkMultiPhysicsJobIO()
    : mitk::AbstractFileIO(sv4guiMitkMultiPhysicsJob::GetStaticNameOfClass(), CreatesvSimJobMimeType(), "sv4guiMultiPhysics Job")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> sv4guiMitkMultiPhysicsJobIO::DoRead()
{
    std::vector<mitk::BaseData::Pointer> result;

    tinyxml2::XMLDocument document;

    std::string fileName = GetInputLocation();

    if (document.LoadFile(fileName.c_str()) != tinyxml2::XML_SUCCESS)
    {
        mitkThrow() << "Could not open/read/parse " << fileName;
        //        MITK_ERROR << "Could not open/read/parse " << fileName;
        return result;
    }

    //    tinyxml2::XMLElement* version = document.FirstChildElement("format");

    tinyxml2::XMLElement* mjElement = document.FirstChildElement("mitk_job");

    if(!mjElement){
        //        MITK_ERROR << "No job data in "<< fileName;
        mitkThrow() << "No job data in "<< fileName;
        return result;
    }

    sv4guiMitkMultiPhysicsJob::Pointer mitkSimJob = sv4guiMitkMultiPhysicsJob::New();
    const char* modelName;
    const char* meshName;
    const char* status;
    int procNum=1;
    mjElement->QueryStringAttribute("model_name", &modelName);
    mjElement->QueryStringAttribute("mesh_name", &meshName);
    mjElement->QueryStringAttribute("status", &status);
    mjElement->QueryIntAttribute("process_number",&procNum);

    mitkSimJob->SetModelName(modelName);
    mitkSimJob->SetMeshName(meshName);
    mitkSimJob->SetStatus(status);
    mitkSimJob->SetProcessNumber(procNum);

    const char* strValue;

    tinyxml2::XMLElement* jobElement = mjElement->FirstChildElement("job");

    if (jobElement != nullptr) {
        sv4guiMultiPhysicsJob* job = new sv4guiMultiPhysicsJob();

        jobElement->QueryIntAttribute("nsd",&job->nsd);
        jobElement->QueryIntAttribute("timeSteps",&job->timeSteps);
        const char* stepSize;
        jobElement->QueryStringAttribute("stepSize",&stepSize);
        job->stepSize = stepSize;
        jobElement->QueryBoolAttribute("continuePrevious",&job->continuePrevious);

        const char* restartFileName;
        jobElement->QueryStringAttribute("restartFileName", &restartFileName);
        job->restartFileName = restartFileName;
        jobElement->QueryIntAttribute("restartInc", &job->restartInc);

        jobElement->QueryBoolAttribute("vtkSaveResults", &job->vtkSaveResults);
        const char* vtkFileName;
        jobElement->QueryStringAttribute("vtkFileName", &vtkFileName);
        job->vtkFileName = vtkFileName;
        jobElement->QueryIntAttribute("vtkInc", &job->vtkInc);

        jobElement->QueryIntAttribute("startSavingStep",&job->startSavingStep);
        jobElement->QueryBoolAttribute("saveAvgResult",&job->saveAvgResult);
        jobElement->QueryDoubleAttribute("rhoInf",&job->rhoInf);
        const char* stopFileName;
        jobElement->QueryStringAttribute("stopFileName",&stopFileName);
        job->stopFileName = stopFileName;
        jobElement->QueryBoolAttribute("remeshing",&job->remeshing);
        jobElement->QueryBoolAttribute("verbose",&job->verbose);
        jobElement->QueryBoolAttribute("warn",&job->warn);
        jobElement->QueryBoolAttribute("debug",&job->debug);

        tinyxml2::XMLElement* dsElement = jobElement->FirstChildElement("domains");
        if(dsElement != nullptr)
        {
            for( tinyxml2::XMLElement* domainElement = dsElement->FirstChildElement("domain");
                 domainElement != nullptr;
                 domainElement =domainElement->NextSiblingElement("domain") )
            {
//                if (domainElement == nullptr)
//                    continue;

                sv4guiMultiPhysicsDomain domain;
                const char* name;
                const char* type;
                const char* folderName;
                const char* fileName;
                const char* surfaceName;
                const char* faceFolderName;
                domainElement->QueryStringAttribute("name", &name);
                domainElement->QueryStringAttribute("type", &type);
                domainElement->QueryStringAttribute("folder_name", &folderName);
                domainElement->QueryStringAttribute("file_name", &fileName);
                domainElement->QueryStringAttribute("surface_name",&surfaceName);
                domainElement->QueryStringAttribute("face_folder_name", &faceFolderName);
                domain.name = name;
                domain.type = type;
                domain.folderName = folderName;
                domain.fileName = fileName;
                domain.surfaceName = surfaceName;
                domain.faceFolderName = faceFolderName;

                for(tinyxml2::XMLElement* faceElement=domainElement->FirstChildElement("face");
                    faceElement !=nullptr;
                    faceElement = faceElement->NextSiblingElement("face"))
                {
                    const char* faceName;
                    faceElement->QueryStringAttribute("name",&faceName);
                    domain.faceNames.push_back(faceName);
                }

                job->m_Domains[domain.name]=domain;

            }
        }

        tinyxml2::XMLElement* eqsElement = jobElement->FirstChildElement("equations");
        if(eqsElement != nullptr)
        {
            for( tinyxml2::XMLElement* eqElement = eqsElement->FirstChildElement("equation");
                 eqElement != nullptr;
                 eqElement =eqElement->NextSiblingElement("equation") )
            {
                strValue="";
                eqElement->QueryStringAttribute("physName", &strValue);
                if(strValue=="" || strValue=="none")
                    continue;

                sv4guiMultiPhysicseqClass eq=sv4guiMultiPhysicseqClass(QString::fromStdString(strValue));

                eqElement->QueryBoolAttribute("coupled", &eq.coupled);
                eqElement->QueryIntAttribute("minItr", &eq.minItr);
                eqElement->QueryIntAttribute("maxItr", &eq.maxItr);
                strValue="";
                eqElement->QueryStringAttribute("tol", &strValue);
                eq.tol=QString::fromStdString(strValue);
                eqElement->QueryDoubleAttribute("dBr", &eq.dBr);
                eqElement->QueryDoubleAttribute("backflow", &eq.backflowStab);

                tinyxml2::XMLElement* propsElement=eqElement->FirstChildElement("props");
                if(propsElement!=nullptr)
                {
                    for(tinyxml2::XMLElement* propElement=propsElement->FirstChildElement("prop");
                        propElement !=nullptr;
                        propElement = propElement->NextSiblingElement("prop"))
                    {
                        strValue="";
                        propElement->QueryStringAttribute("name",&strValue);
                        const char* strValue2;
                        propElement->QueryStringAttribute("value",&strValue2);

                        if(strValue=="Constitutive model")
                        {
                            eq.constitutiveModel=QString::fromStdString(strValue2);
                            continue;
                        }

                        eq.setPropValue(QString::fromStdString(strValue2).toDouble(),QString::fromStdString(strValue));
                    }
                }

                tinyxml2::XMLElement* lsElement=eqElement->FirstChildElement("linear_solver");
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

                tinyxml2::XMLElement* rmElement=eqElement->FirstChildElement("remesher");
                if(rmElement!=nullptr)
                {
                    strValue="";
                    rmElement->QueryStringAttribute("name",&strValue);
                    eq.remesher=QString::fromStdString(strValue);

                    rmElement->QueryDoubleAttribute("angle",&eq.rmMinAngle);
                    rmElement->QueryDoubleAttribute("ratio",&eq.rmMaxRadiusRatio);
                    rmElement->QueryIntAttribute("frequency",&eq.rmFrequency);
                    rmElement->QueryIntAttribute("copy_frequency",&eq.rmCopyFrequency);

                    for(tinyxml2::XMLElement* sizeElement=rmElement->FirstChildElement("edge_size");
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

                tinyxml2::XMLElement* outputsElement=eqElement->FirstChildElement("outputs");
                if(outputsElement!=nullptr)
                {
                    tinyxml2::XMLElement* spatialElement=outputsElement->FirstChildElement("spatial");
                    if(spatialElement!=nullptr)
                    {
                        for(tinyxml2::XMLElement* oElement=spatialElement->FirstChildElement("output");
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

                tinyxml2::XMLElement* bcsElement=eqElement->FirstChildElement("boundary_conditions");
                if(bcsElement!=nullptr)
                {
                    for(tinyxml2::XMLElement* faceElement=bcsElement->FirstChildElement("face");
                        faceElement!=nullptr;
                        faceElement=faceElement->NextSiblingElement("face"))
                    {
                        sv4guiMultiPhysicsbcClass bc;

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

mitk::IFileIO::ConfidenceLevel sv4guiMitkMultiPhysicsJobIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void sv4guiMitkMultiPhysicsJobIO::Write()
{
    //std::string dmsg("[sv4guiMitkMultiPhysicsJobIO::Write] ");
    //std::cout << dmsg << "========= Write ==========" << std::endl;

    ValidateOutputLocation();

    std::string fileName=GetOutputLocation();

    const sv4guiMitkMultiPhysicsJob* mitkSimJob = dynamic_cast<const sv4guiMitkMultiPhysicsJob*>(this->GetInput());
    if(!mitkSimJob) return;

    tinyxml2::XMLDocument document;
    tinyxml2::XMLDeclaration* decl = document.NewDeclaration();
    decl->SetValue("xml version=\"1.0\" encoding=\"\"");
    document.LinkEndChild( decl );

    tinyxml2::XMLElement* mjElement = document.NewElement("mitk_job");
    mjElement->SetAttribute("model_name", mitkSimJob->GetModelName().c_str());
    mjElement->SetAttribute("mesh_name", mitkSimJob->GetMeshName().c_str());
    mjElement->SetAttribute("status", mitkSimJob->GetStatus().c_str());
    mjElement->SetAttribute("process_number", mitkSimJob->GetProcessNumber());
    mjElement->SetAttribute("version",  "1.0" );
    document.LinkEndChild(mjElement);

    sv4guiMultiPhysicsJob* job = mitkSimJob->GetSimJob();

    if (job) {
        tinyxml2::XMLElement* jobElement = document.NewElement("job");
        mjElement->LinkEndChild(jobElement);

        jobElement->SetAttribute("nsd",job->nsd);
        jobElement->SetAttribute("timeSteps",job->timeSteps);
        jobElement->SetAttribute("stepSize",job->stepSize.c_str());
        jobElement->SetAttribute("continuePrevious",job->continuePrevious?"true":"false");

        jobElement->SetAttribute("restartFileName", job->restartFileName.c_str());
        jobElement->SetAttribute("restartInc", job->restartInc);

        jobElement->SetAttribute("vtkSaveResults", job->vtkSaveResults);
        jobElement->SetAttribute("vtkFileName", job->vtkFileName.c_str());
        jobElement->SetAttribute("vtkInc", job->vtkInc);

        jobElement->SetAttribute("startSavingStep",job->startSavingStep);
        jobElement->SetAttribute("saveAvgResult",job->saveAvgResult?"true":"false");
        jobElement->SetAttribute("rhoInf",job->rhoInf);
        jobElement->SetAttribute("stopFileName",job->stopFileName.c_str());
        jobElement->SetAttribute("remeshing",job->remeshing?"true":"false");
        jobElement->SetAttribute("verbose",job->verbose?"true":"false");
        jobElement->SetAttribute("warn",job->warn?"true":"false");
        jobElement->SetAttribute("debug",job->debug?"true":"false");

        tinyxml2::XMLElement* dsElement = document.NewElement("domains");
        jobElement->LinkEndChild(dsElement);

        for(auto& md : job->m_Domains) {
            sv4guiMultiPhysicsDomain& domain=md.second;

            tinyxml2::XMLElement* domainElement = document.NewElement("domain");
            dsElement->LinkEndChild(domainElement);

            domainElement->SetAttribute("name", domain.name.c_str());
            domainElement->SetAttribute("type", domain.type.c_str());
            domainElement->SetAttribute("folder_name", domain.folderName.c_str());
            domainElement->SetAttribute("file_name",domain.fileName.c_str());
            domainElement->SetAttribute("surface_name",domain.surfaceName.c_str());
            domainElement->SetAttribute("face_folder_name",domain.faceFolderName.c_str());

            for(std::string faceName : domain.faceNames)
            {
                tinyxml2::XMLElement* faceElement = document.NewElement("face");
                domainElement->LinkEndChild(faceElement);
                faceElement->SetAttribute("name", faceName.c_str());
            }
        }

        tinyxml2::XMLElement* eqsElement = document.NewElement("equations");
        jobElement->LinkEndChild(eqsElement);

        for(sv4guiMultiPhysicseqClass & eq : job->m_Eqs) {
            tinyxml2::XMLElement* eqElement = document.NewElement("equation");
            eqsElement->LinkEndChild(eqElement);

            eqElement->SetAttribute("physName",eq.physName.toStdString().c_str());
            eqElement->SetAttribute("coupled",eq.coupled?"true":"false");
            eqElement->SetAttribute("minItr",eq.minItr);
            eqElement->SetAttribute("maxItr",eq.maxItr);
            eqElement->SetAttribute("tol",eq.tol.toStdString().c_str());
            eqElement->SetAttribute("dBr",eq.dBr);
            eqElement->SetAttribute("backflow",eq.backflowStab);

            tinyxml2::XMLElement* propsElement = document.NewElement("props");
            eqElement->LinkEndChild(propsElement);

            for(int i=0;i<eq.getPropCount();++i) {
                tinyxml2::XMLElement* propElement = document.NewElement("prop");
                propsElement->LinkEndChild(propElement);
                propElement->SetAttribute("name",eq.getPropName(i).toStdString().c_str());
                propElement->SetAttribute("value", eq.getPropValue(i));
            }

            if(eq.physName=="FSI" || eq.physName=="struct")
            {
                tinyxml2::XMLElement* propElement = document.NewElement("prop");
                propsElement->LinkEndChild(propElement);

                propElement->SetAttribute("name","Constitutive model");
                propElement->SetAttribute("value",eq.constitutiveModel.toStdString().c_str());
            }

            tinyxml2::XMLElement* lsElement = document.NewElement("linear_solver");
            eqElement->LinkEndChild(lsElement);
            lsElement->SetAttribute("lsType",eq.lsType.toStdString().c_str());
            lsElement->SetAttribute("lsMaxItr",eq.lsMaxItr);
            lsElement->SetAttribute("lsTol",eq.lsTol.toStdString().c_str());
            lsElement->SetAttribute("lsNSGMMaxItr",eq.lsNSGMMaxItr);
            lsElement->SetAttribute("lsNSGMTol",eq.lsNSGMTol.toStdString().c_str());
            lsElement->SetAttribute("lsNSCGMaxItr",eq.lsNSCGMaxItr);
            lsElement->SetAttribute("lsNSCGTol",eq.lsNSCGTol.toStdString().c_str());
            lsElement->SetAttribute("lsAbsoluteTol",eq.lsAbsoluteTol.toStdString().c_str());
            lsElement->SetAttribute("lsKrylovDim",eq.lsKrylovDim);
            lsElement->SetAttribute("lsPreconditioner",eq.lsPreconditioner.toStdString().c_str());

            if(eq.physName=="FSI" && eq.remesher!="None")
            {
                tinyxml2::XMLElement* rmElement = document.NewElement("remesher");
                eqElement->LinkEndChild(rmElement);
                rmElement->SetAttribute("name",eq.remesher.toStdString().c_str());
                rmElement->SetAttribute("angle",eq.rmMinAngle);
                rmElement->SetAttribute("ratio",eq.rmMaxRadiusRatio);
                rmElement->SetAttribute("frequency",eq.rmFrequency);
                rmElement->SetAttribute("copy_frequency",eq.rmCopyFrequency);

                for(auto& pair : job->m_Domains)
                {
                    std::string domainName=pair.first;
                    sv4guiMultiPhysicsDomain& domain=pair.second;

                    tinyxml2::XMLElement* sizeElement = document.NewElement("edge_size");
                    rmElement->LinkEndChild(sizeElement);
                    sizeElement->SetAttribute("domain",domainName.c_str());
                    sizeElement->SetAttribute("size",domain.edgeSize);
                }
            }

            tinyxml2::XMLElement* outputsElement = document.NewElement("outputs");
            eqElement->LinkEndChild(outputsElement);

            tinyxml2::XMLElement* spatialElement = document.NewElement("spatial");
            outputsElement->LinkEndChild(spatialElement);
            foreach ( QString outName , eq.getOutputNames() )
            {
                tinyxml2::XMLElement* oElement = document.NewElement("output");
                spatialElement->LinkEndChild(oElement);
                oElement->SetAttribute("name",outName.toStdString().c_str());
            }

            tinyxml2::XMLElement* bcsElement = document.NewElement("boundary_conditions");
            eqElement->LinkEndChild(bcsElement);
            for ( auto& f : eq.faceBCs )
            {
                sv4guiMultiPhysicsbcClass& bc=f.second;

                tinyxml2::XMLElement* faceElement = document.NewElement("face");
                bcsElement->LinkEndChild(faceElement);
                faceElement->SetAttribute("name",bc.faceName.toStdString().c_str());
                faceElement->SetAttribute("group",bc.bcGrp.toStdString().c_str());
                faceElement->SetAttribute("type",bc.bcType.toStdString().c_str());

                if ( bc.bcType == "Steady" )
                    faceElement->SetAttribute("value",bc.g);
                else if ( bc.bcType == "Unsteady" )
                    faceElement->SetAttribute("file",bc.gtFile.toStdString().c_str());
                else if ( bc.bcType == "Resistance" )
                    faceElement->SetAttribute("value",bc.r);
                else if ( bc.bcType == "General" )
                    faceElement->SetAttribute("file",bc.gmFile.toStdString().c_str());
                else if ( bc.bcType == "Projection" )
                    faceElement->SetAttribute("projection",bc.projectionFaceName.toStdString().c_str());

                faceElement->SetAttribute("profile",bc.profile.toStdString().c_str());
                if(bc.profile== "User_defined" )
                    faceElement->SetAttribute("profile_file",bc.gxFile.toStdString().c_str());

                faceElement->SetAttribute("zperm",bc.zperm?"true":"false");
                faceElement->SetAttribute("flux",bc.flux?"true":"false");

                faceElement->SetAttribute("impose_integral", bc.imposeIntegral?"true":"false");
                faceElement->SetAttribute("effective_direction", bc.effectiveDirection.toStdString().c_str());
            }

        }

    }

    if (document.SaveFile(fileName.c_str()) != tinyxml2::XML_SUCCESS)
    {
        mitkThrow() << "Could not write MultiPhysics parameters to file " << fileName;
    }
}

mitk::IFileIO::ConfidenceLevel sv4guiMitkMultiPhysicsJobIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const sv4guiMitkMultiPhysicsJob* input = dynamic_cast<const sv4guiMitkMultiPhysicsJob*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

sv4guiMitkMultiPhysicsJobIO* sv4guiMitkMultiPhysicsJobIO::IOClone() const
{
    return new sv4guiMitkMultiPhysicsJobIO(*this);
}
