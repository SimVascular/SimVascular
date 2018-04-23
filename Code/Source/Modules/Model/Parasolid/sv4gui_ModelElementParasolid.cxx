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

#include "sv4gui_ModelElementParasolid.h"
#include "sv4gui_ModelUtilsParasolid.h"

#include <iostream>

sv4guiModelElementParasolid::sv4guiModelElementParasolid()
{
    m_Type="Parasolid";
    m_MaxDist=1.0;
    std::vector<std::string> exts={"xmt_txt"};
    m_FileExtensions=exts;
}

sv4guiModelElementParasolid::sv4guiModelElementParasolid(const sv4guiModelElementParasolid &other)
    : sv4guiModelElementAnalytic(other)
{
}

sv4guiModelElementParasolid::~sv4guiModelElementParasolid()
{
}

sv4guiModelElementParasolid* sv4guiModelElementParasolid::Clone()
{
    return new sv4guiModelElementParasolid(*this);
}

sv4guiModelElement* sv4guiModelElementParasolid::CreateModelElement()
{
    return new sv4guiModelElementParasolid();
}

sv4guiModelElement* sv4guiModelElementParasolid::CreateModelElement(std::vector<mitk::DataNode::Pointer> segNodes
                                , int numSamplingPts
                                , svLoftingParam *param
                                , int* stats
                                , double maxDist
                                , int noInterOut
                                , double tol
                                , unsigned int t)
{
    return sv4guiModelUtilsParasolid::CreateModelElementParasolid(segNodes,numSamplingPts,maxDist,t);
}

sv4guiModelElement* sv4guiModelElementParasolid::CreateModelElementByBlend(std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii
                                                  , sv4guiModelElement::svBlendParam* param)
{
    return sv4guiModelUtilsParasolid::CreateModelElementParasolidByBlend(this,blendRadii);
}

bool sv4guiModelElementParasolid::ReadFile(std::string filePath)
{
    cvParasolidSolidModel* parasolid=new cvParasolidSolidModel();
    char* df=const_cast<char*>(filePath.c_str());
    m_InnerSolid=parasolid;
    if(m_InnerSolid->ReadNative(df)==SV_OK)
        return true;
    else
        return false;
}

bool sv4guiModelElementParasolid::WriteFile(std::string filePath)
{
    if(m_InnerSolid)
    {
        char* df=const_cast<char*>(filePath.c_str());
        if (m_InnerSolid->WriteNative(0,df)!=SV_OK )
            return false;
    }

    return true;
}

int sv4guiModelElementParasolid::GetFaceIdentifierFromInnerSolid(std::string faceName)
{
    int faceID=GetFaceIDFromInnerSolid(faceName);
    char* value;
    m_InnerSolid->GetFaceAttribute("identifier",faceID,&value);
    std::string idtf(value);
    int ident=std::stoi(idtf);

    return ident;
}

int sv4guiModelElementParasolid::GetFaceIdentifierFromInnerSolid(int faceID)
{
    char* value;
    m_InnerSolid->GetFaceAttribute("identifier",faceID,&value);
    std::string idtf(value);
    int ident=std::stoi(idtf);

    return ident;
}
