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

#include "svModelElementOCCT.h"
#include "svModelUtilsOCCT.h"

#include <iostream>

svModelElementOCCT::svModelElementOCCT()
{
    m_Type="OpenCASCADE";
    m_MaxDist=20.0;
    std::vector<std::string> exts={"brep","step","iges","stl"};
    m_FileExtensions=exts;
}

svModelElementOCCT::svModelElementOCCT(const svModelElementOCCT &other)
    : svModelElementAnalytic(other)
{
}

svModelElementOCCT::~svModelElementOCCT()
{
}

svModelElementOCCT* svModelElementOCCT::Clone()
{
    return new svModelElementOCCT(*this);
}

svModelElement* svModelElementOCCT::CreateModelElement()
{
    return new svModelElementOCCT();
}

svModelElement* svModelElementOCCT::CreateModelElement(std::vector<mitk::DataNode::Pointer> segNodes
                                , int numSamplingPts
                                , svLoftingParam *param
                                , int* stats
                                , double maxDist
                                , int noInterOut
                                , double tol
                                , unsigned int t)
{
    return svModelUtilsOCCT::CreateModelElementOCCT(segNodes,numSamplingPts,param,maxDist,t);
}

svModelElement* svModelElementOCCT::CreateModelElementByBlend(std::vector<svModelElement::svBlendParamRadius*> blendRadii
                                                  , svModelElement::svBlendParam* param)
{
    return svModelUtilsOCCT::CreateModelElementOCCTByBlend(this,blendRadii);
}

bool svModelElementOCCT::ReadFile(std::string filePath)
{
    cvOCCTSolidModel* occtSolid=new cvOCCTSolidModel();
    char* df=const_cast<char*>(filePath.c_str());
    m_InnerSolid=occtSolid;
    if(m_InnerSolid->ReadNative(df)==SV_OK)
        return true;
    else
        return false;
}

bool svModelElementOCCT::WriteFile(std::string filePath)
{
    if(m_InnerSolid)
    {
        char* df=const_cast<char*>(filePath.c_str());
        if (m_InnerSolid->WriteNative(0,df) != SV_OK )
            return false;
    }

    return true;
}
