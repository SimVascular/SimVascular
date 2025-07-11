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

#ifndef SV4GUI_XMLIOUTIL_H
#define SV4GUI_XMLIOUTIL_H

#include "SimVascular.h"

#include <sv4guiModuleCommonExports.h>

#include "mitkPoint.h"
#include "mitkVector.h"

#include <tinyxml2.h>

#include <list>

class SV4GUIMODULECOMMON_EXPORT sv4guiXmlIOUtil
{
public:
    sv4guiXmlIOUtil(tinyxml2::XMLDocument& doc) : document(doc) { }
    sv4guiXmlIOUtil() = delete;

    tinyxml2::XMLElement* CreateXMLxyzElement(const char* name, double v[3]);
    tinyxml2::XMLElement* CreateXMLPointElement(const char* name, int id, mitk::Point3D point);
    tinyxml2::XMLElement* CreateXMLPointElement(const char* name, mitk::Point3D point);
    tinyxml2::XMLElement* CreateXMLVectorElement(const char* name, mitk::Vector3D vec);

    void Getxyz(tinyxml2::XMLElement* element, double xyz[3]);
    mitk::Point3D GetPoint(tinyxml2::XMLElement* element);
    mitk::Vector3D GetVector(tinyxml2::XMLElement* element);
    std::list< double > GetDoubleAttributeListFromXMLNode(tinyxml2::XMLElement* e, const char *attributeNameBase, unsigned int count);

    tinyxml2::XMLDocument& document;
};

#endif // SV4GUI_XMLIOUTIL_H
