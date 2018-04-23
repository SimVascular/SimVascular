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

#include "sv4gui_XmlIOUtil.h"

TiXmlElement* sv4guiXmlIOUtil::CreateXMLxyzElement(const char* name, double v[3])
{
    auto  xyzElement = new TiXmlElement(name);
    xyzElement->SetDoubleAttribute("x", v[0]);
    xyzElement->SetDoubleAttribute("y", v[1]);
    xyzElement->SetDoubleAttribute("z", v[2]);
    return xyzElement;
}

TiXmlElement* sv4guiXmlIOUtil::CreateXMLPointElement(const char* name, int id,mitk::Point3D point)
{
    auto  xyzElement = new TiXmlElement(name);
    xyzElement->SetDoubleAttribute("id", id);
    xyzElement->SetDoubleAttribute("x", point[0]);
    xyzElement->SetDoubleAttribute("y", point[1]);
    xyzElement->SetDoubleAttribute("z", point[2]);
    return xyzElement;
}

TiXmlElement* sv4guiXmlIOUtil::CreateXMLPointElement(const char* name, mitk::Point3D point)
{
    double v[3];
    v[0]=point[0];
    v[1]=point[1];
    v[2]=point[2];

    return CreateXMLxyzElement(name,v);
}

TiXmlElement* sv4guiXmlIOUtil::CreateXMLVectorElement(const char* name, mitk::Vector3D vec)
{
    double v[3];
    v[0]=vec[0];
    v[1]=vec[1];
    v[2]=vec[2];

    return CreateXMLxyzElement(name,v);
}

void sv4guiXmlIOUtil::Getxyz(TiXmlElement* element, double xyz[3])
{
    element->QueryDoubleAttribute("x", &xyz[0]);
    element->QueryDoubleAttribute("y", &xyz[1]);
    element->QueryDoubleAttribute("z", &xyz[2]);
}

mitk::Point3D sv4guiXmlIOUtil::GetPoint(TiXmlElement* element)
{
    double p[3]={0};
    Getxyz(element,p);

    mitk::Point3D point;
    point[0]=p[0];
    point[1]=p[1];
    point[2]=p[2];

    return point;
}

mitk::Vector3D sv4guiXmlIOUtil::GetVector(TiXmlElement* element)
{
    double v[3]={0};
    Getxyz(element,v);

    mitk::Vector3D vector;
    vector[0]=v[0];
    vector[1]=v[1];
    vector[2]=v[2];

    return vector;
}

std::list< double >
sv4guiXmlIOUtil::GetDoubleAttributeListFromXMLNode(TiXmlElement* e, const char *attributeNameBase, unsigned int count)
{
    std::list< double > list;

    if (e == nullptr)
    {
//        throw std::invalid_argument("node invalid");
        mitkThrow() << "Xml node invalid";
    }
    for ( unsigned int i = 0; i < count; ++i )
    {
        mitk::ScalarType p(-1.0);
        std::stringstream attributeName;
        attributeName << attributeNameBase << i;

        if (e->QueryDoubleAttribute( attributeName.str().c_str(), &p ) == TIXML_WRONG_TYPE)
        {
            mitkThrow() << "Xml node malformatted";
//            throw std::invalid_argument("node malformatted");
        }

        list.push_back( p );
    }

    return list;
}
