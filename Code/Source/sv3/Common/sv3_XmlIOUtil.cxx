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

#include "sv3_XmlIOUtil.h"
#include <array>
#include <stdexcept>

using sv3::XmlIOUtil;

TiXmlElement* XmlIOUtil::CreateXMLxyzElement(const char* name, double v[3])
{
    auto  xyzElement = new TiXmlElement(name);
    xyzElement->SetDoubleAttribute("x", v[0]);
    xyzElement->SetDoubleAttribute("y", v[1]);
    xyzElement->SetDoubleAttribute("z", v[2]);
    return xyzElement;
}

TiXmlElement* XmlIOUtil::CreateXMLPointElement(const char* name, int id,std::array<double,3> point)
{
    auto  xyzElement = new TiXmlElement(name);
    xyzElement->SetDoubleAttribute("id", id);
    xyzElement->SetDoubleAttribute("x", point[0]);
    xyzElement->SetDoubleAttribute("y", point[1]);
    xyzElement->SetDoubleAttribute("z", point[2]);
    return xyzElement;
}

TiXmlElement* XmlIOUtil::CreateXMLPointElement(const char* name, std::array<double,3> point)
{
    double v[3];
    v[0]=point[0];
    v[1]=point[1];
    v[2]=point[2];

    return CreateXMLxyzElement(name,v);
}

TiXmlElement* XmlIOUtil::CreateXMLVectorElement(const char* name, std::array<double,3> vec)
{
    double v[3];
    v[0]=vec[0];
    v[1]=vec[1];
    v[2]=vec[2];

    return CreateXMLxyzElement(name,v);
}

void XmlIOUtil::Getxyz(TiXmlElement* element, double xyz[3])
{
    element->QueryDoubleAttribute("x", &xyz[0]);
    element->QueryDoubleAttribute("y", &xyz[1]);
    element->QueryDoubleAttribute("z", &xyz[2]);
}

std::array<double,3> XmlIOUtil::GetPoint(TiXmlElement* element)
{
    double p[3]={0};
    Getxyz(element,p);

    std::array<double,3> point;
    point[0]=p[0];
    point[1]=p[1];
    point[2]=p[2];

    return point;
}

std::array<double,3> XmlIOUtil::GetVector(TiXmlElement* element)
{
    double v[3]={0};
    Getxyz(element,v);

    std::array<double,3> vector;
    vector[0]=v[0];
    vector[1]=v[1];
    vector[2]=v[2];

    return vector;
}

std::list< double >
XmlIOUtil::GetDoubleAttributeListFromXMLNode(TiXmlElement* e, const char *attributeNameBase, unsigned int count)
{
    std::list< double > list;

    if (e == nullptr)
    {
        throw std::invalid_argument("node invalid");
    }
    for ( unsigned int i = 0; i < count; ++i )
    {
        double p=-1.0;
        std::stringstream attributeName;
        attributeName << attributeNameBase << i;

        if (e->QueryDoubleAttribute( attributeName.str().c_str(), &p ) == TIXML_WRONG_TYPE)
        {
            throw std::invalid_argument("node malformatted");
        }

        list.push_back( p );
    }

    return list;
}
