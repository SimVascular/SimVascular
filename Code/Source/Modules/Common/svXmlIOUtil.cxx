#include "svXmlIOUtil.h"

TiXmlElement* svXmlIOUtil::CreateXMLxyzElement(const char* name, double v[3])
{
    auto  xyzElement = new TiXmlElement(name);
    xyzElement->SetDoubleAttribute("x", v[0]);
    xyzElement->SetDoubleAttribute("y", v[1]);
    xyzElement->SetDoubleAttribute("z", v[2]);
    return xyzElement;
}

TiXmlElement* svXmlIOUtil::CreateXMLPointElement(const char* name, int id,mitk::Point3D point)
{
    auto  xyzElement = new TiXmlElement(name);
    xyzElement->SetDoubleAttribute("id", id);
    xyzElement->SetDoubleAttribute("x", point[0]);
    xyzElement->SetDoubleAttribute("y", point[1]);
    xyzElement->SetDoubleAttribute("z", point[2]);
    return xyzElement;
}

TiXmlElement* svXmlIOUtil::CreateXMLPointElement(const char* name, mitk::Point3D point)
{
    double v[3];
    v[0]=point[0];
    v[1]=point[1];
    v[2]=point[2];

    return CreateXMLxyzElement(name,v);
}

TiXmlElement* svXmlIOUtil::CreateXMLVectorElement(const char* name, mitk::Vector3D vec)
{
    double v[3];
    v[0]=vec[0];
    v[1]=vec[1];
    v[2]=vec[2];

    return CreateXMLxyzElement(name,v);
}

void svXmlIOUtil::Getxyz(TiXmlElement* element, double xyz[3])
{
    element->QueryDoubleAttribute("x", &xyz[0]);
    element->QueryDoubleAttribute("y", &xyz[1]);
    element->QueryDoubleAttribute("z", &xyz[2]);
}

mitk::Point3D svXmlIOUtil::GetPoint(TiXmlElement* element)
{
    double p[3]={0};
    Getxyz(element,p);

    mitk::Point3D point;
    point[0]=p[0];
    point[1]=p[1];
    point[2]=p[2];

    return point;
}

mitk::Vector3D svXmlIOUtil::GetVector(TiXmlElement* element)
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
svXmlIOUtil::GetDoubleAttributeListFromXMLNode(TiXmlElement* e, const char *attributeNameBase, unsigned int count)
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
