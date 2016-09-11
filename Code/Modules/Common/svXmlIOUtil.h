#ifndef SVXMLIOUTIL_H
#define SVXMLIOUTIL_H

#include "SimVascular.h"

#include <svCommonExports.h>

#include "mitkPoint.h"
#include "mitkVector.h"

#include <tinyxml.h>

#include <list>

class SVCOMMON_EXPORT svXmlIOUtil
{
public:

    static TiXmlElement* CreateXMLxyzElement(const char* name, double v[3]);
    static TiXmlElement* CreateXMLPointElement(const char* name, int id, mitk::Point3D point);
    static TiXmlElement* CreateXMLPointElement(const char* name, mitk::Point3D point);
    static TiXmlElement* CreateXMLVectorElement(const char* name, mitk::Vector3D vec);

    static void Getxyz(TiXmlElement* element, double xyz[3]);

    static mitk::Point3D GetPoint(TiXmlElement* element);

    static mitk::Vector3D GetVector(TiXmlElement* element);

    static std::list< double > GetDoubleAttributeListFromXMLNode(TiXmlElement* e, const char *attributeNameBase, unsigned int count);

};

#endif // SVXMLIOUTIL_H
