#ifndef SVSEG3D_H
#define SVSEG3D_H

#include <svSegmentationExports.h>

#include <vtkPolyData.h>
#include <vtkSmartPointer.h>
#include <map>

struct SVSEGMENTATION_EXPORT svSeed
{
    int id;
    std::string type;//begin, end

    double x;
    double y;
    double z;

    double radius;

    bool selected;

    std::string status;

    svSeed()
        : id(-1)
        , type("")
        , x(0)
        , y(0)
        , z(0)
        , radius(0.2)
        , selected(false)
        , status("")
    {
    }

    svSeed(double xx, double yy, double zz, std::string ttype="")
        : id(-1)
        , type(ttype)
        , x(xx)
        , y(yy)
        , z(zz)
        , radius(0.2)
        , selected(false)
        , status("")
    {
    }

    svSeed(const svSeed &other)
        : id(-1)
        , type(other.type)
        , x(other.x)
        , y(other.y)
        , z(other.z)
        , radius(other.radius)
        , selected(false)
        , status("")
    {
    }

};

struct SVSEGMENTATION_EXPORT svSeg3DParam
{
    std::string method;

    double lowerThreshold;
    double upperThreshold;

    std::map<int, svSeed> seedMap;

    svSeg3DParam()
        : method("")
        , lowerThreshold(0)
        , upperThreshold(0)
    {
    }

    svSeg3DParam(const svSeg3DParam &other)
        : method(other.method)
        , lowerThreshold(other.lowerThreshold)
        , upperThreshold(other.upperThreshold)
        , seedMap(other.seedMap)
    {
    }

    std::map<int,svSeed>& GetSeedMap()
    {
        return seedMap;
    }

    int AddSeed(svSeed seed)
    {    int newID=seed.id;

         if(newID<0)
         {
             int idmax=0;
             for(auto s:seedMap)
             {
                 if(s.first>idmax)
                     idmax=s.first;
             }

             newID=idmax+1;

             seed.id=newID;
         }

         seedMap[newID]=seed;

         return newID;
    }

    void RemoveSeed(int id)
    {
       seedMap.erase(id);
    }

};



class SVSEGMENTATION_EXPORT svSeg3D
{
public:

    svSeg3D();

    svSeg3D(const svSeg3D &other, bool copyVpd=true);

    virtual ~svSeg3D();

    virtual svSeg3D* Clone();

    svSeg3DParam& GetParam();

    svSeg3DParam& GetInnerParam();

    void SetParam(svSeg3DParam param, bool copyToInner=true);

    vtkSmartPointer<vtkPolyData> GetVtkPolyData(){return m_Vpd;}

    SetVtkPolyData(vtkSmartPointer<vtkPolyData> vpd) {m_Vpd=vpd;}

protected:

    svSeg3DParam m_Param;

    svSeg3DParam m_InnerParam;

    vtkSmartPointer<vtkPolyData> m_Vpd;

};


#endif // SVSEG3D_H
