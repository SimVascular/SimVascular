#ifndef SVMITKSEG3D_H
#define SVMITKSEG3D_H

#include <svSegmentationExports.h>

#include <mitkSurface.h>

struct SVSEGMENTATION_EXPORT svSeed
{
    int id;
    std::string type;//begin, end

    double x;
    double y;
    double z;

    double radius;

    svSeed()
        : id(-1)
        , type("")
        , x(0)
        , y(0)
        , z(0)
        , radius(1)
    {
    }

    svSeed(const svSeed &other)
        : id(-1)
        , type(other.type)
        , x(other.x)
        , y(other.y)
        , z(other.z)
        , radius(other.radius)
    {
    }

};

class SVSEGMENTATION_EXPORT svMitkSeg3D : public mitk::Surface
{
public:

    mitkClassMacro(svMitkSeg3D, mitk::Surface);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual void ExecuteOperation(mitk::Operation *operation) override;

    void SetMethod(std::string type);

    std::string GetMethod() const;

    std::map<int,svSeed>& GetSeedMap();

    int AddSeed(svSeed seed);

    bool RemoveSeed(int id);

    bool IsDataModified(){return m_DataModified;}
    void SetDataModified(bool modified = true){m_DataModified=modified;}

  protected:

    mitkCloneMacro(Self);

    svMitkSeg3D();
    svMitkSeg3D(const svMitkSeg3D &other);
    virtual ~svMitkSeg3D();

    std::string m_Method;

    bool m_DataModified;

    //parameters

    std::map<int, svSeed> m_SeedMap;
};

itkEventMacro( svMitkSeg3DEvent, itk::AnyEvent );

itkEventMacro( svMitkSeg3DSetEvent, svMitkSeg3DEvent );

#endif // SVMITKSEG3D_H
