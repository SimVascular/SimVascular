#ifndef SVMITKSEG3D_H
#define SVMITKSEG3D_H

#include <svSegmentationExports.h>

#include <svSeg3D.h>

#include <mitkSurface.h>

class SVSEGMENTATION_EXPORT svMitkSeg3D : public mitk::Surface
{
public:

    mitkClassMacro(svMitkSeg3D, mitk::Surface);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual bool IsEmptyTimeStep(unsigned int t) const override;
    virtual void ExecuteOperation(mitk::Operation *operation) override;

    bool IsDataModified(){return m_DataModified;}
    void SetDataModified(bool modified = true){m_DataModified=modified;}

    svSeg3D* GetSeg3D() {return m_Seg3D;}

    void SetSeg3D(svSeg3D* seg3D);

  protected:

    mitkCloneMacro(Self);

    svMitkSeg3D();
    svMitkSeg3D(const svMitkSeg3D &other);
    virtual ~svMitkSeg3D();

    bool m_DataModified;

    svSeg3D* m_Seg3D;

};

itkEventMacro( svMitkSeg3DEvent, itk::AnyEvent );

itkEventMacro( svMitkSeg3DSetEvent, svMitkSeg3DEvent );

#endif // SVMITKSEG3D_H
