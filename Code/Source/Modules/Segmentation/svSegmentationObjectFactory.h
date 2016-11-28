#ifndef SVSEGMENTATIONOBJECTFACTORY_H
#define SVSEGMENTATIONOBJECTFACTORY_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContourGroupIO.h"

#include "mitkCoreObjectFactoryBase.h"

class SVSEGMENTATION_EXPORT svSegmentationObjectFactory : public mitk::CoreObjectFactoryBase
{
public:
    mitkClassMacro(svSegmentationObjectFactory,mitk::CoreObjectFactoryBase);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)
    virtual mitk::Mapper::Pointer CreateMapper(mitk::DataNode* node, MapperSlotId slotId) override;
    virtual void SetDefaultProperties(mitk::DataNode* node) override;
    virtual const char* GetFileExtensions() override;
    virtual mitk::CoreObjectFactoryBase::MultimapType GetFileExtensionsMap() override;
    virtual const char* GetSaveFileExtensions() override;
    virtual mitk::CoreObjectFactoryBase::MultimapType GetSaveFileExtensionsMap() override;

    void RegisterIOFactories(); //deprecatedSince{2013_09}
protected:
    svSegmentationObjectFactory();
    ~svSegmentationObjectFactory();
    void CreateFileExtensionsMap();
    MultimapType m_FileExtensionsMap;
    MultimapType m_SaveFileExtensionsMap;

private:

};

struct SVSEGMENTATION_EXPORT RegistersvSegmentationObjectFactory{
  RegistersvSegmentationObjectFactory();

  virtual ~RegistersvSegmentationObjectFactory();

  svSegmentationObjectFactory::Pointer m_Factory;
  svContourGroupIO* m_ContourGroupIO;
};

#endif // SVSEGMENTATIONOBJECTFACTORY_H
