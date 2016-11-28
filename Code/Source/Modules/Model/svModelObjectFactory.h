#ifndef SVMODELOBJECTFACTORY_H
#define SVMODELOBJECTFACTORY_H

#include <svModelExports.h>

#include "svModelIO.h"

#include "mitkCoreObjectFactoryBase.h"

class SVMODEL_EXPORT svModelObjectFactory : public mitk::CoreObjectFactoryBase
{
public:
    mitkClassMacro(svModelObjectFactory,mitk::CoreObjectFactoryBase);
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
    svModelObjectFactory();
    ~svModelObjectFactory();
    void CreateFileExtensionsMap();
    MultimapType m_FileExtensionsMap;
    MultimapType m_SaveFileExtensionsMap;

private:

};

struct SVMODEL_EXPORT RegistersvModelObjectFactory{
  RegistersvModelObjectFactory();

  virtual ~RegistersvModelObjectFactory();

  svModelObjectFactory::Pointer m_Factory;
  svModelIO* m_ModelIO;
};

#endif // SVMODELOBJECTFACTORY_H
