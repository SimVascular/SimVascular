#ifndef SVPATHOBJECTFACTORY_H
#define SVPATHOBJECTFACTORY_H

#include "SimVascular.h"

#include <svPathExports.h>

#include "svPathIO.h"

#include "mitkCoreObjectFactoryBase.h"

class SVPATH_EXPORT svPathObjectFactory : public mitk::CoreObjectFactoryBase
{
public:
    mitkClassMacro(svPathObjectFactory,mitk::CoreObjectFactoryBase);
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
    svPathObjectFactory();
    ~svPathObjectFactory();
    void CreateFileExtensionsMap();
    MultimapType m_FileExtensionsMap;
    MultimapType m_SaveFileExtensionsMap;

private:

};

struct SVPATH_EXPORT RegistersvPathObjectFactory{

    RegistersvPathObjectFactory();

    virtual ~RegistersvPathObjectFactory();

    svPathObjectFactory::Pointer m_Factory;
    svPathIO* m_svPathIO;
};

#endif // SVPATHOBJECTFACTORY_H
