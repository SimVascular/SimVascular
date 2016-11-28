#ifndef SVMITKMESHOBJECTFACTORY_H
#define SVMITKMESHOBJECTFACTORY_H

#include <svMeshExports.h>

#include "svMitkMeshIO.h"

#include "mitkCoreObjectFactoryBase.h"

class SVMESH_EXPORT svMitkMeshObjectFactory : public mitk::CoreObjectFactoryBase
{
public:
    mitkClassMacro(svMitkMeshObjectFactory,mitk::CoreObjectFactoryBase);
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
    svMitkMeshObjectFactory();
    ~svMitkMeshObjectFactory();
    void CreateFileExtensionsMap();
    MultimapType m_FileExtensionsMap;
    MultimapType m_SaveFileExtensionsMap;

private:

};

struct SVMESH_EXPORT RegistersvMitkMeshObjectFactory{
  RegistersvMitkMeshObjectFactory();

  virtual ~RegistersvMitkMeshObjectFactory();

  svMitkMeshObjectFactory::Pointer m_Factory;
  svMitkMeshIO* m_MitkMeshIO;
};

#endif // SVMITKMESHOBJECTFACTORY_H
