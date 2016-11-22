#ifndef SVMITKSIMULATIONOBJECTFACTORY_H
#define SVMITKSIMULATIONOBJECTFACTORY_H

#include <svSimulationExports.h>

#include "svMitkSimJobIO.h"

#include "mitkCoreObjectFactoryBase.h"

class SVSIMULATION_EXPORT svMitkSimulationObjectFactory : public mitk::CoreObjectFactoryBase
{
public:
    mitkClassMacro(svMitkSimulationObjectFactory,mitk::CoreObjectFactoryBase);
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
    svMitkSimulationObjectFactory();
    ~svMitkSimulationObjectFactory();
    void CreateFileExtensionsMap();
    MultimapType m_FileExtensionsMap;
    MultimapType m_SaveFileExtensionsMap;

private:

};

struct SVSIMULATION_EXPORT RegistersvMitkSimulationObjectFactory{
  RegistersvMitkSimulationObjectFactory();

  virtual ~RegistersvMitkSimulationObjectFactory();

  svMitkSimulationObjectFactory::Pointer m_Factory;
  svMitkSimJobIO* m_MitkSimJobIO;
};

#endif // SVMITKSIMULATIONOBJECTFACTORY_H
