#ifndef sv4guiMitksvFSIOBJECTFACTORY_H
#define sv4guiMitksvFSIOBJECTFACTORY_H

#include <svFSIExports.h>

#include "sv4gui_MitksvFSIJobIO.h"

#include <mitkCoreObjectFactoryBase.h>

class SVFSI_EXPORT sv4guiMitksvFSIObjectFactory : public mitk::CoreObjectFactoryBase
{
public:
    mitkClassMacro(sv4guiMitksvFSIObjectFactory,mitk::CoreObjectFactoryBase);
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
    sv4guiMitksvFSIObjectFactory();
    ~sv4guiMitksvFSIObjectFactory();
    void CreateFileExtensionsMap();
    MultimapType m_FileExtensionsMap;
    MultimapType m_SaveFileExtensionsMap;

private:

};

struct SVFSI_EXPORT Registersv4guiMitksvFSIObjectFactory{
  Registersv4guiMitksvFSIObjectFactory();

  virtual ~Registersv4guiMitksvFSIObjectFactory();

  sv4guiMitksvFSIObjectFactory::Pointer m_Factory;
  sv4guiMitksvFSIJobIO* m_MitkSimJobIO;
};

#endif // sv4guiMitksvFSIOBJECTFACTORY_H
