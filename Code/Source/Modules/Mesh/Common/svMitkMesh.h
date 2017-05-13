#ifndef SVMITKMESH_H
#define SVMITKMESH_H

#include <svMeshExports.h>

#include "svMesh.h"
#include "svMitkMeshOperation.h"

#include "mitkBaseData.h"

class SVMESH_EXPORT svMitkMesh : public mitk::BaseData
{
public:

    mitkClassMacro(svMitkMesh, mitk::BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual void Expand( unsigned int timeSteps = 1 ) override;

    virtual void ExecuteOperation(mitk::Operation *operation) override;

    virtual bool IsEmptyTimeStep(unsigned int t) const override;

    virtual unsigned int GetTimeSize() const;

    svMesh* GetMesh(unsigned int t=0) const;

    void SetMesh(svMesh* mesh, unsigned int t=0);

    void CalculateBoundingBox(double *bounds,unsigned int t = 0 );

    void SetType(std::string type);

    std::string GetType() const;

    std::string GetModelName() const;

    void SetModelName(std::string name);

    virtual void UpdateOutputInformation() override;
    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual bool VerifyRequestedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;

    bool IsDataModified(){return m_DataModified;}
    void SetDataModified(bool modified = true){m_DataModified=modified;}

  protected:

    mitkCloneMacro(Self);

    svMitkMesh();
    svMitkMesh(const svMitkMesh &other);
    virtual ~svMitkMesh();

    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
    virtual void ClearData() override;
    virtual void InitializeEmpty() override;

    std::vector< svMesh* > m_MeshSet;

    bool m_CalculateBoundingBox;

    std::string m_Type;

    std::string m_ModelName;

    bool m_DataModified;
};

itkEventMacro( svMitkMeshEvent, itk::AnyEvent );

itkEventMacro( svMitkMeshExtendTimeRangeEvent, svMitkMeshEvent );
itkEventMacro( svMitkMeshSetEvent, svMitkMeshEvent );

#endif // SVMITKMESH_H
