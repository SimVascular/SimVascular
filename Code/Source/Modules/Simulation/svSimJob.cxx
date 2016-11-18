#include "svSimJob.h"

svSimJob::svSimJob()
    :m_PrescribedCapNumber(0)
{
}

svSimJob::svSimJob(const svSimJob &other)
    : m_BasicProps(other.m_BasicProps)
    , m_InletProps(other.m_InletProps)
    , m_OutletProps(other.m_OutletProps)
    , m_WallProps(other.m_WallProps)
    , m_SolverProps(other.m_SolverProps)
    , m_RunProps(other.m_RunProps)
{
}

svSimJob::~svSimJob()
{
}

svSimJob* svSimJob::Clone()
{
    return new svSimJob(*this);
}

void svSimJob::SetBasicProps(std::map<std::string,std::string> basicProps)
{
    m_BasicProps=basicProps;
}

std::map<std::string,std::string> svSimJob::GetBasicProps() const
{
    return m_BasicProps;
}

void svSimJob::SetBasicProp(const std::string& key, std::string value)
{
    m_BasicProps[key]=value;
}

std::string svSimJob::GetBasicProp(const std::string& key) const
{
    return m_BasicProps[key];
}

void svSimJob::SetInletProps(std::map<std::string,std::map<std::string,std::string>> inletProps)
{
    m_InletProps=inletProps;
}

std::map<std::string,std::map<std::string,std::string>> svSimJob::GetInletProps() const
{
    return m_InletProps;
}

void svSimJob::SetInletProp(const std::string& inletName, const std::string& key, std::string value)
{
    m_InletProps[inletName][key]=value;
}

std::string svSimJob::GetInletProp(const std::string& inletName, const std::string& key) const
{
    return m_InletProps[inletName][key];
}

void svSimJob::SetOutletProps(std::map<std::string,std::map<std::string,std::string>> outletProps)
{
    m_OutletProps=outletProps;
}

std::map<std::string,std::map<std::string,std::string>> svSimJob::GetOutletProps() const
{
    return m_OutletProps;
}

void svSimJob::SetOutletProp(const std::string& outletName, const std::string& key, std::string value)
{
    m_OutletProps[outletName][key]=value;
}

std::string svSimJob::GetOutletProp(const std::string& outletName, const std::string& key) const
{
    return m_OutletProps[outletName][key];
}

void svSimJob::SetWallProps(std::map<std::string,std::string> wallProps)
{
    m_WallProps=wallProps;
}

std::map<std::string,std::string> svSimJob::GetWallProps() const
{
    return m_WallProps;
}

void svSimJob::SetWallProp(const std::string& key, std::string value)
{
    m_WallProps[key]=value;
}

std::string svSimJob::GetWallProp(const std::string& key) const
{
    return m_WallProps[key];
}

void svSimJob::SetVarProps(std::map<std::string,std::map<std::string,std::string>> varProps)
{
    m_VarProps=varProps;
}

std::map<std::string,std::map<std::string,std::string>> svSimJob::GetVarProps() const
{
    return m_VarProps;
}

void svSimJob::SetVarProp(const std::string& faceName, const std::string& key, std::string value)
{
    m_VarProps[faceName][key]=value;
}

std::string svSimJob::GetVarProp(const std::string& faceName, const std::string& key) const
{
    return m_VarProps[faceName][key];
}

void svSimJob::SetSolverProps(std::map<std::string,std::string> solverProps)
{
    m_SolverProps=solverProps;
}

std::map<std::string,std::string> svSimJob::GetSolverProps() const
{
    return m_SolverProps;
}

void svSimJob::SetSolverProp(const std::string& key, std::string value)
{
    m_SolverProps[key]=value;
}

std::string svSimJob::GetSolverProp(const std::string& key) const
{
    return m_SolverProps[key];
}

void svSimJob::SetRunProps(std::map<std::string,std::string> runProps)
{
    m_RunProps=runProps;
}

std::map<std::string,std::string> svSimJob::GetRunProps() const
{
    return m_RunProps;
}

void svSimJob::SetRunProp(const std::string& key, std::string value)
{
    m_RunProps[key]=value;
}

std::string svSimJob::GetRunProp(const std::string& key) const
{
    return m_RunProps[key];
}

void svSimJob::SetIDs(std::map<std::string,int> IDs)
{
    m_IDs=IDs;
}

std::map<std::string,int> svSimJob::GetIDs() const
{
    return m_IDs;
}

void svSimJob::SetPrescribedCapNumber(int number)
{
    m_PrescribedCapNumber=number;
}

int svSimJob::GetPrescribedCapNumber()
{
    return m_PrescribedCapNumber;
}
