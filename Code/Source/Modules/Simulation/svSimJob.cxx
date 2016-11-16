#include "svSimJob.h"

#include <map>

svSimJob::svSimJob()
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

std::map<std::string,std::string>& svSimJob::GetBasicProps()
{
    return m_BasicProps;
}

void svSimJob::SetBasicProp(const std::string& key, std::string value)
{
    m_BasicProps[key]=value;
}

std::string svSimJob::GetBasicProp(const std::string& key)
{
    return m_BasicProps[key];
}

std::map<std::string,std::map<std::string,std::string>>& svSimJob::GetInletProps()
{
    return m_InletProps;
}

void svSimJob::SetInletProp(const std::string& inletName, const std::string& key, std::string value)
{
    m_InletProps[inletName][key]=value;
}

std::string svSimJob::GetInletProp(const std::string& inletName, const std::string& key)
{
    return m_InletProps[inletName][key];
}

std::map<std::string,std::string>& svSimJob::GetOutletProps()
{
    return m_OutletProps;
}

void svSimJob::SetOutletProp(const std::string& key, std::string value)
{
    m_OutletProps[key]=value;
}

std::string svSimJob::GetOutletProp(const std::string& key)
{
    return m_OutletProps[key];
}

std::map<std::string,std::string>& svSimJob::GetWallProps()
{
    return m_WallProps;
}

void svSimJob::SetWallProp(const std::string& key, std::string value)
{
    m_WallProps[key]=value;
}

std::string svSimJob::GetWallProp(const std::string& key)
{
    return m_WallProps[key];
}

std::map<std::string,std::string>& svSimJob::GetSolverProps()
{
    return m_SolverProps;
}

void svSimJob::SetSolverProp(const std::string& key, std::string value)
{
    m_SolverProps[key]=value;
}

std::string svSimJob::GetSolverProp(const std::string& key)
{
    return m_SolverProps[key];
}

std::map<std::string,std::string>& svSimJob::GetRunProps()
{
    return m_RunProps;
}

void svSimJob::SetRunProp(const std::string& key, std::string value)
{
    m_RunProps[key]=value;
}

std::string svSimJob::GetRunProp(const std::string& key)
{
    return m_RunProps[key];
}
