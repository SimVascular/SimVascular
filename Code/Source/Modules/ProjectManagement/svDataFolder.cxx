#include "svDataFolder.h"

svDataFolder::svDataFolder()
{
    this->InitializeEmpty();
}

svDataFolder::svDataFolder(const svDataFolder &other)
    : BaseData(other)
{
}

svDataFolder::~svDataFolder()
{
//    this->ClearData();
}

//void svDataFolder::ClearData()
//{
////    Superclass::ClearData();
//}

void svDataFolder::InitializeEmpty()
{
    //    Superclass::InitializeTimeGeometry(1);
    //    m_Initialized = true;
}

void svDataFolder::SetRequestedRegionToLargestPossibleRegion()
{
}

bool svDataFolder::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool svDataFolder::VerifyRequestedRegion()
{
    return false;
}

void svDataFolder::SetRequestedRegion(const DataObject * )
{
}

std::vector<std::string> svDataFolder::GetNodeNamesToRemove()
{
    return m_NodeNamesToRemove;
}

void svDataFolder::AddToRemoveList(std::string nodeName)
{
    m_NodeNamesToRemove.push_back(nodeName);
}

void svDataFolder::ClearRemoveList()
{
    m_NodeNamesToRemove.clear();
}
