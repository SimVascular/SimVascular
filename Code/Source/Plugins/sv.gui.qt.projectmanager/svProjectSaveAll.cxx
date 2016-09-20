#include "svProjectSaveAll.h"
#include "svProjectManager.h"

const QString svProjectSaveAll::EXTENSION_ID = "sv.projectsaveall";

svProjectSaveAll::svProjectSaveAll()
{
}

svProjectSaveAll::~svProjectSaveAll()
{
}

void svProjectSaveAll::Exec()
{
    svProjectManager::SaveAllProjects(GetDataStorage());
}
