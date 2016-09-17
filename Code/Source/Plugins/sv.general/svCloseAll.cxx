#include "svCloseAll.h"

#include <mitkNodePredicateNot.h>
#include <mitkNodePredicateProperty.h>

#include <QDir>
#include <QMessageBox>


const QString svCloseAll::EXTENSION_ID = "sv.closeall";

svCloseAll::svCloseAll()
{
}

svCloseAll::~svCloseAll()
{
}

void svCloseAll::Exec()
{
    try
    {
        mitk::DataStorage::Pointer dataStorage = GetDataStorage();

        if( dataStorage->GetSubset(mitk::NodePredicateNot::New(mitk::NodePredicateProperty::New("helper object", mitk::BoolProperty::New(true))))->empty())
        {
          return;
        }

        QString msg = "Are you sure that you want to close all data objects without saving?";
        if (QMessageBox::question(NULL, "Close all data?", msg,
                                  QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
        {
          return;
        }

        mitk::DataStorage::SetOfObjects::ConstPointer nodesToRemove = dataStorage->GetAll();
        dataStorage->Remove(nodesToRemove);

    }
    catch (std::exception& e)
    {
      MITK_ERROR << "Exception caught during closing project: " << e.what();
      QMessageBox::warning(NULL, "Error", QString("An error occurred during Close All Data: %1").arg(e.what()));
    }

}
