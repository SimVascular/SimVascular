#include "svSaveScene.h"

#include <QmitkIOUtil.h>

#include <mitkIOUtil.h>
#include <mitkSceneIO.h>
#include <mitkProgressBar.h>
#include <mitkNodePredicateNot.h>
#include <mitkNodePredicateProperty.h>

#include <QDir>
#include <QFileDialog>
#include <QMessageBox>


const QString svSaveScene::EXTENSION_ID = "sv.savescene";

svSaveScene::svSaveScene()
{
}

svSaveScene::~svSaveScene()
{
}

void svSaveScene::Exec()
{

    try
    {

        mitk::DataStorage::Pointer storage = GetDataStorage();

        QString fileName = QFileDialog::getSaveFileName(NULL,
                                                        "Save MITK Scene",
                                                        QDir::homePath(),
                                                        "MITK scene files (*.mitk)",
                                                        NULL,QFileDialog::DontUseNativeDialog);

        if (fileName.isEmpty() )
            return;

        if ( fileName.right(5) != ".mitk" )
            fileName += ".mitk";

        //Contructing SceneIO leads to double free or segmentation fault when quitting application.
        mitk::SceneIO::Pointer sceneIO = mitk::SceneIO::New();

        mitk::ProgressBar::GetInstance()->AddStepsToDo(2);

        /* Build list of nodes that should be saved */
        mitk::NodePredicateNot::Pointer isNotHelperObject =
                mitk::NodePredicateNot::New(mitk::NodePredicateProperty::New("helper object", mitk::BoolProperty::New(true)));
        mitk::DataStorage::SetOfObjects::ConstPointer nodesToBeSaved = storage->GetSubset(isNotHelperObject);

        if ( !sceneIO->SaveScene( nodesToBeSaved, storage, fileName.toStdString() ) )
        {
            QMessageBox::information(NULL,
                                     "Scene saving",
                                     "Scene could not be written completely. Please check the log.",
                                     QMessageBox::Ok);

        }
        mitk::ProgressBar::GetInstance()->Progress(2);

        mitk::SceneIO::FailedBaseDataListType::ConstPointer failedNodes = sceneIO->GetFailedNodes();
        if (!failedNodes->empty())
        {
            std::stringstream ss;
            ss << "The following nodes could not be serialized:" << std::endl;
            for ( mitk::SceneIO::FailedBaseDataListType::const_iterator iter = failedNodes->begin();
                  iter != failedNodes->end();
                  ++iter )
            {
                ss << " - ";
                if ( mitk::BaseData* data =(*iter)->GetData() )
                {
                    ss << data->GetNameOfClass();
                }
                else
                {
                    ss << "(NULL)";
                }

                ss << " contained in node '" << (*iter)->GetName() << "'" << std::endl;
            }

            MITK_WARN << ss.str();
        }

        mitk::PropertyList::ConstPointer failedProperties = sceneIO->GetFailedProperties();
        if (!failedProperties->GetMap()->empty())
        {
            std::stringstream ss;
            ss << "The following properties could not be serialized:" << std::endl;
            const mitk::PropertyList::PropertyMap* propmap = failedProperties->GetMap();
            for ( mitk::PropertyList::PropertyMap::const_iterator iter = propmap->begin();
                  iter != propmap->end();
                  ++iter )
            {
                ss << " - " << iter->second->GetNameOfClass() << " associated to key '" << iter->first << "'" << std::endl;
            }

            MITK_WARN << ss.str();
        }
    }
    catch (std::exception& e)
    {
        MITK_ERROR << "Exception caught during scene saving: " << e.what();
    }

}
