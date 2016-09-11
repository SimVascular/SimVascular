#include "svGeneralInit.h"
#include "svDataFolder.h"

#include "svProjectManager.h"

#include <QmitkNodeDescriptorManager.h>
#include <mitkNodePredicateDataType.h>
#include <mitkIOUtil.h>

#include <QMessageBox>
#include <QInputDialog>
#include <QFileDialog>

const QString svGeneralInit::EXTENSION_ID = "sv.generalinit";

svGeneralInit::svGeneralInit()
{
    mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    QmitkNodeDescriptorManager::GetInstance()->AddDescriptor(new QmitkNodeDescriptor(tr("svProjectFolder"), QString(":svprojectfolder.png"), isProjectFolder, this));

    mitk::NodePredicateDataType::Pointer isImageFolder = mitk::NodePredicateDataType::New("svImageFolder");
    QmitkNodeDescriptorManager::GetInstance()->AddDescriptor(new QmitkNodeDescriptor(tr("svImageFolder"), QString(":svimagefolder.png"), isImageFolder, this));

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");
    QmitkNodeDescriptorManager::GetInstance()->AddDescriptor(new QmitkNodeDescriptor(tr("svPathFolder"), QString(":svpathfolder.png"), isPathFolder, this));

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("svPath");
    QmitkNodeDescriptorManager::GetInstance()->AddDescriptor(new QmitkNodeDescriptor(tr("svPath"), QString(":svpath.png"), isPath, this));

    mitk::NodePredicateDataType::Pointer isSegmentationFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");
    QmitkNodeDescriptorManager::GetInstance()->AddDescriptor(new QmitkNodeDescriptor(tr("svSegmentationFolder"), QString(":svsegfolder.png"), isSegmentationFolder, this));

    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("svContourGroup");
    QmitkNodeDescriptorManager::GetInstance()->AddDescriptor(new QmitkNodeDescriptor(tr("svContourGroup"), QString(":svcontourgroup.png"), isContourGroup, this));

    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("svModelFolder");
    QmitkNodeDescriptorManager::GetInstance()->AddDescriptor(new QmitkNodeDescriptor(tr("svModelFolder"), QString(":svmodelfolder.png"), isModelFolder, this));

    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");
    QmitkNodeDescriptorManager::GetInstance()->AddDescriptor(new QmitkNodeDescriptor(tr("svModel"), QString(":svmodel.png"), isModel, this));

    mitk::NodePredicateDataType::Pointer isMeshFolder = mitk::NodePredicateDataType::New("svMeshFolder");
    QmitkNodeDescriptorManager::GetInstance()->AddDescriptor(new QmitkNodeDescriptor(tr("svMeshFolder"), QString(":svmeshfolder.png"), isMeshFolder, this));

    mitk::NodePredicateDataType::Pointer isSimulationFolder = mitk::NodePredicateDataType::New("svSimulationFolder");
    QmitkNodeDescriptorManager::GetInstance()->AddDescriptor(new QmitkNodeDescriptor(tr("svSimulationFolder"), QString(":svsimfolder.png"), isSimulationFolder, this));


    QmitkNodeDescriptor* unknownDataNodeDescriptor =  QmitkNodeDescriptorManager::GetInstance()->GetUnknownDataNodeDescriptor();
    QAction* removeAction = new QAction(QIcon(":Remove.png"), "Remove", this);
    QObject::connect( removeAction, SIGNAL( triggered(bool) ) , this, SLOT( RemoveSelectedNodes(bool) ) );
    unknownDataNodeDescriptor->AddAction(removeAction);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(unknownDataNodeDescriptor,removeAction));

    QAction* renameAction = new QAction(QIcon(":Rename.png"), "Rename", this);
    QObject::connect( renameAction, SIGNAL( triggered(bool) ) , this, SLOT( RenameSelectedNode(bool) ) );
    unknownDataNodeDescriptor->AddAction(renameAction,false);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(unknownDataNodeDescriptor,renameAction));

    QmitkNodeDescriptor* projectNodeDescriptor = getNodeDescriptorManager()->GetDescriptor("svProjectFolder");
    QAction* closeAction = new QAction(QIcon(":closeproject.png"), "Close Project", this);
    QObject::connect( closeAction, SIGNAL( triggered(bool) ) , this, SLOT( CloseProject(bool) ) );
    projectNodeDescriptor->AddAction(closeAction,false);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(projectNodeDescriptor, closeAction));


    QmitkNodeDescriptor* folderNodeDescriptor = getNodeDescriptorManager()->GetDescriptor("svImageFolder");
    QAction* action = new QAction(QIcon(":addimage.png"), "Add Image", this);
    QObject::connect( action, SIGNAL( triggered(bool) ) , this, SLOT( AddImage(bool) ) );
    folderNodeDescriptor->AddAction(action,false);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(folderNodeDescriptor, action));

}

svGeneralInit::~svGeneralInit()
{
    for (std::vector< std::pair< QmitkNodeDescriptor*, QAction* > >::iterator it = m_DescriptorActionList.begin();it != m_DescriptorActionList.end(); it++)
    {
        // first== the NodeDescriptor; second== the registered QAction
        (it->first)->RemoveAction(it->second);
    }
}


void svGeneralInit::RemoveSelectedNodes( bool )
{

    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer node = 0;

    std::vector<mitk::DataNode::Pointer> selectedNodes;

    QString question = tr("Do you really want to remove ");

    mitk::TNodePredicateDataType<svDataFolder>::Pointer isDataFolder= mitk::TNodePredicateDataType<svDataFolder>::New();

    for (int i=0;i<nodes.size();i++)
    {
        node = nodes[i];
        if ( node.IsNotNull() && !isDataFolder->CheckNode(node)/*& strcmp(node->GetData()->GetNameOfClass(), "PlaneGeometryData") != 0*/ )
        {
            selectedNodes.push_back(node);
            question.append(QString::fromStdString(node->GetName()));
            question.append(", ");
        }
    }
    // remove the last two characters = ", "
    question = question.remove(question.size()-2, 2);
    question.append(" from data storage?");

    if(selectedNodes.size()==0)
    {
        return;
    }

    QMessageBox::StandardButton answerButton = QMessageBox::question( this
                                                                      , tr("DataManager")
                                                                      , question
                                                                      , QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes);

    if(answerButton == QMessageBox::Yes)
    {


        for (std::vector<mitk::DataNode::Pointer>::iterator it = selectedNodes.begin()
             ; it != selectedNodes.end(); it++)
        {
            node = *it;
            if( !isDataFolder->CheckNode(node))
            {
                GetDataStorage()->Remove(node);
            }
            //      if (m_GlobalReinitOnNodeDelete)
            //          this->GlobalReinit(false);
        }
    }
}


void svGeneralInit::RenameSelectedNode( bool )
{

    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer node = nodes.front();

    if ( node.IsNotNull())
    {
        mitk::TNodePredicateDataType<svDataFolder>::Pointer isDataFolder= mitk::TNodePredicateDataType<svDataFolder>::New();
        if( isDataFolder->CheckNode(node))
        {
            return;
        }

        bool ok;
        QString text = QInputDialog::getText(this, tr("Rename"),
                                             tr("New Name:"), QLineEdit::Normal,
                                             QString::fromStdString(node->GetName()), &ok);
        QString newName=text.trimmed();
        if (ok && !newName.isEmpty())
        {
            node->SetName(newName.toStdString());

        }

    }

}

void svGeneralInit::AddImage(bool)
{
    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
    mitk::NodePredicateDataType::Pointer isImageFolder = mitk::NodePredicateDataType::New("svImageFolder");

    if(nodes.size()>0 && isImageFolder->CheckNode(nodes.front()))
    {
        mitk::DataNode::Pointer imageFolderNode=nodes.front();

        QString imageFilePath = QFileDialog::getOpenFileName(this, tr("Open File"),
                                                             QDir::homePath(),
                                                             tr("Image Files (*.vti *.nrrd *.dcm)"),NULL,QFileDialog::DontUseNativeDialog);

        if(!imageFilePath.isEmpty())
        {
            svProjectManager::AddImage(GetDataStorage(), imageFilePath, imageFolderNode);
        }

    }

}

void svGeneralInit::CloseProject(bool)
{
    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
    mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("svProjectFolder");

    if(nodes.size()>0 && isProjectFolder->CheckNode(nodes.front()))
    {

        QString msg = "Are you sure that you want to close the project "+QString::fromStdString(nodes.front()->GetName())+"?";
        if (QMessageBox::question(NULL, "Close Project", msg,
                                  QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
        {
            return;
        }

        try
        {
            mitk::DataStorage::Pointer dataStorage = GetDataStorage();

            mitk::DataStorage::SetOfObjects::ConstPointer nodesToRemove=dataStorage->GetDerivations(nodes.front(),nullptr,false);

            if( !nodesToRemove->empty())
            {
                dataStorage->Remove(nodesToRemove);
            }

            dataStorage->Remove(nodes.front());

        }
        catch (std::exception& e)
        {
            MITK_ERROR << "Exception caught during closing project: " << e.what();
            QMessageBox::warning(NULL, "Error", QString("An error occurred during Close Project: %1").arg(e.what()));
        }

    }




}
