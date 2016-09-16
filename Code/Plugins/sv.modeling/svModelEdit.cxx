#include "svModelEdit.h"
#include "ui_svModelEdit.h"
#include "ui_svSegSelectionWidget.h"

#include "svModelUtils.h"

//#include "SimVascular.h"
//#include "cv_sys_geom.h"
//#include "cvPolyData.h"
//#include "cv_polydatasolid_utils.h"

#include <mitkNodePredicateDataType.h>
#include <mitkSurface.h>
#include <svModel.h>

#include <QTreeView>
#include <QStandardItemModel>

#include <iostream>
using namespace std;

const QString svModelEdit::EXTENSION_ID = "sv.modeledit";

svModelEdit::svModelEdit() :
    ui(new Ui::svModelEdit)
{
    m_Model=NULL;
    m_ModelNode=NULL;
}

svModelEdit::~svModelEdit()
{
    for (std::vector< std::pair< QmitkNodeDescriptor*, QAction* > >::iterator it = mDescriptorActionList.begin();it != mDescriptorActionList.end(); it++)
    {
        (it->first)->RemoveAction(it->second);
    }

    delete ui;
}

void svModelEdit::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);

    connect(GetDataManager()->GetTreeView(), SIGNAL(doubleClicked(const QModelIndex &)), this, SLOT(ShowModelEditPaneForModel()));

    QmitkNodeDescriptor* dataNodeDescriptor = getNodeDescriptorManager()->GetDescriptor("svModel");
    QAction* action = new QAction(QIcon(":modeledit.png"), "Edit Model", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( ShowModelEditPane() ) );
    dataNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(dataNodeDescriptor, action));

    connect(ui->btnUpdateModel, SIGNAL(clicked()), this, SLOT(ShowSegSelectionWidget()) );

    m_SegSelectionWidget=new svSegSelectionWidget();
    m_SegSelectionWidget->move(400,400);
    m_SegSelectionWidget->hide();
    connect(m_SegSelectionWidget->ui->buttonBox,SIGNAL(accepted()), this, SLOT(CreateModel()));
    connect(m_SegSelectionWidget->ui->buttonBox,SIGNAL(rejected()), this, SLOT(HideSegSelectionWidget()));

}

void svModelEdit::Activated()
{

    OnSelectionChanged(GetCurrentSelection());
}

void svModelEdit::Deactivated()
{

    ClearAll();
}

void svModelEdit::OnSelectionChanged(const QList<mitk::DataNode::Pointer>& nodes )
{
    if(!IsActivated())
    {
        return;
    }

    if(nodes.size()==0)
    {
        ClearAll();
        setEnabled(false);
        return;
    }

    mitk::DataNode::Pointer modelNode=nodes.front();

    if(m_ModelNode==modelNode)
    {
        return;
    }

    ClearAll();

    m_ModelNode=modelNode;
    m_Model=dynamic_cast<svModel*>(modelNode->GetData());
    if(!m_Model)
    {
        ClearAll();
        setEnabled(false);
        return;
    }

    setEnabled(true);

    ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));

    svModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement)
        ui->labelModelType->setText(QString::fromStdString(modelElement->GetType()));

}

void svModelEdit::NodeChanged(const mitk::DataNode* node)
{
}

void svModelEdit::NodeAdded(const mitk::DataNode* node)
{
}

void svModelEdit::NodeRemoved(const mitk::DataNode* node)
{
    OnSelectionChanged(GetCurrentSelection());
}

void svModelEdit::ShowModelEditPane()
{
    useExtension(EXTENSION_ID);
}

void svModelEdit::ShowModelEditPaneForModel()
{
    QList<mitk::DataNode::Pointer> selectedNodes=GetCurrentSelection();
    if(IsModel(selectedNodes))
    {
        ShowModelEditPane();
    }
}

bool svModelEdit::IsModel(QList<mitk::DataNode::Pointer> nodes)
{
    if(!nodes.isEmpty())
    {
        mitk::DataNode::Pointer node=nodes.first();
        if( node.IsNotNull() && dynamic_cast<svModel*>(node->GetData()) )
        {
            return true;
        }

    }
    return false;
}

void svModelEdit::ClearAll()
{
    m_Model=NULL;
    m_ModelNode=NULL;

    ui->labelModelName->setText("");
    ui->labelModelType->setText("");
}

void svModelEdit::ShowSegSelectionWidget()
{

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSubset(isProjFolder);

    if(rs->size()<1) return;

    mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

    rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svSegmentationFolder"));
    if(rs->size()<1) return;

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);
    rs=GetDataStorage()->GetDerivations(segFolderNode);
    if(rs->size()<1) return;

    std::vector<mitk::DataNode::Pointer> segNodes;
    for(int i=0;i<rs->size();i++)
        segNodes.push_back(rs->GetElement(i));

    int segNum=segNodes.size();

    QStandardItemModel *model;
    // QStandardItemModel(int rows, int columns, QObject * parent = 0)
    model = new QStandardItemModel(segNum,2,this);

    svModelElement* modelElement=m_Model->GetModelElement();

    for(int row = 0; row < segNum; row++)
    {
        for(int col = 0; col < 2; col++)
        {
            QModelIndex index
                    = model->index(row,col,QModelIndex());
//            if(col==0)
//            {
//                model->setData(index,QString::fromStdString(segNodes[row]->GetName()));
//            }
//            else if(col==1)
//            {
//                if(modelElement&&modelElement->HasSeg(segNodes[row]->GetName()))
//                    model->setData(index,true);
//                else
//                    model->setData(index,false);
//            }
            if(col==0)
            {
                QStandardItem* item= new QStandardItem(QString::fromStdString(segNodes[row]->GetName()));
                model->setItem(row,col,item);
            }
            else if(col==1)
            {
                if(modelElement&&modelElement->HasSeg(segNodes[row]->GetName()))
                {
                    QStandardItem* item= new QStandardItem(true);
                    item->setCheckable(true);
                    item->setCheckState(Qt::Checked);
                    model->setItem(row,col,item);
                }
                else
                {
                    QStandardItem* item= new QStandardItem(false);
                    item->setCheckable(true);
                    item->setCheckState(Qt::Unchecked);
                    model->setItem(row,col,item);
                }

            }
        }
    }

    QStringList headers;
    headers << "Segmentation" << "Use";
    model->setHorizontalHeaderLabels(headers);

    m_SegSelectionWidget->ui->tableView->setModel(model);
    m_SegSelectionWidget->ui->tableView->setColumnWidth(0,150);
    m_SegSelectionWidget->show();
}

void svModelEdit::HideSegSelectionWidget()
{
    m_SegSelectionWidget->hide();
}

void svModelEdit::CreateModel()
{
    std::vector<std::string> segNames;
    int rowCount=m_SegSelectionWidget->ui->tableView->model()->rowCount(QModelIndex());
    for(int i=0;i<rowCount;i++)
    {
        QModelIndex index= m_SegSelectionWidget->ui->tableView->model()->index(i,1, QModelIndex());
        if(index.data(Qt::CheckStateRole) == Qt::Checked){
            QModelIndex idx= m_SegSelectionWidget->ui->tableView->model()->index(i,0, QModelIndex());
            segNames.push_back(idx.data().toString().toStdString());
        }
    }

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSubset(isProjFolder);

    if(rs->size()<1) return;

    mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

    rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svSegmentationFolder"));
    if(rs->size()<1) return;

//    mitk::DataNode * 	GetNamedDerivedNode (const char *name, const mitk::DataNode *sourceNode, bool onlyDirectDerivations=true) const

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);
//    rs=GetDataStorage()->GetDerivations(segFolderNode);
//    if(rs->size()<1) return;

    std::vector<mitk::DataNode::Pointer> segNodes;
//    for(int i=0;i<rs->size();i++)
//        segNodes.push_back(rs->GetElement(i));

    for(int i=0;i<segNames.size();i++)
    {
        mitk::DataNode::Pointer node=GetDataStorage()->GetNamedDerivedNode(segNames[i].c_str(),segFolderNode);
        if(node.IsNotNull())
            segNodes.push_back(node);
    }

    svModelElement* modelElement=svModelUtils::CreateSolidModelElement(segNodes);

    m_Model->SetModelElement(modelElement);

    mitk::DataStorage::SetOfObjects::ConstPointer nodesToRemove=GetDataStorage()->GetDerivations(m_ModelNode,nullptr,false);
    if( !nodesToRemove->empty())
    {
        GetDataStorage()->Remove(nodesToRemove);
    }

    std::vector<svModelElement::svFace*> faces=modelElement->GetFaces();

    for(int i=0;i<faces.size();i++)
    {
          mitk::Surface::Pointer surface=mitk::Surface::New();
          surface->SetVtkPolyData((faces[i]->vpd));
          mitk::DataNode::Pointer surfaceNode = mitk::DataNode::New();
          surfaceNode->SetData(surface);
          surfaceNode->SetName(faces[i]->name);
          GetDataStorage()->Add(surfaceNode,m_ModelNode);

    }


    HideSegSelectionWidget();
}

//void svModelEdit::UpdateModel()
//{
//    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
//    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSubset(isProjFolder);

//    std::vector<svContourGroup*> groups;
//    std::vector<std::string> names;

//    if(rs->size()>0)
//    {
//        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

//        rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svSegmentationFolder"));
//        if(rs->size()>0)
//        {
//            mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);
//            rs=GetDataStorage()->GetDerivations(segFolderNode);
//            //if(rs->size()<1) return;

//            for(int i=0;i<rs->size();i++)
//            {
//                mitk::DataNode::Pointer segNode=rs->GetElement(i);
//                svContourGroup* group = dynamic_cast<svContourGroup*>(segNode->GetData());
//                if(group!=NULL)
//                {
//                    groups.push_back(group);
//                    names.push_back(segNode->GetName());
//                }
//            }

//        }

//    }

//    if(groups.size()==0) return;

//    vtkPolyData* solidvpd=svModelUtils::CreateSolidModelPolyData(groups);
//    if(solidvpd==NULL) return;

//    m_Model->SetVtkPolyData(solidvpd);

//    int *doublecaps;
//    int numfaces=0;

//    cvPolyData *src=new cvPolyData(solidvpd);
//    cvPolyData *dst = NULL;

//    sys_geom_set_ids_for_caps(src, &dst,  &doublecaps,&numfaces);

//    solidvpd=dst->GetVtkPolyData();
//    m_Model->SetVtkPolyData(solidvpd);

//    int totalNumFaces=0;
//    for(int i=0;i<numfaces;i++)
//    {
//        totalNumFaces=totalNumFaces+doublecaps[i]+2;
//    }
//    string *allNames=new string[totalNumFaces];

//    for(int i=0;i<numfaces;i++)
//    {
//        allNames[i]="wall_"+names[i];
//        allNames[i+numfaces]="cap_"+names[i];
//        if(doublecaps[i]!=0)
//            allNames[2*numfaces]="cap_"+names[i]+"_2";
//    }

//    cout<<allNames[0]<<","<<allNames[1]<<","<<allNames[2]<<","<<allNames[3]<<","<<allNames[4]<<endl;

//    int numBoundaryRegions;
//    int* faceIds=NULL;
//    PlyDtaUtils_GetFaceIds( solidvpd, &numBoundaryRegions, &faceIds);

//    cout<<numBoundaryRegions<<endl;
//    cout<<faceIds[0]<<","<<faceIds[1]<<","<<faceIds[2]<<","<<faceIds[3]<<","<<faceIds[4]<<endl;


//    for(int i=0;i<totalNumFaces;i++)
//    {
//          vtkPolyData *facepd = vtkPolyData::New();
//          int faceid=i+1;
//          PlyDtaUtils_GetFacePolyData(solidvpd, &faceid, facepd);

//          mitk::Surface::Pointer face=mitk::Surface::New();
//          face->SetVtkPolyData((facepd));
//          mitk::DataNode::Pointer facelNode = mitk::DataNode::New();
//          facelNode->SetData(face);
//          facelNode->SetName(allNames[i]);
//          GetDataStorage()->Add(facelNode,m_ModelNode);

//    }

//}
