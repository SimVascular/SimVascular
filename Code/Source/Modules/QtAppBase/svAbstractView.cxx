
#include "svAbstractView.h"
#include "svApplication.h"

#include "QmitkDataNodeItemModel.h"

// Qt Includes
#include <QItemSelectionModel>
#include <QApplication>
#include <QMessageBox>
//#include <QScrollArea>
//#include <QVBoxLayout>
#include <QTreeView>

#include <iostream>
using namespace std;

class svAbstractViewPrivate
{
public:

  svAbstractViewPrivate()
    : m_DataNodeItemModel(new QmitkDataNodeItemModel)
    , m_DataNodeSelectionModel(new QItemSelectionModel(m_DataNodeItemModel))
  {
  }

  ~svAbstractViewPrivate()
  {
    delete m_DataNodeSelectionModel;
    delete m_DataNodeItemModel;
  }

  /**
   * Holds a helper model for firing selection events.
   */
  QmitkDataNodeItemModel* m_DataNodeItemModel;

  /**
   * The selection model for the QmitkDataNodeItemModel;
   */
  QItemSelectionModel* m_DataNodeSelectionModel;
};



svAbstractView::svAbstractView()
  : dd(new svAbstractViewPrivate())
{
}

void svAbstractView::AfterCreateQtPartControl()
{
  svAbstractFunctionality::AfterCreateQtPartControl();

  QObject::connect( GetDataManager()->GetTreeView()->selectionModel()
    , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
    , this
    , SLOT( HandleOnSelectionChanged() ) );

//  QObject::connect( GetDataManager()->GetTreeView()
//    , SIGNAL(customContextMenuRequested(const QPoint&))
//    , this
//    , SLOT( AppendDataManagerMenuActions() ) );

  this->OnSelectionChanged(this->GetCurrentSelection());

}

svAbstractView::~svAbstractView()
{
    //maybe need to disconnect.
}

svQmitkDataManager* svAbstractView::GetDataManager()
{
	return svApplication::application()->dataManager();
}

QList<mitk::DataNode::Pointer> svAbstractView::GetCurrentSelection()
{
	return GetDataManager()->GetCurrentSelection();
}

bool svAbstractView::IsCurrentSelectionValid()
{
  return (GetCurrentSelection().size()>0);
}

void svAbstractView::OnSelectionChanged(const QList<mitk::DataNode::Pointer>& /*nodes*/)
{
}

void svAbstractView::HandleOnSelectionChanged()
{
    if(this->GetCurrentSelection().isEmpty())
    {
      OnNullSelection();
      return;
    }

	OnSelectionChanged(this->GetCurrentSelection());
}

void svAbstractView::OnNullSelection()
{
}


void svAbstractView::FireNodeSelected( mitk::DataNode::Pointer node )
{
  QList<mitk::DataNode::Pointer> nodes;
  nodes << node;
  this->FireNodesSelected(nodes);
}

void svAbstractView::FireNodesSelected( const QList<mitk::DataNode::Pointer>& nodes )
{

    if (nodes.empty())
  {
    dd->m_DataNodeSelectionModel->clearSelection();
    dd->m_DataNodeItemModel->clear();

  }
  else
  {

    // The helper data node model is just used for sending selection events.
    // We add the to be selected nodes and set the selection range to everything.

    dd->m_DataNodeItemModel->clear();
    foreach(mitk::DataNode::Pointer node, nodes)
    {
      dd->m_DataNodeItemModel->AddDataNode(node);
    }

    dd->m_DataNodeSelectionModel->select(QItemSelection(dd->m_DataNodeItemModel->index(0,0), dd->m_DataNodeItemModel->index(nodes.size()-1, 0)),
                                        QItemSelectionModel::ClearAndSelect);
  }
}

//void svAbstractView::AppendMenuActions(QMenu* menu)
//{
//}

//void svAbstractView::AppendDataManagerMenuActions()
//{
//    QList<mitk::DataNode::Pointer> selectedNodes = this->GetCurrentSelection();

//    if(!selectedNodes.isEmpty())
//    {
//        QMenu* menu=GetDataManager()->GetNodeMenu();
//        AppendMenuActions(menu);
//        menu->popup(QCursor::pos());
//    }
//}
