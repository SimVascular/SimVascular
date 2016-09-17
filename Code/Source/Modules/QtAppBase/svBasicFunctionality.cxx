
#include "svBasicFunctionality.h"
#include "svApplication.h"
#include "svExtensionManager.h"

// Qt Includes
#include <QApplication>
#include <QMessageBox>
//#include <QScrollArea>
//#include <QVBoxLayout>

class svBasicFunctionalityPrivate
{

public:

  svBasicFunctionalityPrivate(svBasicFunctionality* qq)
    : q(qq)
    , m_Parent(nullptr)
    , m_InDataStorageChanged(false)
  {
  }

  ~svBasicFunctionalityPrivate()
  {
  }

  /**
   * Called when a DataStorage Add Event was thrown. Sets
   * m_InDataStorageChanged to true and calls NodeAdded afterwards.
   * \see m_InDataStorageChanged
   */
  void NodeAddedProxy(const mitk::DataNode* node)
  {
    // garantuee no recursions when a new node event is thrown in NodeAdded()
    if(!m_InDataStorageChanged)
    {
      m_InDataStorageChanged = true;
      q->NodeAdded(node);
      q->DataStorageModified();
      m_InDataStorageChanged = false;
    }
  }

  /**
   * Called when a DataStorage remove event was thrown. Sets
   * m_InDataStorageChanged to true and calls NodeRemoved afterwards.
   * \see m_InDataStorageChanged
   */
  void NodeRemovedProxy(const mitk::DataNode* node)
  {
    // garantuee no recursions when a new node event is thrown in NodeAdded()
    if(!m_InDataStorageChanged)
    {
      m_InDataStorageChanged = true;
      q->NodeRemoved(node);
      q->DataStorageModified();
      m_InDataStorageChanged = false;
    }
  }

  /**
   * Called when a DataStorage changed event was thrown. Sets
   * m_InDataStorageChanged to true and calls NodeChanged afterwards.
   * \see m_InDataStorageChanged
   */
  void NodeChangedProxy(const mitk::DataNode* node)
  {
    // garantuee no recursions when a new node event is thrown in NodeAdded()
    if(!m_InDataStorageChanged)
    {
      m_InDataStorageChanged = true;
      q->NodeChanged(node);
      q->DataStorageModified();
      m_InDataStorageChanged = false;
    }
  }

  svBasicFunctionality* const q;

  /**
   * Saves the parent of this view (this is the scrollarea created in CreatePartControl(QWidget*)
   * \see CreatePartControl(QWidget*)
   */
  QWidget* m_Parent;

  /**
   * Saves if this class is currently working on DataStorage changes.
   * This is a protector variable to avoid recursive calls on event listener functions.
   */
  bool m_InDataStorageChanged;

};

svBasicFunctionality::svBasicFunctionality()
    : d(new svBasicFunctionalityPrivate(this))
{
}

//svExtensionManager* svAbstractView::getExtensionManager()
//{
//    return svApplication::application()->extensionManager();
//}

void svBasicFunctionality::CreatePartControl(QWidget* parent)
{

  // scrollArea
//  auto   scrollArea = new QScrollArea;
//  //QVBoxLayout* scrollAreaLayout = new QVBoxLayout(scrollArea);
//  scrollArea->setFrameShadow(QFrame::Plain);
//  scrollArea->setFrameShape(QFrame::NoFrame);
//  scrollArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
//  scrollArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);

  // m_Parent
//  d->m_Parent = new QWidget;
//  //m_Parent->setSizePolicy(QSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding));
//  this->CreateQtPartControl(d->m_Parent);
    if(parent){
        this->CreateQtPartControl(parent);
    }else{
        this->CreateQtPartControl(this);
    }

  //scrollAreaLayout->addWidget(m_Parent);
  //scrollArea->setLayout(scrollAreaLayout);

  // set the widget now
//  scrollArea->setWidgetResizable(true);
//  scrollArea->setWidget(d->m_Parent);

  // add the scroll area to the real parent (the view tabbar)

//  QWidget* parentQWidget;
//  if(parent==nullptr){
//	  QWidget* parentQWidget = new QWidget;
//  }else{
//	  QWidget* parentQWidget = static_cast<QWidget*>(parent);
//  }
//
//  auto   parentLayout = new QVBoxLayout(parentQWidget);
//  parentLayout->setMargin(0);
//  parentLayout->setSpacing(0);
//  parentLayout->addWidget(scrollArea);
//
//  // finally set the layout containing the scroll area to the parent widget (= show it)
//  parentQWidget->setLayout(parentLayout);
//
  this->AfterCreateQtPartControl();

//  this->SetFocus();

}

void svBasicFunctionality::AfterCreateQtPartControl()
{
//  this->SetSelectionProvider();

  // REGISTER DATASTORAGE LISTENER
  this->GetDataStorage()->AddNodeEvent.AddListener( mitk::MessageDelegate1<svBasicFunctionalityPrivate, const mitk::DataNode*>
                                                    ( d.data(), &svBasicFunctionalityPrivate::NodeAddedProxy ) );
  this->GetDataStorage()->ChangedNodeEvent.AddListener( mitk::MessageDelegate1<svBasicFunctionalityPrivate, const mitk::DataNode*>
                                                        ( d.data(), &svBasicFunctionalityPrivate::NodeChangedProxy ) );
  this->GetDataStorage()->RemoveNodeEvent.AddListener( mitk::MessageDelegate1<svBasicFunctionalityPrivate, const mitk::DataNode*>
                                                       ( d.data(), &svBasicFunctionalityPrivate::NodeRemovedProxy ) );

  //use similar way as above  to deal with preference changes in future
  // send preferences changed event
  // this->OnPreferencesChanged(this->GetPreferences());
}

svBasicFunctionality::~svBasicFunctionality()
{

  this->GetDataStorage()->AddNodeEvent.RemoveListener( mitk::MessageDelegate1<svBasicFunctionalityPrivate, const mitk::DataNode*>
                                                       ( d.data(), &svBasicFunctionalityPrivate::NodeAddedProxy ) );
  this->GetDataStorage()->RemoveNodeEvent.RemoveListener( mitk::MessageDelegate1<svBasicFunctionalityPrivate, const mitk::DataNode*>
                                                          ( d.data(), &svBasicFunctionalityPrivate::NodeRemovedProxy) );
  this->GetDataStorage()->ChangedNodeEvent.RemoveListener( mitk::MessageDelegate1<svBasicFunctionalityPrivate, const mitk::DataNode*>
                                                           ( d.data(), &svBasicFunctionalityPrivate::NodeChangedProxy ) );

    //maybe need to disconnect.
}

//void QmitkAbstractView::OnPreferencesChanged( const berry::IBerryPreferences* )
//{
//}

void svBasicFunctionality::DataStorageModified()
{
}

void svBasicFunctionality::HandleException( const char* str, QWidget* parent, bool showDialog )
{
  //itkGenericOutputMacro( << "Exception caught: " << str );
  //MITK_ERROR << str;
  if ( showDialog )
  {
    QMessageBox::critical ( parent, "Exception caught!", str );
  }
}

void svBasicFunctionality::HandleException( std::exception& e, QWidget* parent, bool showDialog )
{
  HandleException( e.what(), parent, showDialog );
}

void svBasicFunctionality::WaitCursorOn()
{
  QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );
}

void svBasicFunctionality::BusyCursorOn()
{
  QApplication::setOverrideCursor( QCursor(Qt::BusyCursor) );
}

void svBasicFunctionality::WaitCursorOff()
{
  RestoreOverrideCursor();
}

void svBasicFunctionality::BusyCursorOff()
{
  RestoreOverrideCursor();
}

void svBasicFunctionality::RestoreOverrideCursor()
{
  QApplication::restoreOverrideCursor();
}

//berry::IPreferences::Pointer QmitkAbstractView::GetPreferences() const
//{
//  berry::IPreferencesService* prefService = d->m_PrefServiceTracker.getService();
//  // const_cast workaround for bad programming: const uncorrectness this->GetViewSite() should be const
//  QString id = "/" + (const_cast<QmitkAbstractView*>(this))->GetViewSite()->GetId();
//  return prefService ? prefService->GetSystemPreferences()->Node(id): berry::IPreferences::Pointer(nullptr);
//}

mitk::DataStorage::Pointer svBasicFunctionality::GetDataStorage()
{

  return svApplication::application()->dataStorage();
}

mitk::StandaloneDataStorage::Pointer svBasicFunctionality::GetStandaloneDataStorage()
{

  return svApplication::application()->standaloneDataStorage();
}


void svBasicFunctionality::useExtension(QString extensionID){
    svApplication::application()->extensionManager()->useExtension(extensionID);
}

svAbstractExtension* svBasicFunctionality::GetExtension(QString extensionID)
{
    return svApplication::application()->extensionManager()->getExtension(extensionID);
}

QmitkNodeDescriptorManager* svBasicFunctionality::getNodeDescriptorManager(){
    return QmitkNodeDescriptorManager::GetInstance();
}

void svBasicFunctionality::NodeAdded( const mitk::DataNode*  /*node*/ )
{
}

void svBasicFunctionality::NodeRemoved( const mitk::DataNode*  /*node*/ )
{
}

void svBasicFunctionality::NodeChanged( const mitk::DataNode* /*node*/ )
{
}

void svBasicFunctionality::CreateQtPartControl(QWidget* /*parent*/)
{
}

void svBasicFunctionality::Visible()
{
}

void svBasicFunctionality::Hidden()
{
}

void svBasicFunctionality::showEvent(QShowEvent *se)
{
    svAbstractExtension::showEvent(se);
    Visible();
}

void svBasicFunctionality::hideEvent(QHideEvent *he)
{
    svAbstractExtension::hideEvent(he);
    Hidden();
}

bool svBasicFunctionality::IsActivated()
{
    return m_Active;
}

void svBasicFunctionality::SetActivated(bool activated)
{
    m_Active=activated;
}

void svBasicFunctionality::Activated()
{
}

void svBasicFunctionality::Enter()
{
	SetActivated(true);
	Activated();
}

void svBasicFunctionality::Deactivated()
{
}

void svBasicFunctionality::Leave()
{
	SetActivated(false);
	Deactivated();
}
