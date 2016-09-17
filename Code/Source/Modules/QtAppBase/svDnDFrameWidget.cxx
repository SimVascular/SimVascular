#include "svDnDFrameWidget.h"

#include <mitkIOUtil.h>

#include <QDragEnterEvent>
#include <QMimeData>

#include "svApplication.h"

svDnDFrameWidget::svDnDFrameWidget(QWidget *parent)
: QWidget(parent)
{
  setAcceptDrops(true);
}

svDnDFrameWidget::~svDnDFrameWidget()
{
}

void svDnDFrameWidget::dragEnterEvent( QDragEnterEvent *event )
{   // accept drags
  event->acceptProposedAction();
}

void svDnDFrameWidget::dropEvent( QDropEvent * event )
{ //open dragged files

  QList<QUrl> fileNames = event->mimeData()->urls();
  if (fileNames.empty())
    return;

//  QStringList fileNames2;
//  //TODO Qt 4.7 API
//  //fileNames2.reserve(fileNames.size());
//  foreach(QUrl url, fileNames)
//  {
//    fileNames2.push_back(url.toLocalFile());
//  }

  std::vector<std::string> fileNames2;

  foreach(QUrl url, fileNames)
  {
    fileNames2.push_back(url.toLocalFile().toStdString());
  }

  mitk::DataStorage::Pointer dataStorage=svApplication::application()->dataStorage();
  mitk::IOUtil::LoadFiles(fileNames2,*dataStorage);
  mitk::RenderingManager::GetInstance()->InitializeViewsByBoundingObjects(dataStorage);

  event->accept();
}
