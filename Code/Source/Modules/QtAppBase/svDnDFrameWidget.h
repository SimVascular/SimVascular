#ifndef svDndFrameWidget_h
#define svDndFrameWidget_h

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#ifdef __MINGW32__
// We need to inlclude winbase.h here in order to declare
// atomic intrinsics like InterlockedIncrement correctly.
// Otherwhise, they would be declared wrong within qatomic_windows.h .
#include <windows.h>
#endif

#include <QWidget>

class QDragEnterEvent;
class QDropEvent;
class QMouseEvent;

class SVQTAPPBASE_EXPORT svDnDFrameWidget : public QWidget
{
  Q_OBJECT

public:
  svDnDFrameWidget(QWidget *parent);
  virtual ~svDnDFrameWidget();

private:
  void dragEnterEvent( QDragEnterEvent *event ) override;
  void dropEvent( QDropEvent * event ) override;

};


#endif
