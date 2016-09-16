#ifndef SVABSTRACTFUNCTIONALITY_H_
#define SVABSTRACTFUNCTIONALITY_H_

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include "svBasicFunctionality.h"
#include "svRenderWindowPart.h"

#include <QmitkStdMultiWidget.h>
#include <mitkRenderingManager.h>

class SVQTAPPBASE_EXPORT svAbstractFunctionality : public svBasicFunctionality
{

    Q_OBJECT

public:

  /**
   * Nothing to do in the standard ctor. <b>Initiliaze your GUI in CreateQtPartControl(QWidget*)</b>
   */
  svAbstractFunctionality();

  /**
   * Disconnects all standard event listeners
   */
  virtual ~svAbstractFunctionality();

   /**
   * \return The active svRenderWindowPart.
   */
  static svRenderWindowPart* GetRenderWindowPart();

  static QmitkStdMultiWidget* GetDisplayWidget();

  static int GetTimeStep(const mitk::BaseData* data);

  static mitk::ScalarType GetTime(const mitk::BaseData* data);

  /**
   * Request an update of all render windows.
   *
   * \param requestType Specifies the type of render windows for which an update
   *        will be requested.
   */
  static void RequestRenderWindowUpdate(mitk::RenderingManager::RequestType requestType = mitk::RenderingManager::REQUEST_UPDATE_ALL);
  static void RequestRenderWindowImmediateUpdate(mitk::RenderingManager::RequestType requestType = mitk::RenderingManager::REQUEST_UPDATE_ALL);

protected:

  virtual void AfterCreateQtPartControl() override;

private:

  Q_DISABLE_COPY(svAbstractFunctionality)

};

#endif /*SVABSTRACTFUNCTIONALITY_H_*/
