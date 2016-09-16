#include "svRedo.h"

#include <mitkVerboseLimitedLinearUndo.h>
#include <mitkUndoController.h>

const QString svRedo::EXTENSION_ID = "sv.redo";

svRedo::svRedo()
{
}

svRedo::~svRedo()
{
}

void svRedo::Exec()
{
    mitk::UndoModel* model = mitk::UndoController::GetCurrentUndoModel();
    if (model)
    {
      if (mitk::VerboseLimitedLinearUndo* verboseundo = dynamic_cast<mitk::VerboseLimitedLinearUndo*>( model ))
      {
        mitk::VerboseLimitedLinearUndo::StackDescription descriptions =
          verboseundo->GetRedoDescriptions();
        if (descriptions.size() >= 1)
        {
          MITK_INFO << "Redo " << descriptions.front().second;
        }
      }
      model->Redo();
    }
    else
    {
      MITK_ERROR << "No undo model instantiated";
    }
}
