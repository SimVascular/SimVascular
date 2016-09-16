#include "svUndo.h"

#include <mitkVerboseLimitedLinearUndo.h>
#include <mitkUndoController.h>

const QString svUndo::EXTENSION_ID = "sv.undo";

svUndo::svUndo()
{
}

svUndo::~svUndo()
{
}

void svUndo::Exec()
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
      model->Undo();
    }
    else
    {
      MITK_ERROR << "No undo model instantiated";
    }
}
