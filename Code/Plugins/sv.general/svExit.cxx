#include "svExit.h"
//#include "svApplication.h"

#include <QMessageBox>
#include <QApplication>

const QString svExit::EXTENSION_ID = "sv.exit";

svExit::svExit()
{
}

svExit::~svExit()
{
}

void svExit::Exec()
{

        QString msg = "Are you sure that you want to exit SimVascular?";
        if (QMessageBox::question(NULL, "Exit", msg,
                                  QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
        {
          return;
        }

        qApp->quit();
//        svApplication::application()->quit();

}
