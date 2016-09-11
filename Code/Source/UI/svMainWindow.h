#ifndef SVMAINWINDOW_H
#define SVMAINWINDOW_H

#include "SimVascular.h"

#include <QMainWindow>

#include "svCollapsiblePane.h"

#include <mitkDataStorage.h>
#include "QmitkStdMultiWidget.h"

#include <ctkPythonConsole.h>

#include <vtkRenderer.h>
#include <vtkSmartPointer.h>

#include <vtkRenderWindow.h>

class SVQTMAINWINDOW_EXPORT svMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit svMainWindow(QWidget *parent = 0);
    ~svMainWindow();
    ctkPythonConsole* pythonConsole;

public slots:
    vtkRenderer* getRenderer();

    vtkRenderWindow* getRenderWindow();

protected:

    QmitkStdMultiWidget* m_DisplayWidget;

    mitk::DataStorage::Pointer m_DataStorage;

    void connectViews();

    void closeEvent(QCloseEvent *event) Q_DECL_OVERRIDE;

private slots:

    void on_actionPythonConsole_triggered();

    void on_actionAxialOnly_triggered();

    void on_actionSagittalOnly_triggered();

    void on_actionCoronalOnly_triggered();

    void on_actionLayout2DTop_triggered();

    void on_actionLayout2DLeft_triggered();

    void on_actionLayoutStandard_triggered();

    void on_actionLayout3DOnly_triggered();

private:

    void initializePythonConsole();
};

#endif // SVMAINWINDOW_H
