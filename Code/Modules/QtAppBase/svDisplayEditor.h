#ifndef SVDISPLAYEDITOR_H
#define SVDISPLAYEDITOR_H

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include "svBasicFunctionality.h"
#include <QmitkStdMultiWidget.h>

class SVQTAPPBASE_EXPORT svDisplayEditor : public svBasicFunctionality
{
    Q_OBJECT

public:

    svDisplayEditor(QWidget* parent=0);

    virtual ~svDisplayEditor();

public slots:

    QmitkStdMultiWidget* GetDisplayWidget();

protected:

    virtual void CreateQtPartControl(QWidget *parent) override;

    QWidget* m_Parent;

    mitk::DataStorage::Pointer m_DataStorage;

    QmitkStdMultiWidget* m_DisplayWidget;

};

#endif // SVDISPLAYEDITOR_H
