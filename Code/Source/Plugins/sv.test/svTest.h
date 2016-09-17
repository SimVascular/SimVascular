#ifndef SVTEST_H
#define SVTEST_H

#include "svAbstractView.h"
#include <QWidget>

class svTest : public svAbstractView
{  
  Q_OBJECT
  
  public:  

    static const QString EXTENSION_ID;

    svTest();

    virtual ~svTest();

  public slots:

  protected:

    virtual void CreateQtPartControl(QWidget *parent) override;


    QWidget* m_Parent;


};

#endif // SVTEST_H

