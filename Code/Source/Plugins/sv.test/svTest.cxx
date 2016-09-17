#include "svTest.h"
#include <QLabel>

const QString svTest::EXTENSION_ID = "sv.test";

svTest::svTest()
{
}

svTest::~svTest()
{

}

void svTest::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;

    QVBoxLayout* layout = new QVBoxLayout(parent);
    QLabel* ltest=new QLabel("Test");
    ltest->setMinimumWidth(300);
    layout->addWidget(ltest);
    layout->setContentsMargins(0,0,0,0);

    parent->setLayout(layout);
    //parent->setMinimumWidth(300);

}


