#include "svCollapsiblePane.h"
#include <QHBoxLayout>

svCollapsiblePane::svCollapsiblePane(QWidget* parent) :
    QWidget(parent)
{
    toggleButton=new QPushButton("<<",parent);
    toggleButton->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Expanding);
    toggleButton->setFixedWidth(20);

    pane=new QWidget(parent);
    paneLayout=new QVBoxLayout(pane);
    paneLayout->setMargin(0);
    paneLayout->setSpacing(0);
    pane->hide();

    QHBoxLayout* layout=new QHBoxLayout(this);
    layout->setMargin(0);
    layout->setSpacing(0);
    layout->addWidget(toggleButton);
    layout->addWidget(pane);

    connect(toggleButton, SIGNAL(clicked()), this, SLOT(toggleVisibility()));
}

svCollapsiblePane::~svCollapsiblePane()
{
}

void svCollapsiblePane::setWidget(QWidget* widget)
{
    paneLayout->addWidget(widget);
}

void svCollapsiblePane::setPaneVisibility(bool visible){
    if(visible){
        toggleButton->setText(">>");
        pane->show();
    }else{
        toggleButton->setText("<<");
        pane->hide();
    }
}

void svCollapsiblePane::toggleVisibility()
{
    if(pane->isVisible()){
        toggleButton->setText("<<");
        pane->hide();
    }else{
        toggleButton->setText(">>");
        pane->show();
    }
}



