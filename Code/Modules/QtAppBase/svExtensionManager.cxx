#include "svExtensionManager.h"
#include "svApplication.h"

#include "svPluginActivator.h"
#include "svExtensionRegistry.h"

#include <QCoreApplication>
#include <QVBoxLayout>
#include <QDir>
#include <QPluginLoader>
#include <QDomDocument>
#include <iostream>
#include <QGroupBox>
#include <QDebug>

using namespace std;

svExtensionManager::svExtensionManager(QFile* xmlFile)
{

    //Load Plugins
    loadPlugins();

    processPluginXML(xmlFile);

    svApplication::application()->pythonManager()->addObjectToPythonMain("svExtensionManager", this);
}

svExtensionManager::~svExtensionManager()
{
}


void svExtensionManager::processPluginXML(QFile* xmlFile)
{
    //Load xml file
    QDomDocument doc("pluginxml");
    if (!xmlFile->open(QIODevice::ReadOnly)){
        cout << "---- Error open plugin.xml---------" << endl;
        return;
    }
    QString *em=NULL;
    if (!doc.setContent(xmlFile,em)) {
        xmlFile->close();
        cout <<" error setContent" << endl;
        cout << "Error: "<< em->toStdString() << endl;
        return;
    }
    xmlFile->close();

    //    QDomElement docElem = doc.documentElement();

    QDomElement plugin = doc.firstChildElement("plugin");
    QDomElement extension=plugin.firstChildElement("extension");


    //Create Left Widget
    QWidget* leftWidget=new QWidget;
    QVBoxLayout* leftLayout=new QVBoxLayout(leftWidget);
    leftLayout->setMargin(0);
    leftLayout->setSpacing(10);
    leftPane=new svCollapsiblePane();
    leftPane->setWidget(leftWidget);

    leftWidget->setStyleSheet("QGroupBox {  \
                              border: 1px solid gray; \
            margin-top: 0.5em; \
} \
\
QGroupBox::title {   \
    subcontrol-origin: margin;  \
left: 10px;  \
padding: 0 3px 0 3px;\
    font-weight : bold;  \
}");

if(!extension.isNull() && extension.attribute("point")=="sv.views")
{
    QDomNodeList categories=extension.elementsByTagName("category");

    for(int i=0;i<categories.size();i++)
    {
        QDomNode category=categories.item(i);
        if(category.isNull()) continue;

        QDomElement cat = category.toElement();
        if(cat.isNull()) continue;

        QString catName=cat.attribute("name");

        QDomNodeList views=cat.elementsByTagName("view");

        for(int j=0;j<views.size();j++)
        {
            QDomNode view=views.item(j);
            if(view.isNull()) continue;

            QDomElement v = view.toElement();
            if(v.isNull()) continue;

            svViewInfo* viewInfo=new svViewInfo;

            viewInfo->id=v.attribute("id");
            viewInfo->name=v.attribute("name");
            viewInfo->className=v.attribute("class");
            viewInfo->icon=v.attribute("icon");
            viewInfo->type=v.attribute("type");
            viewInfo->location=v.attribute("location","left");
            viewInfo->inMenu=(v.attribute("in_menu","yes")=="yes") ? true : false;
            viewInfo->inToolBar=(v.attribute("in_toolbar","yes")=="yes") ? true : false;
            viewInfo->life=v.attribute("life","long");
            viewInfo->category=catName;
            viewInfo->accompanying_id=v.attribute("accompanying_id");

            QDomElement description = v.firstChildElement("description");
            viewInfo->description=description.text();

            if(viewInfo->type=="init")
            {
                svAbstractExtension* svext=qobject_cast<svAbstractExtension*>(createExtension(viewInfo->className));
            }
            else if(viewInfo->type=="gui" || viewInfo->type=="exec")
            {
                svAbstractExtension* svext=qobject_cast<svAbstractExtension*>(createExtension(viewInfo->className));

				if (svext == NULL) continue;

                if(viewInfo->type=="gui")
                {
                    svext->CreatePartControl();
                    viewInfo->controlWidget=svext;
                    if(viewInfo->location=="left")
                    {
                        QGroupBox* qgb=new QGroupBox(viewInfo->name);
                        QVBoxLayout *vbox = new QVBoxLayout;
                        vbox->setContentsMargins(0,10,0,10);
                        vbox->setSpacing(0);
                        vbox->addWidget(svext);
                        qgb->setLayout(vbox);

                        viewInfo->controlWidget=qgb;

                        leftLayout->addWidget(viewInfo->controlWidget);
                    }
                    viewInfo->controlWidget->hide();
                }
                viewInfo->extension=svext;
                viewList.append(viewInfo);
                viewHash[viewInfo->id] = viewInfo;
            }
            else if(viewInfo->type=="menu_separator")
            {
                viewList.append(viewInfo);
            }

        }
    }

}

}

void svExtensionManager::loadPlugins()
{
    QDir pluginsDir(QCoreApplication::applicationDirPath());
#if defined(Q_OS_WIN)
    if (pluginsDir.dirName().toLower() == "debug" || pluginsDir.dirName().toLower() == "release")
        pluginsDir.cdUp();
#elif defined(Q_OS_MAC)
    if (pluginsDir.dirName() == "MacOS") {
        pluginsDir.cdUp();
        pluginsDir.cdUp();
        pluginsDir.cdUp();
    }
#endif
    pluginsDir.cd("plugins");
    //    qDebug(pluginsDir.absolutePath());

#if(Q_CC_MSVC)
    pluginsDir.setNameFilters(QStringList()<<"*.dll");
#endif

    foreach (QString fileName, pluginsDir.entryList(QDir::Files)) {

        QPluginLoader pluginLoader(pluginsDir.absoluteFilePath(fileName));

        QObject *plugin = pluginLoader.instance();

        if (plugin) {

            svPluginActivator *svplugin = qobject_cast<svPluginActivator *>(plugin);
            if (svplugin)
            {
                svplugin->start();
                qDebug().noquote() <<fileName << ": loaded";
            }else{
                qDebug().noquote() <<fileName << ": loaded, but not svplugin";
            }
        }else{
            qDebug().noquote() <<fileName <<  ": not loaded";
        }


    }

}

QList<svExtensionManager::svViewInfo*> svExtensionManager::getViewList()
{
    return viewList;
}

QObject* svExtensionManager::createExtension(const QString& className)
{
    QObject* result = nullptr;
    QString typeName = className;
    int extensionTypeId = svExtensionRegistry::type(typeName.toLatin1().data());
    if (extensionTypeId == 0)
    {
        QString message = QString("Unable to find class \"%1\"."
                                  " The class was either not registered via "
                                  "SV_REGISTER_EXTENSION_CLASS(type) "
                                  "or you forgot to run Qt's moc on the header file.")
                .arg(className);
        std::cout<<message.toStdString()<<std::endl;
    }
    else
    {
        try
        {
            result = svExtensionRegistry::construct(extensionTypeId);
        }
        catch (const std::exception& e)
        {
            QString message = QString("Unable to instantiate class \"%1\". Error: \"%2\"")
                    .arg(className).arg(QString(e.what()));
            std::cout<<message.toStdString()<<std::endl;
        }
    }

    return result;
}

QWidget* svExtensionManager::getLeftPane(){
    return leftPane;
}

//void svExtensionManager::useExtension(QString extensionID){

//    svViewInfo* viewInfo=viewHash[extensionID];

//    if(!viewInfo) return;

//    if(viewInfo->type=="gui")
//    {
//        if(activeLeftWidget&&activeLeftWidget!=viewInfo->extension){
//            activeLeftWidget->hide();
//            activeLeftWidget->Leave();
//        }

//        activeLeftWidget=viewInfo->extension;
//        leftPane->setMaximumWidth(activeLeftWidget->sizeHint().width());
//        activeLeftWidget->Enter();
//        activeLeftWidget->show();
//        leftPane->setPaneVisibility(true);
//    }else{
//        viewInfo->extension->Enter();
//        viewInfo->extension->Exec();
//        viewInfo->extension->Leave();
//    }
//}

svAbstractExtension* svExtensionManager::getExtension(QString id)
{
    svViewInfo* viewInfo=viewHash[id];
    if(viewInfo)
    {
        return viewInfo->extension;
    }else{
        return NULL;
    }
}


void svExtensionManager::useExtension(QString IDs){

    QStringList extensionIDs = IDs.split(";", QString::SkipEmptyParts);
    extensionIDs.removeDuplicates();


    //For leftPane
    QStringList leftIDs;

    for(int i=0;i<extensionIDs.size();i++)
    {
        svViewInfo* viewInfo=viewHash[extensionIDs.at(i)];
        if(viewInfo)
        {
            if(viewInfo->type=="gui")
            {
                if(viewInfo->location=="left")
                {
                    leftIDs.append(extensionIDs.at(i));
                }
            }
        }
    }


    if(!leftIDs.isEmpty()){
        QStringList waitingRemoveList;

        for(int i=0;i<leftActiveIDs.size();i++)
        {
            QString activeID=leftActiveIDs.at(i);
            if(leftIDs.contains(activeID))
            {
                leftIDs.removeAll(activeID);
            }else{
                waitingRemoveList.append(activeID);
                svViewInfo* viewInfo=viewHash[activeID];
                if(viewInfo&&viewInfo->type=="gui"&&viewInfo->location!="popup")
                {
                    viewInfo->controlWidget->hide();
                    viewInfo->extension->Leave();
                }
            }
        }

        for(int i=0;i<waitingRemoveList.size();i++)
        {
            leftActiveIDs.removeAll(waitingRemoveList.at(i));
        }

    }

    bool newLeftActive=false;

    for(int i=0;i<extensionIDs.size();i++)
    {
        svViewInfo* viewInfo=viewHash[extensionIDs.at(i)];
        if(viewInfo)
        {
            if(viewInfo->type=="gui")
            {
                viewInfo->extension->Enter();
                viewInfo->controlWidget->show();
                viewInfo->extension->SetFocus();
                if(viewInfo->location=="left")
                {
                    leftActiveIDs.append(extensionIDs.at(i));
                    newLeftActive=true;
                }
            }else{
                viewInfo->extension->Enter();
                viewInfo->extension->Exec();
                viewInfo->extension->Leave();
            }
        }
    }

    if(newLeftActive)
    {
        int leftWidth=0;
        bool leftShow=false;
        int tempWidth=0;

        for(int i=0;i<leftActiveIDs.size();i++)
        {
            QString activeID=leftActiveIDs.at(i);

            svViewInfo* viewInfo=viewHash[activeID];
            if(viewInfo&&viewInfo->type=="gui")
            {
                if(viewInfo->location=="left")
                {
                    leftShow=true;
                    tempWidth=viewInfo->controlWidget->sizeHint().width();
                    if(leftWidth<tempWidth) leftWidth=tempWidth;
                }

            }

        }

        if(leftShow)
        {
            leftPane->setMaximumWidth(leftWidth+40);
            leftPane->setPaneVisibility(true);
        }

    }
}
