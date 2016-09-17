#ifndef SVEXTENSIONMANAGER_H
#define SVEXTENSIONMANAGER_H

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include "svCollapsiblePane.h"
#include "svAbstractExtension.h"

#include <QString>
#include <QStringList>
#include <QHash>
#include <QWidget>
#include <QFile>

class SVQTAPPBASE_EXPORT svExtensionManager : public QObject
{
    Q_OBJECT

public:

    struct svViewInfo {
      QString id;
      QString name;
      QString className;
      QString icon;
      QString type;
      QString location;
      bool    inMenu;
      bool    inToolBar;
      QString life;
      QString description;
      QString category;
      QString accompanying_id;
      svAbstractExtension* extension;
      QWidget* controlWidget;
    };


    svExtensionManager(QFile* xmlFile);

    virtual ~svExtensionManager();

public slots:\

    QWidget* getLeftPane();

    void useExtension(QString extensionID);

    QList<svViewInfo*> getViewList();

    svAbstractExtension* getExtension(QString id);

protected:
    QHash<QString,svViewInfo*> viewHash;

    svCollapsiblePane* leftPane;

    svAbstractExtension* activeLeftWidget=0;
    QStringList leftActiveIDs;

    void loadPlugins();

    QObject* createExtension(const QString& className);

    QList<svViewInfo*> viewList;

    void processPluginXML(QFile* xmlFile);

};

#endif // SVEXTENSIONMANAGER_H
