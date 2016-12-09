
#ifndef SVAPPLICATION_H
#define SVAPPLICATION_H

#include <berryIApplication.h>


class svApplication : public QObject, public berry::IApplication
{
  Q_OBJECT
  Q_INTERFACES(berry::IApplication)

public:

  svApplication();

  QVariant Start(berry::IApplicationContext* context) override;
  void Stop() override;
};

#endif /*SVAPPLICATION_H*/
