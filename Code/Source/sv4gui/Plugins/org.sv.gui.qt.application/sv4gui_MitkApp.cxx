/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "SimVascular.h"
#include "simvascular_options.h"

#include <org_sv_gui_qt_application_Export.h>

#include "sv4gui_MitkApp.h"

#include <QApplication>
#include <QDir>
#include <QVariant>
#include <QDebug>
#include "mitkBaseApplication.h"
#include "ctkPluginFrameworkLauncher.h"

#include "sv_IOstream.h"
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

//#include "sv2_globals.h"

#ifdef WIN32
#include <windows.h>
#include <tchar.h>
#include "Shlwapi.h"
#include <Shlobj.h>

#define BUFSIZE 1024
#define BUF_SIZE 1024

#ifndef GetShortPathName
  #ifdef UNICODE
    #define GetShortPathName GetShortPathNameW
  #else
    #define GetShortPathName GetShortPathNameA
  #endif // !UNICODE
#endif

#endif

sv4guiMitkApp::sv4guiMitkApp(int argc, char* argv[]) : BaseApplication(argc, argv)
{
}

sv4guiMitkApp::~sv4guiMitkApp()
{
}

void sv4guiMitkApp::initializeLibraryPaths() {

  //std::cout << "\n\n *** sv4guiMitkApp: initializeLibraryPaths! *** \n\n" << std::endl << std::flush;

  bool found_sv_plugin_path = false;

  //
  //  This is SV code to start using env variables and registry
  //  to specify library paths.
  //

  // read environment variables for plugin paths
  fprintf(stdout,"Reading plugin paths SV_PLUGIN_PATH environment variable...\n");
  fprintf(stdout,"\n");
  fflush(stdout);
#ifdef WIN32
  char plugin_env[_MAX_ENV];
  size_t requiredSize;
  plugin_env[0]='\0';
  requiredSize = 0;
  getenv_s( &requiredSize, NULL, 0, "SV_PLUGIN_PATH");

  if (requiredSize == 0) {
    std::cerr << "Warning:  SV_PLUGIN_PATH doesn't exist!\n" << std::endl << std::flush;
  } else if (requiredSize >= _MAX_ENV) {
    std::cerr << "FATAL ERROR:  SV_PLUGIN_PATH to long!\n" << std::endl << std::flush;
    exit(-1);
  } else {
    found_sv_plugin_path = true;
    getenv_s( &requiredSize, plugin_env, requiredSize, "SV_PLUGIN_PATH" );
    char seps[] = ";";
    char *token;
    token = strtok( plugin_env, seps );
    while( token != NULL ) {
      // While there are tokens in "string"
      //printf( " %s\n", token );
      QString pluginPath = token;
      ctkPluginFrameworkLauncher::addSearchPath(pluginPath);
      std::cout << "   Adding to plugin search path (" << pluginPath.toStdString() << ")" << std::endl << std::flush;
      // Get next token
      token = strtok( NULL, seps );
    }
  }
#else
  char *plugin_env = getenv("SV_PLUGIN_PATH");
  if (plugin_env == NULL) {
    std::cerr << "Warning:  SV_PLUGIN_PATH doesn't exist!\n" << std::endl << std::flush;
  } else {
    found_sv_plugin_path = true;
    char seps[] = ":";
    char *token;
    token = strtok( plugin_env, seps );
    while( token != NULL ) {
      // While there are tokens in "string"
      //printf( " %s\n", token );
      QString pluginPath = token;
      ctkPluginFrameworkLauncher::addSearchPath(pluginPath);
      std::cout << "   Adding to plugin search path (" << pluginPath.toStdString() << ")" << std::endl << std::flush;
      // Get next token
      token = strtok( NULL, seps );
    }
  }
#endif
  fprintf(stdout,"\n");

  //
  // This is the default behavior in AppUtil for MITK.
  //

  if (!found_sv_plugin_path) {
    QStringList suffixes;
    QDir appDir;

    suffixes << "plugins";
#ifdef WIN32
    suffixes << "bin/plugins";
#ifdef CMAKE_INTDIR
    suffixes << "bin/" CMAKE_INTDIR "/plugins";
#endif
#else
    suffixes << "lib/plugins";
#ifdef CMAKE_INTDIR
    suffixes << "lib/" CMAKE_INTDIR "/plugins";
#endif
#endif

#ifdef __APPLE__
    suffixes << "../../plugins";
#endif

    // we add a couple of standard library search paths for plug-ins
    appDir = QCoreApplication::applicationDirPath();

    // walk one directory up and add bin and lib sub-dirs; this
    // might be redundant
    appDir.cdUp();

    foreach(QString suffix, suffixes)
    {
      ctkPluginFrameworkLauncher::addSearchPath(appDir.absoluteFilePath(suffix));
    }

    suffixes << "plugins";
    suffixes << "bin/plugins";
    suffixes << "lib/plugins";

    // we add a couple of standard library search paths for plug-ins
    appDir = QCoreApplication::applicationDirPath();

    foreach(QString suffix, suffixes)
    {
      ctkPluginFrameworkLauncher::addSearchPath(appDir.absoluteFilePath(suffix));
      std::cout << "Adding to plugin search path (" << appDir.absoluteFilePath(suffix).toStdString() <<  ")" << std::endl << std::flush;
    }

  }

  //
  //  This code is a debugging check to make sure that all of the dll's
  //  can be found in the search path.
  //
  fprintf(stdout,"Checking all plugin paths...\n");
  fprintf(stdout,"\n");
  fflush(stdout);

  QVariant pluginsToStartVariant = this->getProperty(ctkPluginFrameworkLauncher::PROP_PLUGINS);
  QStringList pluginsToStart = pluginsToStartVariant.toStringList();

  for (QStringList::iterator it =  pluginsToStart.begin();
       it !=  pluginsToStart.end(); ++it) {
     QString current = *it;
     QString MypluginPath = ctkPluginFrameworkLauncher::getPluginPath(current);
     std::cout << "  plugin (" << current.toStdString() << ")" << std::endl << std::flush;
     std::cout << "    resolves to [" << MypluginPath.toStdString() << "]" << std::endl << std::flush;
  }
  fprintf(stdout,"\n");
  fflush(stdout);

  return;
}
