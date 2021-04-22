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

#include "sv4gui_Main.h"

#include "SimVascular.h"

#include "simvascular_options.h"

#ifdef SV_USE_PYTHON
  #include "Python.h"
#endif
#include "vtksys/SystemTools.hxx"

#ifdef SV_USE_SV4_GUI
  #include <QApplication>
  #include <QDir>
  #include <QVariant>
  #include <QDebug>
  #include "mitkBaseApplication.h"
  #include "ctkPluginFrameworkLauncher.h"
#endif

#include "sv4gui_MitkApp.h"

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

#include "sv2_globals.h"

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

/* The maximum length of a file name.  */
#if defined(PATH_MAX)
#define SV_PYTHON_MAXPATH PATH_MAX
#elif defined(MAXPATHLEN)
#define SV_PYTHON_MAXPATH MAXPATHLEN
#else
#define SV_PYTHON_MAXPATH 16384
#endif

int sv4guiMain(int argc, char *argv[],bool use_provisioning_file, bool use_workbench) {

     // intentionally remove any additional params when calling qt gui
     int single_argc = 1;
     //mitk::BaseApplication app(single_argc, argv);
     sv4guiMitkApp app(single_argc, argv);

     // note: this command doesn't seem to work.
     // US_LOAD_IMPORTED_MODULES_INTO_MAIN(svmodel svpath ...)
     #ifdef QT_STATICPLUGIN
       //Q_INIT_RESOURCE(...);
       // need to call activators for each plugin, e.g.
       //MitkImagePluginActivator* mitkimageplugin = new MitkImagePluginActivator();
       //mitkimageplugin->start();
     #endif

     app.setSingleMode(true);
     app.setApplicationName("SimVascularApplication");
     app.setOrganizationName("SimVascular");

     if (use_workbench) {
       fprintf(stdout,"Note: Using WorkBench App.\n");
       fflush(stdout);
       app.setProperty(mitk::BaseApplication::PROP_PRODUCT, "org.mitk.gui.qt.extapplication.workbench");
     } else {
       app.setProperty(mitk::BaseApplication::PROP_PRODUCT, "org.sv.gui.qt.application.svworkbench");
     }

     QStringList preloadLibs;
     preloadLibs << "liborg_mitk_gui_qt_ext";
     app.setPreloadLibraries(preloadLibs);

     if (use_provisioning_file == false) {

       fprintf(stdout,"Note: Not using the provisioning file. To use a provisioning file, provide --use-pro flag\n");
       fprintf(stdout,"\n");
       fflush(stdout);

       // can set a provisioning file here, but we hard code the plugins below
       QString provisioningFilePath = "";
       app.setProvisioningFilePath(provisioningFilePath);

       QString plugin_dirs = "";
       app.setProperty(mitk::BaseApplication::ARG_PLUGIN_DIRS, "");

       QStringList pluginsToStart;
       QString pluginPath;

       // Note: You can specify full URL filenames as well, e.g.
       // pluginsToStart.push_back("file:///C:/.../liborg_commontk_eventadmin.dll");

      // remove lib prefix and dll postfix
       pluginsToStart.push_back("org_commontk_configadmin");
       pluginsToStart.push_back("org_commontk_eventadmin");
       pluginsToStart.push_back("org_blueberry_core_runtime");
       pluginsToStart.push_back("org_blueberry_core_expressions");
       pluginsToStart.push_back("org_blueberry_core_commands");
       pluginsToStart.push_back("org_blueberry_ui_qt");
       pluginsToStart.push_back("org_blueberry_ui_qt_help");
       pluginsToStart.push_back("org_blueberry_ui_qt_log");
       pluginsToStart.push_back("org_mitk_core_services");
       pluginsToStart.push_back("org_mitk_gui_common");
       pluginsToStart.push_back("org_mitk_planarfigure");
       pluginsToStart.push_back("org_mitk_core_ext");
       pluginsToStart.push_back("org_mitk_gui_qt_application");
       pluginsToStart.push_back("org_mitk_gui_qt_ext");
       pluginsToStart.push_back("org_mitk_gui_qt_extapplication");
       pluginsToStart.push_back("org_mitk_gui_qt_common");
       pluginsToStart.push_back("org_mitk_gui_qt_stdmultiwidgeteditor");
       pluginsToStart.push_back("org_mitk_gui_qt_common_legacy");
       pluginsToStart.push_back("org_mitk_gui_qt_datamanager");
       pluginsToStart.push_back("org_mitk_gui_qt_properties");
       pluginsToStart.push_back("org_mitk_gui_qt_basicimageprocessing");
       pluginsToStart.push_back("org_mitk_gui_qt_dicom");
       pluginsToStart.push_back("org_mitk_gui_qt_geometrytools");
#if MITK_MAJOR_VERSION == 2016
       pluginsToStart.push_back("org_mitk_gui_qt_imagecropper");
#endif
       pluginsToStart.push_back("org_mitk_gui_qt_imagenavigator");
       pluginsToStart.push_back("org_mitk_gui_qt_measurementtoolbox");
#ifdef SV_USE_PYTHON
       pluginsToStart.push_back("org_mitk_gui_qt_python");
#endif
       if (use_workbench){
         pluginsToStart.push_back("org_mitk_gui_qt_segmentation");
       }
       pluginsToStart.push_back("org_mitk_gui_qt_volumevisualization");
       // SimVascular plugins
       if (!use_workbench) {
         pluginsToStart.push_back("org_sv_gui_qt_application");
         pluginsToStart.push_back("org_sv_projectdatanodes");
         pluginsToStart.push_back("org_sv_gui_qt_datamanager");
         pluginsToStart.push_back("org_sv_gui_qt_projectmanager");
         pluginsToStart.push_back("org_sv_gui_qt_pathplanning");
         pluginsToStart.push_back("org_sv_gui_qt_modeling");
         pluginsToStart.push_back("org_sv_gui_qt_segmentation");
         pluginsToStart.push_back("org_sv_gui_qt_meshing");
         pluginsToStart.push_back("org_sv_gui_qt_simulation");
         pluginsToStart.push_back("org_sv_gui_qt_romsimulation");
         pluginsToStart.push_back("org_sv_gui_qt_imageprocessing");
         pluginsToStart.push_back("org_sv_gui_qt_svfsi");
#ifdef SV_USE_MITK_SEGMENTATION
         pluginsToStart.push_back("org_sv_gui_qt_mitksegmentation");
#endif
#ifdef SV_USE_PYTHON
         pluginsToStart.push_back("org_sv_pythondatanodes");
#endif
       }

        // read environment variables for custom plugins
  fprintf(stdout,"Reading custom plugins SV_CUSTOM_PLUGINS environment variable...\n");
  fprintf(stdout,"\n");
#ifdef WIN32
       char custom_plugins[_MAX_ENV];
       size_t requiredSize;
       custom_plugins[0]='\0';
       requiredSize = 0;
       getenv_s( &requiredSize, NULL, 0, "SV_CUSTOM_PLUGINS");

       if (requiredSize >= _MAX_ENV) {
         std::cerr << "FATAL ERROR:  SV_CUSTOM_PLUGINS to long!\n" << std::endl << std::flush;
         exit(-1);
       } else if (requiredSize > 0) {
         getenv_s( &requiredSize, custom_plugins, requiredSize, "SV_CUSTOM_PLUGINS" );
         char seps[] = ";";
         char *token;
         token = strtok( custom_plugins, seps );
         while( token != NULL ) {
           // While there are tokens in "string"
           //printf( " %s\n", token );
           QString newPlugin = token;
           pluginsToStart.push_back(newPlugin);
           std::cout << "   Adding custom plugin (" << newPlugin.toStdString() << ")" << std::endl << std::flush;
           // Get next token
           token = strtok( NULL, seps );
         }
       }
#else
       char *custom_plugins = getenv("SV_CUSTOM_PLUGINS");
       if (custom_plugins != NULL) {
         char seps[] = ":";
         char *token;
         token = strtok( custom_plugins, seps );
         while( token != NULL ) {
           // While there are tokens in "string"
           //printf( " %s\n", token );
           QString newPlugin = token;
           pluginsToStart.push_back(newPlugin);
           std::cout << "   Adding custom plugin (" << newPlugin.toStdString() << ")" << std::endl << std::flush;
           // Get next token
           token = strtok( NULL, seps );
         }
       }
#endif
       fprintf(stdout,"\n");

       app.setProperty(ctkPluginFrameworkLauncher::PROP_PLUGINS, pluginsToStart);

       //Use transient start with declared activation policy
       ctkPlugin::StartOptions startOptions(ctkPlugin::START_TRANSIENT | ctkPlugin::START_ACTIVATION_POLICY);
       app.setProperty(ctkPluginFrameworkLauncher::PROP_PLUGINS_START_OPTIONS, static_cast<int>(startOptions));
     }

#ifdef SV_USE_PYTHON
  #ifdef SV_USE_PYTHON_EMBEDDED_IMPORTS
    #if PYTHON_MAJOR_VERSION == 3
      SimVascular_pyInit();
      PyImport_AppendInittab("pyGUI",PyInit_pyGUI);
      Py_Initialize();
    #endif
    #if PYTHON_MAJOR_VERSION ==2
      Py_Initialize();
      SimVascular_pyInit();
      initpyGUI();
    #endif
  #else
      Py_Initialize();  
  #endif
#endif

     return app.run();

}

