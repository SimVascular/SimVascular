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

#ifndef SV4GUI_WORKBENCHWINDOWADVISORHACK_H
#define SV4GUI_WORKBENCHWINDOWADVISORHACK_H

#include <QObject>

class ctkPluginContext;
class QmitkPreferencesDialog;

class sv4guiWorkbenchWindowAdvisorHack : public QObject
{
    Q_OBJECT

public slots:

    void onUndo();
    void onRedo();
    void onImageNavigator();
    void onViewNavigator();
    void onEditPreferences();
    void onQuit();

    void onResetPerspective();
    void onClosePerspective();
    void onNewWindow();
    void onIntro();

    /**
     * @brief This slot is called if the user klicks the menu item "help->context help" or presses F1.
     * The help page is shown in a workbench editor.
     */
    void onHelp();

    void onHelpOpenHelpPerspective();

    /**
     * @brief This slot is called if the user clicks in help menu the about button
     */
    void onAbout();

public:

    sv4guiWorkbenchWindowAdvisorHack();
    ~sv4guiWorkbenchWindowAdvisorHack();
    static void SafeHandleNavigatorView(QString view_query_name);
    static sv4guiWorkbenchWindowAdvisorHack* undohack;
};

#endif // SV4GUI_WORKBENCHWINDOWADVISORHACK_H

