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

#ifndef SV4GUI_WORKBENCHINTROPART_H
#define SV4GUI_WORKBENCHINTROPART_H

#include <QtCore/qconfig.h>

#include <berryQtIntroPart.h>
#include <ui_sv4gui_WelcomeScreenViewControls.h>

#include "simvascular_options.h"
#if Qt5_MINOR_VERSION == 4
  class QWebView ;
#else
  class QWebEngineView ;
#endif

class sv4guiWorkbenchIntroPart : public berry::QtIntroPart
{

    // this is needed for all Qt objects that should have a MOC object (everything that derives from QObject)
    Q_OBJECT

public:

    sv4guiWorkbenchIntroPart();
    ~sv4guiWorkbenchIntroPart();


    virtual void CreateQtPartControl(QWidget *parent) override;

    void StandbyStateChanged(bool) override;

    void SetFocus() override;

    virtual void CreateConnections();


protected slots:


    void DelegateMeTo(const QUrl& ShowMeNext);

protected:

    Ui::svWelcomeScreenViewControls* m_Controls;
#if Qt5_MINOR_VERSION == 4
    QWebView* m_view;
#else
    QWebEngineView* m_view;
#endif
};

#endif /* SV4GUI_WORKBENCHINTROPART_H */
