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

#include "sv4gui_WorkbenchIntroPart.h"

#include <berryIWorkbenchWindow.h>
#include <berryIWorkbench.h>
#include <berryIWorkbenchPage.h>
#include <berryIPerspectiveRegistry.h>
#include <berryWorkbenchPreferenceConstants.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>
#include <berryPlatform.h>

#include <berryIEditorReference.h>
#include <berryIEditorInput.h>

#include <ctkPluginContext.h>

#include <mitkIDataStorageService.h>
#include <mitkDataStorageEditorInput.h>

#include <mitkLogMacros.h>

#include <QLabel>
#include <QMessageBox>
#include <QtCore/qconfig.h>

#if Qt5_MINOR_VERSION == 4
#include <QWebView>
#include <QWebPage>
#else
#include <QWebEngineView>
#include <QWebEnginePage>
#endif
#include <QUrlQuery>

#include <QString>
#include <QStringList>
#include <QRegExp>
#include <QChar>
#include <QByteArray>
#include <QDesktopServices>

#include "sv4gui_ApplicationPluginActivator.h"
#include "mitkDataStorageEditorInput.h"
#include <string>

sv4guiWorkbenchIntroPart::sv4guiWorkbenchIntroPart()
    : m_Controls(nullptr)
{
    berry::IPreferences::Pointer workbenchPrefs = sv4guiApplicationPluginActivator::GetDefault()->GetPreferencesService()->GetSystemPreferences();
    workbenchPrefs->PutBool(berry::WorkbenchPreferenceConstants::SHOW_INTRO, true);
    workbenchPrefs->Flush();
}

sv4guiWorkbenchIntroPart::~sv4guiWorkbenchIntroPart()
{
    // if the workbench is not closing (that means, welcome screen was closed explicitly), set "Show_intro" false
    if (!this->GetIntroSite()->GetPage()->GetWorkbenchWindow()->GetWorkbench()->IsClosing())
    {
        berry::IPreferences::Pointer workbenchPrefs = sv4guiApplicationPluginActivator::GetDefault()->GetPreferencesService()->GetSystemPreferences();
        workbenchPrefs->PutBool(berry::WorkbenchPreferenceConstants::SHOW_INTRO, false);
        workbenchPrefs->Flush();
    }
    else
    {
        berry::IPreferences::Pointer workbenchPrefs = sv4guiApplicationPluginActivator::GetDefault()->GetPreferencesService()->GetSystemPreferences();
        workbenchPrefs->PutBool(berry::WorkbenchPreferenceConstants::SHOW_INTRO, true);
        workbenchPrefs->Flush();
    }

    // if workbench is not closing (Just welcome screen closing), open last used perspective
    if (this->GetIntroSite()->GetPage()->GetPerspective()->GetId()
            == "org.mitk.mitkworkbench.perspectives.editor" && !this->GetIntroSite()->GetPage()->GetWorkbenchWindow()->GetWorkbench()->IsClosing())
    {
        berry::IPerspectiveDescriptor::Pointer perspective = this->GetIntroSite()->GetWorkbenchWindow()->GetWorkbench()->GetPerspectiveRegistry()->FindPerspectiveWithId("org.mitk.mitkworkbench.perspectives.editor");
        if (perspective)
        {
            this->GetIntroSite()->GetPage()->SetPerspective(perspective);
        }
    }

}

void sv4guiWorkbenchIntroPart::CreateQtPartControl(QWidget* parent)
{
    if (!m_Controls)
    {
        // create GUI widgets
        m_Controls = new Ui::svWelcomeScreenViewControls;
        m_Controls->setupUi(parent);
        // create a QWebView as well as a QWebPage and QWebFrame within the QWebview
#if Qt5_MINOR_VERSION == 4
        m_view = new QWebView(parent);
        m_view->page()->setLinkDelegationPolicy(QWebPage::DelegateAllLinks);
#else
        m_view = new QWebEngineView(parent);
#endif

        QUrl urlQtResource(QString("qrc:/org.sv.gui.qt.welcomescreen/svworkbenchwelcomeview.html"),  QUrl::TolerantMode );
        m_view->load( urlQtResource );

        // adds the webview as a widget
        parent->layout()->addWidget(m_view);
        this->CreateConnections();
    }
}

void sv4guiWorkbenchIntroPart::CreateConnections()
{
    if ( m_Controls )
    {
        connect( m_view, SIGNAL(linkClicked(const QUrl& )), this, SLOT(DelegateMeTo(const QUrl& )) );
    }
}


void sv4guiWorkbenchIntroPart::DelegateMeTo(const QUrl& showMeNext)
{
    QString scheme          = showMeNext.scheme();
    QByteArray urlHostname  = QUrl::toAce(showMeNext.host());
    QByteArray urlPath      = showMeNext.path().toLatin1();
    QUrlQuery query(showMeNext);
    QByteArray dataset      = query.queryItemValue("dataset").toLatin1();
    QByteArray clear        = query.queryItemValue("clear").toLatin1();//showMeNext.encodedQueryItemValue("clear");

    if (scheme.isEmpty()) MITK_INFO << " empty scheme of the to be delegated link" ;

    // if the scheme is set to mitk, it is to be tested which action should be applied
    if (scheme.contains(QString("mitk")) )
    {
        if(urlPath.isEmpty() ) MITK_INFO << " mitk path is empty " ;

        // searching for the perspective keyword within the host name
        if(urlHostname.contains(QByteArray("perspectives")) )
        {
            // the simplified method removes every whitespace
            // ( whitespace means any character for which the standard C++ isspace() method returns true)
            urlPath = urlPath.simplified();
            QString tmpPerspectiveId(urlPath.data());
            tmpPerspectiveId.replace(QString("/"), QString("") );
            QString perspectiveId  = tmpPerspectiveId;

            // is working fine as long as the perspective id is valid, if not the application crashes
            GetIntroSite()->GetWorkbenchWindow()->GetWorkbench()->ShowPerspective(perspectiveId, GetIntroSite()->GetWorkbenchWindow() );

            // search the Workbench for opened StdMultiWidgets to ensure the focus does not stay on the welcome screen and is switched to
            // a render window editor if one available
            ctkPluginContext* context = sv4guiApplicationPluginActivator::getContext();
            mitk::IDataStorageService* service = nullptr;
            ctkServiceReference serviceRef = context->getServiceReference<mitk::IDataStorageService>();
            if (serviceRef) service = context->getService<mitk::IDataStorageService>(serviceRef);
            if (service)
            {
                berry::IEditorInput::Pointer editorInput(new mitk::DataStorageEditorInput( service->GetActiveDataStorage() ));

                // search for opened StdMultiWidgetEditors
                berry::IEditorPart::Pointer editorPart = GetIntroSite()->GetPage()->FindEditor( editorInput );

                // if an StdMultiWidgetEditor open was found, give focus to it
                if(editorPart)
                {
                    GetIntroSite()->GetPage()->Activate( editorPart );
                }
            }
        }
    }
    // if the scheme is set to http, by default no action is performed, if an external webpage needs to be
    // shown it should be implemented below
    else if (scheme.contains(QString("http")) )
    {
        QDesktopServices::openUrl(showMeNext);
        //    m_view->load( ) ;
    }
    else if(scheme.contains("qrc"))
    {
        m_view->load(showMeNext);
    }

}


void sv4guiWorkbenchIntroPart::StandbyStateChanged(bool /*standby*/)
{

}


void sv4guiWorkbenchIntroPart::SetFocus()
{

}
