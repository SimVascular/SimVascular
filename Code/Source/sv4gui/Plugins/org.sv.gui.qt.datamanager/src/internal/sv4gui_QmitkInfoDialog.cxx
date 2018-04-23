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

/*===================================================================

The Medical Imaging Interaction Toolkit (MITK)

Copyright (c) German Cancer Research Center,
Division of Medical and Biological Informatics.
All rights reserved.

This software is distributed WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.

See LICENSE.txt or http://www.mitk.org for details.

===================================================================*/

#include "sv4gui_QmitkInfoDialog.h"

#include "QmitkDataStorageComboBox.h"

#include <sstream>

#include <QGridLayout>
#include <QTextBrowser>
#include <QPushButton>
#include <QLineEdit>
#include <QEvent>
#include <QKeyEvent>

sv4guiQmitkInfoDialog::sv4guiQmitkInfoDialog( const QList<mitk::DataNode::Pointer> &_Nodes, QWidget * parent /*= 0*/, Qt::WindowFlags f /*= 0 */ )
: QDialog(parent, f)
{
  // DIM
  auto   parentLayout = new QGridLayout;
  auto   _QmitkDataStorageComboBox = new QmitkDataStorageComboBox(this, true);
  m_KeyWord = new QLineEdit;
  m_KeyWord->installEventFilter(this);
  m_SearchButton = new QPushButton("Search (F3)", this);
  m_SearchButton->installEventFilter(this);
  m_TextBrowser = new QTextBrowser(this);
  QPushButton* _CancelButton = new QPushButton("Cancel", this);

  // SET
  this->setMinimumSize(512, 512);
  this->setLayout(parentLayout);
  this->setSizeGripEnabled(true);
  this->setModal(true);

  parentLayout->addWidget(_QmitkDataStorageComboBox, 0, 0, 1, 2);
  parentLayout->addWidget(m_KeyWord, 1, 0);
  parentLayout->addWidget(m_SearchButton, 1, 1);
  parentLayout->addWidget(m_TextBrowser, 2, 0, 1, 2);
  parentLayout->addWidget(_CancelButton, 3, 0, 1, 2);

  QObject::connect( _QmitkDataStorageComboBox, SIGNAL( OnSelectionChanged( const mitk::DataNode* ) )
    , this, SLOT( OnSelectionChanged( const mitk::DataNode* ) ) );

  foreach(mitk::DataNode::Pointer node, _Nodes)
  {
    _QmitkDataStorageComboBox->AddNode(node);
  }

  QObject::connect( m_KeyWord, SIGNAL( textChanged ( const QString & )  )
    , this, SLOT( KeyWordTextChanged(const QString &) ) );

  QObject::connect( m_SearchButton, SIGNAL( clicked ( bool ) )
    , this, SLOT( OnSearchButtonClicked( bool ) ) );

  QObject::connect( _CancelButton, SIGNAL( clicked ( bool ) )
    , this, SLOT( OnCancelButtonClicked( bool ) ) );

  _CancelButton->setDefault(true);

}

void sv4guiQmitkInfoDialog::OnSelectionChanged( const mitk::DataNode* node )
{
  std::ostringstream s;
  itk::Indent i(2);
  mitk::BaseData* _BaseData = node->GetData();
  if(_BaseData)
    _BaseData->Print(s, i);
  m_TextBrowser->setPlainText(QString::fromStdString(s.str()));
}

void sv4guiQmitkInfoDialog::OnSearchButtonClicked( bool  /*checked*/ /*= false */ )
{
  QString keyWord = m_KeyWord->text();
  QString text = m_TextBrowser->toPlainText();

  if(keyWord.isEmpty() || text.isEmpty())
    return;

  m_TextBrowser->find(keyWord);
  m_SearchButton->setText("Search Next(F3)");
}

void sv4guiQmitkInfoDialog::OnCancelButtonClicked( bool  /*checked*/ /*= false */ )
{
  this->done(0);
}

bool sv4guiQmitkInfoDialog::eventFilter( QObject *obj, QEvent *event )
{
  if (event->type() == QEvent::KeyPress)
  {
    QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
    if(keyEvent->key() == Qt::Key_F3  || keyEvent->key() == Qt::Key_Return)
    {
      // trigger deletion of selected node(s)
      this->OnSearchButtonClicked(true);
      // return true: this means the delete key event is not send to the table
      return true;
    }
  }
  // standard event processing
  return QObject::eventFilter(obj, event);
}

void sv4guiQmitkInfoDialog::KeyWordTextChanged(const QString &  /*text*/)
{
  QTextCursor textCursor = m_TextBrowser->textCursor();
  textCursor.setPosition(0);
  m_TextBrowser->setTextCursor(textCursor);
  m_SearchButton->setText("Search (F3)");
}
