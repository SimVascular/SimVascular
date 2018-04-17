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

#ifndef SVQMITKSINGLENODESELECTION_H_
#define SVQMITKSINGLENODESELECTION_H_

// Own includes
#include "berryISelection.h"
/// Qmitk
#include <org_sv_gui_qt_datamanager_Export.h>

//# forward declarations
namespace mitk
{
  class DataNode;
}

namespace berry
{

  ///
  /// \struct SingleNodeSelection
  /// \brief Represents a selection object that encapsulates the selection of a single node.
  ///
  struct SV_QT_DATAMANAGER SingleNodeSelection: public berry::ISelection
  {
    ///
    /// \brief Make typdefs for pointer types.
    ///
    berryObjectMacro(SingleNodeSelection);
    ///
    /// \brief Node setter.
    ///
    void SetNode(mitk::DataNode* _SelectedNode);
    ///
    /// \brief Node getter.
    ///
    mitk::DataNode* GetNode() const;
    ///
    /// \brief Checks if node is 0.
    ///
    virtual bool IsEmpty() const override;

  protected:
    ///
    /// \brief Holds the node that is currently selected.
    ///
    mitk::DataNode* m_Node;
  };

}

#endif // SVQMITKSINGLENODESELECTION_H_
