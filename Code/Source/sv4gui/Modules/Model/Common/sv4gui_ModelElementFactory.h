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

#ifndef SV4GUI_MODELELEMENTFACTORY_H
#define SV4GUI_MODELELEMENTFACTORY_H

#include <sv4guiModuleModelExports.h>

#include "sv4gui_ModelElement.h"
#include <map>

class SV4GUIMODULEMODEL_EXPORT sv4guiModelElementFactory
{

public:

    typedef sv4guiModelElement* (*ModelElementCreationFunction)();

    static void RegisterCreationFunction(std::string type, ModelElementCreationFunction function);

    static sv4guiModelElement* CreateModelElement(std::string type);

    static std::vector<std::string> GetAvailableTypes();

    static bool IsTypeAvailable(std::string type);

    static void RegisterFileExtensions(std::string type, std::vector<std::string> fileExtensions);

    static std::vector<std::string> GetFileExtensions(std::string type);

    static std::string GetType(std::string fileExtension);

  protected:

    static std::map<std::string, ModelElementCreationFunction> m_FunctionMap;

    static std::map<std::string, std::vector<std::string>> m_FileExtensionMap;
  };


#endif // SV4GUI_MODELELEMENTFACTORY_H
