/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef __itkTransformIOFactoryRegisterManager_h
#define __itkTransformIOFactoryRegisterManager_h

namespace itk {

class TransformIOFactoryRegisterManager
{
  public:
  TransformIOFactoryRegisterManager(void (*list[])(void))
    {
    for(;*list; ++list)
      {
      (*list)();
      }
    }
};


//
//  The following code is intended to be expanded at the end of the
//  itkTransformFileReader.h and itkTransformFileWriter.h files.
//
void  MatlabTransformIOFactoryRegister__Private(void);void  HDF5TransformIOFactoryRegister__Private(void);void  TxtTransformIOFactoryRegister__Private(void);

//
// The code below registers available IO helpers using static initialization in
// application translation units. Note that this code will be expanded in the
// ITK-based applications and not in ITK itself.
//
namespace {

  void (*TransformIOFactoryRegisterRegisterList[])(void) = {
    MatlabTransformIOFactoryRegister__Private,HDF5TransformIOFactoryRegister__Private,TxtTransformIOFactoryRegister__Private,
    0};
  TransformIOFactoryRegisterManager TransformIOFactoryRegisterManagerInstance(TransformIOFactoryRegisterRegisterList);

}

}

#endif
