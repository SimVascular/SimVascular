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

#ifndef __itkImageIOFactoryRegisterManager_h
#define __itkImageIOFactoryRegisterManager_h

namespace itk {

class ImageIOFactoryRegisterManager
{
  public:
  ImageIOFactoryRegisterManager(void (*list[])(void))
    {
    for(;*list; ++list)
      {
      (*list)();
      }
    }
};


//
//  The following code is intended to be expanded at the end of the
//  itkImageFileReader.h and itkImageFileWriter.h files.
//
void  NiftiImageIOFactoryRegister__Private(void);void  NrrdImageIOFactoryRegister__Private(void);void  GiplImageIOFactoryRegister__Private(void);void  HDF5ImageIOFactoryRegister__Private(void);void  JPEGImageIOFactoryRegister__Private(void);void  GDCMImageIOFactoryRegister__Private(void);void  BMPImageIOFactoryRegister__Private(void);void  LSMImageIOFactoryRegister__Private(void);void  PNGImageIOFactoryRegister__Private(void);void  TIFFImageIOFactoryRegister__Private(void);void  VTKImageIOFactoryRegister__Private(void);void  StimulateImageIOFactoryRegister__Private(void);void  BioRadImageIOFactoryRegister__Private(void);void  MetaImageIOFactoryRegister__Private(void);void  MRCImageIOFactoryRegister__Private(void);void  GE4ImageIOFactoryRegister__Private(void);void  GE5ImageIOFactoryRegister__Private(void);

//
// The code below registers available IO helpers using static initialization in
// application translation units. Note that this code will be expanded in the
// ITK-based applications and not in ITK itself.
//
namespace {

  void (*ImageIOFactoryRegisterRegisterList[])(void) = {
    NiftiImageIOFactoryRegister__Private,NrrdImageIOFactoryRegister__Private,GiplImageIOFactoryRegister__Private,HDF5ImageIOFactoryRegister__Private,JPEGImageIOFactoryRegister__Private,GDCMImageIOFactoryRegister__Private,BMPImageIOFactoryRegister__Private,LSMImageIOFactoryRegister__Private,PNGImageIOFactoryRegister__Private,TIFFImageIOFactoryRegister__Private,VTKImageIOFactoryRegister__Private,StimulateImageIOFactoryRegister__Private,BioRadImageIOFactoryRegister__Private,MetaImageIOFactoryRegister__Private,MRCImageIOFactoryRegister__Private,GE4ImageIOFactoryRegister__Private,GE5ImageIOFactoryRegister__Private,
    0};
  ImageIOFactoryRegisterManager ImageIOFactoryRegisterManagerInstance(ImageIOFactoryRegisterRegisterList);

}

}

#endif
