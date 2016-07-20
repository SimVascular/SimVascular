/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code.
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef CV_GLOBALS_H

#include "SimVascular.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"
#include "cvRepository.h"

#include "tcl.h"

extern SV_EXPORT_GLOBALS cvRepository *gRepository;

extern SV_EXPORT_GLOBALS Tcl_HashTable gLsetVTable;
extern SV_EXPORT_GLOBALS Tcl_HashTable gLsetCoreTable;

extern SV_EXPORT_GLOBALS char projectionSetBase_[CV_STRLEN];

// global variable to figure out if we are running in batch mode
extern SV_EXPORT_GLOBALS int gSimVascularBatchMode;

extern SV_EXPORT_GLOBALS Tcl_Interp* gVtkTclInterp;
extern SV_EXPORT_GLOBALS Tcl_Interp* getTclInterp();

extern SV_EXPORT_GLOBALS void *gOCCTManager;

#endif
