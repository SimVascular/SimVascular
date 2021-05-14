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

// The 'sv4guisvFSIbcClass' class is used to store data for an initial condition.

#ifndef SV4GUI_SVFSIBCCLASS_H
#define SV4GUI_SVFSIBCCLASS_H

#include "sv4guiModulesvFSIExports.h"

#define maxOutput 10
#define maxProp 10
#include <map>
#include <QStringList>

class SV4GUIMODULESVFSI_EXPORT sv4guisvFSIbcClass 
{
  public:
    QString faceName;

    QString bcGrp;
    QString bcType;
    QString tDep;
    QString profile;
    int eDrn;
    int cplBCPtr;
    int faIn;
    double r;
    double g;
    QString gxFile;
    QString gmFile;
    QString gtFile;
    bool zperm;
    bool flux;

    QString projectionFaceName;

    bool imposeIntegral;
    QString effectiveDirection;

    sv4guisvFSIbcClass();
    ~sv4guisvFSIbcClass();
};

#endif // SV4GUI_SVFSIBCCLASS_H
