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

// A class to integrate Tcl and Qt event loops, part of EDASkel, a sample EDA app
// Copyright (C) 2010 Jeffrey Elliot Trull <edaskel@att.net>
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#if !defined(EDASKEL_QT_TCL_NOTIFIER)
#define EDASKEL_QT_TCL_NOTIFIER

#include "SimVascular.h"

#include <QSocketNotifier>
#include <QTimer>
#include <map>
#include <tcl.h>

namespace QtTclNotify {

  // Tcl has a concept of a "Notifier", a sort of API through which one can retarget its event processing
  // and integrate with other systems.  This is done by implementing a set of 8 functions for it to call
  // as needed.  This is described in more detail in the Notifier man page supplied with the Tcl distribution,
  // but I found that without the examples tclUnixNotfy.c and tclXtNotify.c (included in the distribution)
  // I could not have understood what was required.

  // I've implemented a Notifier in C++ through a singleton class with static methods matching the Notifier
  // API and non-static methods, where needed, to access the Qt signal/slot mechanism

  class QtTclFileHandler;   // forward ref
  // Singleton class.  Static methods to work with Tcl callbacks, non-static for Qt signal/slot mechanism
  class QtTclNotifier : public QObject {
    Q_OBJECT
      public:
    typedef std::map<int, QtTclFileHandler*> HandlerMap;

    // the key Tcl Notifier functions
    static void SetTimer(Tcl_Time const* timePtr);
    static int WaitForEvent(Tcl_Time const* timePtr);
    static void CreateFileHandler(int fd, int mask, Tcl_FileProc* proc, ClientData clientData);
    static void DeleteFileHandler(int fd);
    static void* InitNotifier();
    static void FinalizeNotifier(ClientData clientData);
    static void AlertNotifier(ClientData clientData);
    static void ServiceModeHook(int mode);

    static QtTclNotifier* getInstance();

    static void setup();

    public slots:
    void readReady(int fd);
    void writeReady(int fd);
    void exception(int fd);
    void handle_timer();
  private:
    QtTclNotifier();    // singleton
    ~QtTclNotifier();

    template<int TclActivityType> static void perform_callback(int fd);
    HandlerMap m_handlers;
    QTimer* m_timer;                    // for implementing Tcl_SetTimer
    static QtTclNotifier* m_notifier;   // pointer to the single instance we allow

  };

  // QtTclFileHandler objects aggregate the activity mask/callback function/client data for a given file descriptor
  // it will also "own" (in a Qt sense - object hierarchy) the QSocketNotifiers created for it
  // This way they will get destroyed when the parent does and I won't have to clean them up
  class QtTclFileHandler : public QObject {
    Q_OBJECT
      public:
    QtTclFileHandler(Tcl_FileProc* proc, ClientData clientData, int mask) : m_proc(proc), m_clientData(clientData), m_mask(mask) {}
    void perform_callback(int type, int fd);
  private:
    Tcl_FileProc* m_proc;      // function to call
    ClientData m_clientData;   // extra data to supply
    int m_mask;                // types of activity supported
  };

}

#endif
