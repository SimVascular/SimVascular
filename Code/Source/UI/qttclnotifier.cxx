// Implementation of classes to integrate the Qt and Tcl event loops, part of EDASkel, a sample EDA app
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

#include "qttclnotifier.h"
#include <QCoreApplication>

using namespace QtTclNotify;

// Tell Tcl to replace its default notifier with ours
void QtTclNotifier::setup() {
  Tcl_NotifierProcs notifier;
  notifier.createFileHandlerProc = CreateFileHandler;
  notifier.deleteFileHandlerProc = DeleteFileHandler;
  notifier.setTimerProc = SetTimer;
  notifier.waitForEventProc = WaitForEvent;
  notifier.initNotifierProc = InitNotifier;
  notifier.finalizeNotifierProc = FinalizeNotifier;
  notifier.alertNotifierProc = AlertNotifier;
  notifier.serviceModeHookProc = ServiceModeHook;
  Tcl_SetNotifier(&notifier);
}

// Store the requested callback and establish one or more QSocketNotifier objects to link the activity to it
void QtTclNotifier::CreateFileHandler(int fd, int mask, Tcl_FileProc* proc, ClientData clientData) {
    // find any existing handler and deactivate it
    HandlerMap::iterator old_handler_it = getInstance()->m_handlers.find(fd);
    if (old_handler_it != getInstance()->m_handlers.end()) {
      // schedule QtTclFileHandler QObject for deletion (and disconnection), along with its QSocketNotifiers,
      // when control returns to event loop
      old_handler_it->second->deleteLater();
      // remove from map
      getInstance()->m_handlers.erase(old_handler_it);
    }
      
    QtTclFileHandler* hdlr = new QtTclFileHandler(proc, clientData, mask);
    if (mask & TCL_READABLE) {
      // create read activity socket notifier as a child of the handler (so will be destroyed at the same time)
      QSocketNotifier* rdntf = new QSocketNotifier(fd, QSocketNotifier::Read, hdlr);
      QObject::connect(rdntf, SIGNAL(activated(int)),
		       getInstance(), SLOT(readReady(int)));
    }
    if (mask & TCL_WRITABLE) {
      QSocketNotifier* wrntf = new QSocketNotifier(fd, QSocketNotifier::Write, hdlr);
      QObject::connect(wrntf, SIGNAL(activated(int)),
		       getInstance(), SLOT(writeReady(int)));
    }
    if (mask & TCL_EXCEPTION) {
      QSocketNotifier* exntf = new QSocketNotifier(fd, QSocketNotifier::Exception, hdlr);
      QObject::connect(exntf, SIGNAL(activated(int)),
		       getInstance(), SLOT(exception(int)));
    }
    getInstance()->m_handlers.insert(std::make_pair(fd, hdlr));

  }

// remove the handler for the given file descriptor and cancel its notifications
void QtTclNotifier::DeleteFileHandler(int fd) {
  HandlerMap::iterator old_handler_it = getInstance()->m_handlers.find(fd);
  if (old_handler_it != getInstance()->m_handlers.end()) {
    old_handler_it->second->deleteLater();
    getInstance()->m_handlers.erase(old_handler_it);
  }
  // Note: Tcl seems to call this thing with invalid fd's sometimes.  I had a debug message for that,
  // but got tired of seeing it fire all the time.
}

// find and execute handler for the given file descriptor and activity
template<int TclActivityType> void QtTclNotifier::perform_callback(int fd) {
  // find the handler
  HandlerMap::const_iterator handler_it = getInstance()->m_handlers.find(fd);
  // check that it was found
  if (handler_it == getInstance()->m_handlers.end()) {
    qDebug("could not find a registered file handler for fd=%d\n", fd);
    return;
  }

  // pass the request to the filehandler object for execution
  handler_it->second->perform_callback(TclActivityType, fd);

}
void QtTclNotifier::readReady(int fd) { perform_callback<TCL_READABLE>(fd); }
void QtTclNotifier::writeReady(int fd) { perform_callback<TCL_WRITABLE>(fd); }
void QtTclNotifier::exception(int fd) { perform_callback<TCL_EXCEPTION>(fd); }

// arrange for Tcl_ServiceAll to be executed after the specified time
void QtTclNotifier::SetTimer(Tcl_Time const* timePtr) {
  if (getInstance()->m_timer->isActive()) {
    getInstance()->m_timer->stop();
  }
  if (timePtr) {
    getInstance()->m_timer->start(timePtr->sec * 1000 + timePtr->usec / 1000);
  }
}

// What to do after the requested interval passes - always Tcl_ServiceAll()
void QtTclNotifier::handle_timer() {
  Tcl_ServiceAll();
}

// If events are available process them, and otherwise wait up to a specified interval for one to occur
int QtTclNotifier::WaitForEvent(Tcl_Time const* timePtr) {
  // following tclXtNotify.c here.  Hope the analogies hold.
  int timeout;
  if (timePtr) {
    timeout = timePtr->sec * 1000 + timePtr->usec / 1000;
    if (timeout == 0) {
      if (!QCoreApplication::hasPendingEvents()) {
	// timeout 0 means "do not block". There are no events, so return without processing
	return 0;
      }
    } else {
      // there are no events now, but maybe there will be some after we sleep the specified interval
      SetTimer(timePtr);
    }
  }
  // block if necessary until we have some events
  QCoreApplication::processEvents(QEventLoop::WaitForMoreEvents);
  return 1;
}

// Singleton class implementation trick
QtTclNotifier* QtTclNotifier::getInstance() {
  if (!m_notifier) {
    m_notifier = new QtTclNotifier();
  }
  return m_notifier;
}

// initialize static data storage
QtTclNotifier* QtTclNotifier::m_notifier = 0;
// xtor/dtor for singleton class.  Called when getInstance() is first called
QtTclNotifier::QtTclNotifier() {
  m_timer = new QTimer(this);
  m_timer->setSingleShot(true);
  QObject::connect(m_timer, SIGNAL(timeout()),
		   this, SLOT(handle_timer()));
}
QtTclNotifier::~QtTclNotifier() {}

// STUB METHODS

// we don't use the client data information for this notifier
// This could be helpful for multi-thread support, though. TBD
void* QtTclNotifier::InitNotifier() { return 0; }
void QtTclNotifier::FinalizeNotifier(ClientData) {}
void QtTclNotifier::AlertNotifier(ClientData) {}

// Can't find any examples of how this should work.  Unix implementation is empty
void QtTclNotifier::ServiceModeHook(int) {}



// only one method for QtTclFileHandler - executing the callback (with type check)
void QtTclFileHandler::perform_callback(int type,
					int fd      // only for debug - remove?
					) {
  // check that the mask includes the actual activity we had
  if (!(m_mask & type)) {
    qDebug("signal type received (%d) for fd %d should not be active!", type, fd);
    return;
  }

  // execute proc with stored client data
  (*m_proc)(m_clientData, type);
}
