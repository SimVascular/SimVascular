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

/* System independent timer support.  Two types are supported, CPU
 * and WallClock.
 *
 * Currently supports (-D only one for each of cvCPUTimer and cvWallClockTimer)
 * To add a new one, you should only need to write the currentTime() routine.
 *
 *  cvCPUTimer
 *
 *	USE_GETRUSAGE if your system has getrusage() (newer than times)
 *	USE_TIMES if your system has times()
 *
 *  cvWallClockTimer
 *
 *	USE_GETTIMEOFDAY if your system has gettimeofday()
 *	USE_TIME if your system has time()
 */



#include "SimVascular.h" 

#include "cvTimer.h"

extern "C" { int getrusage( int, struct rusage* ); }

int cvBaseTimer::calculateGranularityOnConstructor = 0;


double cvBaseTimer::seconds() const
{
    cvTimeInBaseTimer tmp = currentTime();
    return tmp - savedTime_;
}


double cvBaseTimer::granularity()
{
    return getGranularity();
}


void cvBaseTimer::reset()
{
    savedTime_ = currentTime();
}


cvTimeInBaseTimer cvBaseTimer::currentTime() const
{
    cvTimeInBaseTimer t;

    return t;
}


double cvBaseTimer::getGranularity()
{
    return -1.0;
}


int cvCPUTimer::granularityKnown_ = 0;


cvCPUTimer::cvCPUTimer()
{
    if( calculateGranularityOnConstructor ) {
	getGranularity();
    }
    savedTime_ = currentTime();
}


double cvCPUTimer::getGranularity()
{
    if( !granularityKnown_ ) {
	cvTimeInBaseTimer t1;
	cvTimeInBaseTimer t2;

	t1 = currentTime();
	do {
	    t2 = currentTime();
	} while( t1 == t2 );

	granularity_ = t2 - t1;
	granularityKnown_ = 1;
    }
    return granularity_;
}


int cvWallClockTimer::granularityKnown_ = 0;


cvWallClockTimer::cvWallClockTimer()
{
    if( calculateGranularityOnConstructor ) {
	getGranularity();
    }
    savedTime_ = currentTime();
}


double cvWallClockTimer::getGranularity()
{
    if( !granularityKnown_ ) {
	cvTimeInBaseTimer t1;
	cvTimeInBaseTimer t2;

	t1 = currentTime();
	do {
	    t2 = currentTime();
	} while( t1 == t2 );

	granularity_ = t2 - t1;
	granularityKnown_ = 1;
    }
    return granularity_;
}


#ifdef USE_GETRUSAGE

#ifdef HAVE_CPU_CURRENTTIME
error error error.  You probably have incompatible defines.  Check the 
documentation at the top.
#endif
#define HAVE_CPU_CURRENTTIME

#include <sys/time.h>
#include <sys/resource.h>

extern "C" 
{
   int getrusage( int, struct rusage* );
}



cvTimeInBaseTimer cvCPUTimer::currentTime() const
{
    cvTimeInBaseTimer t;
    struct rusage tmp;

    getrusage( RUSAGE_SELF, &tmp );
    t.seconds_ = tmp.ru_utime.tv_sec;
    t.nseconds_ = tmp.ru_utime.tv_usec * 1000;

    return t;
}

#endif /* USE_GETRUSAGE */


#ifdef USE_TIMES

#ifdef HAVE_CPU_CURRENTTIME
error error error.  You probably have incompatible defines.  Check the 
documentation at the top.
#endif
#define HAVE_CPU_CURRENTTIME

#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>

// some systems have HZ defined in <sys/param.h>.

#ifndef HZ
#define HZ 60
#endif


cvTimeInBaseTimer cvCPUTimer::currentTime() const
{
    cvTimeInBaseTimer t;
    struct tms tmp;

    times( &tmp );
    t.seconds_ = tmp.tms_utime / HZ;
    t.nseconds_ = (long) ((tmp.tms_utime % HZ) * (1.0e9 / HZ));

    return t;
}

#endif /* USE_TIMES */


#ifdef USE_TIME

#ifdef HAVE_WALLCLOCK_CURRENTTIME
error error error.  You probably have incompatible defines.  Check the 
documentation at the top.
#endif
#define HAVE_WALLCLOCK_CURRENTTIME

#include <sys/types.h>
#include <sys/time.h>

cvTimeInBaseTimer cvWallClockTimer::currentTime() const
{
    cvTimeInBaseTimer t;
    time_t tloc;

    time(&tloc);
    t.seconds_ = tloc;
    t.nseconds_ = 0;

    return t;
}

#endif /* USE_TIME */


#ifdef USE_GETTIMEOFDAY

#ifdef HAVE_WALLCLOCK_CURRENTTIME
error error error.  You probably have incompatible defines.  Check the 
documentation at the top.
#endif
#define HAVE_WALLCLOCK_CURRENTTIME


#include <sys/time.h>

cvTimeInBaseTimer cvWallClockTimer::currentTime() const
{
    cvTimeInBaseTimer t;
    struct timeval tp;
    struct timezone tzp;

    gettimeofday( &tp, &tzp );
    t.seconds_ = tp.tv_sec;
    t.nseconds_ = tp.tv_usec * 1000;

    return t;
}

#endif /* USE_GETTIMEOFDAY */

#ifdef USE_NOTIMER

#define HAVE_CPU_CURRENTTIME

cvTimeInBaseTimer cvCPUTimer::currentTime() const
{
    cvTimeInBaseTimer t;
    t.seconds_ = 0;
    t.nseconds_ = 0;

    return t;
}

#define HAVE_WALLCLOCK_CURRENTTIME

cvTimeInBaseTimer cvWallClockTimer::currentTime() const
{
    cvTimeInBaseTimer t;

    t.seconds_ = 0;
    t.nseconds_ = 0;

    return t;
}

#endif /* USE_NOTIMER */

