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

#ifndef SU_Timer_h
#define SU_Timer_h

#include "SimVascular.h"
#include "svLSetExports.h" // For exports

// This hack is present because Sun CC (SC1.0) does not fully implement
// nested classes.

class SV_EXPORT_LSET cvTimeInBaseTimer {
public:
    inline cvTimeInBaseTimer();
    inline double operator -( const cvTimeInBaseTimer& ) const;
    inline int operator ==( const cvTimeInBaseTimer& ) const;

    long seconds_;
    long nseconds_;			// 2^31-1 will hold 1,000,000,000
};


inline cvTimeInBaseTimer::cvTimeInBaseTimer()
{
    seconds_ = 0;
    nseconds_ = 0;
}


inline double cvTimeInBaseTimer::operator -( const cvTimeInBaseTimer& t ) const
{
    return seconds_ - t.seconds_ + 1.0e-9 * (nseconds_ - t.nseconds_);
}


inline int cvTimeInBaseTimer::operator ==( const cvTimeInBaseTimer& t ) const
{
    return seconds_ == t.seconds_  &&  nseconds_ == t.nseconds_;
}



class SV_EXPORT_LSET cvBaseTimer {
public:
    double seconds() const;
    double granularity();
    void reset();

    static int calculateGranularityOnConstructor;

protected:
    virtual cvTimeInBaseTimer currentTime() const;
    virtual double getGranularity();

    cvTimeInBaseTimer savedTime_;
    double granularity_;
};


class SV_EXPORT_LSET cvCPUTimer : public cvBaseTimer {
public:
    cvCPUTimer();

protected:
    cvTimeInBaseTimer currentTime() const;
    double getGranularity();

private:
    static int granularityKnown_;
};


class SV_EXPORT_LSET cvWallClockTimer : public cvBaseTimer {
public:
    cvWallClockTimer();

protected:
    cvTimeInBaseTimer currentTime() const;
    double getGranularity();

private:
    static int granularityKnown_;
};


#endif /* SU_Timer_h */
