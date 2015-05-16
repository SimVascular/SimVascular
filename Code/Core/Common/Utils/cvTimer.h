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

#ifndef SU_Timer_h
#define SU_Timer_h


// This hack is present because Sun CC (SC1.0) does not fully implement
// nested classes.

class cvTimeInBaseTimer {
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



class cvBaseTimer {
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


class cvCPUTimer : public cvBaseTimer {
public:
    cvCPUTimer();

protected:
    cvTimeInBaseTimer currentTime() const;
    double getGranularity();

private:
    static int granularityKnown_;
};


class cvWallClockTimer : public cvBaseTimer {
public:
    cvWallClockTimer();

protected:
    cvTimeInBaseTimer currentTime() const;
    double getGranularity();

private:
    static int granularityKnown_;
};


#endif /* SU_Timer_h */
