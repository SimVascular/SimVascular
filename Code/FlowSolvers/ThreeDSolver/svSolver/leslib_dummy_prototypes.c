/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Rensselaer Polytechnic Institute, Charles A. Taylor,
 * Kenneth E. Jansen.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:

 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * Neither the name of the Stanford University or Rensselaer Polytechnic
 * Institute nor the names of its contributors may be used to endorse or
 * promote products derived from this software without specific prior
 * written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

#ifdef SV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE
  #define myflesnew_   MYFLESNEW
  #define myflessolve_ MYFLESSOLVE
  #define savelesrestart_ SAVELESRESTART
  #define readlesrestart_ READLESRESTART
  #define solverlicenseserver_ SOLVERLICENSESERVER
  #define getsol_ GETSOL
  #define usrnew_ USRNEW      
#endif

/* nothing to be done for SV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE case */

typedef	int	Integer	;		/* integer type			*/
typedef	double	Real ;			/* real    type			*/
typedef	float	Float ;			/* float   type			*/
typedef	char*	String ;		/* string  type			*/
typedef	void*	Data ;			/* data    type			*/
typedef	void	Void ;			/* void    type			*/

/* ignore real usrHd data structure */
typedef int UsrHd;

void    myflesnew_(	 Integer*	lesId,
                         Integer*	lmport,                             
                         Integer*	eqnType,
                         Integer*	nDofs,
                         Integer*	minIters,
                         Integer*	maxIters,
                         Integer*	nKvecs,
                         Integer*	prjFlag,
                         Integer*	nPrjs,
                         Integer*	presPrjFlag,
                         Integer*	nPresPrjs,
                         Real*	        tol,
                         Real*     	presTol,
                         Integer*	verbose,
                         Real*     	stats,
                         Integer*	nPermDims,
                         Integer*	nTmpDims,
                         char*          lmhost          ) {
    return ;
}

void
savelesrestart_( Integer* lesId,
                 Real*    aperm,
                 Integer* nshg,
                 Integer* myrank,
                 Integer* lstep,
                 Integer* nPermDims ) {

  return;
}

void
readlesrestart_( Integer* lesId,
                 Real*    aperm,
                 Integer* nshg,
                 Integer* myrank,
                 Integer* lstep ,
                 Integer* nPermDims ) {

  return;
}

void  myflessolve_( Integer* lesId,
                    UsrHd    usrHd){
    return;

}

int solverlicenseserver_(char key[]){
    return 1;
}

void getsol_ ( UsrHd usrHd,
              double* Dy  )
{
    return;
}

void   usrnew_(	UsrHd	  usrHd,
                        int*      eqnType,
                        double*	  aperm,
                        double*	  atemp,
                        double*   resf,
                        double*   solinc,
                        double*   flowDiag,
                        double*   sclrDiag,
                        double*   lesP,
                        double*   lesQ,
                        int*      iBC,
                        double*   BC,
                        int*      iper,
                        int*      ilwork,
                        int*      numpe,
                        int*      nNodes,
                        int*      nenl,
                        int*	  nPermDims,
                        int*	  nTmpDims,
                        int*	  rowp,
                        int*	  colm,
                        double*   lhsK,
                        double*   lhsP,
                        double*   lhsS,
                        int       nnz_tot,
                        double*   CGsol
    )
{

  return;

}
