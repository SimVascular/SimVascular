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


#ifndef	__USR_H__
#define	__USR_H__

/*===========================================================================
 *
 * "UsrHd":  User main struct
 *
 *===========================================================================
 */

typedef struct _Usr {
    int         eqnType ;               /* equation type flag             */
    double*	aperm ;			/* permanent data	          */
    double*	atemp ;			/* temporary data	          */
    double*	resf ;			/* residual vector	          */
    double*	solinc ;		/* solution increment	          */
    double*     flowDiag ;              /* global diagonal of total lhs   */
    double*     sclrDiag ;              /* global diagonal of energy lhs  */
    double*     lesP ;                  /* utility vector for Q = A * P   */
    double*     lesQ ;                  /* utility vector for Q = A * P   */
    int*        iBC  ;                  /* boundary condition code        */
    double*     BC   ;                  /* boundary condition array       */
    int*        iper ;                  /* periodic nodal information     */
    int*        ilwork ;                /* local MPI communication array  */
    int         numpe ;                 /* number of processors           */
    int		nNodes ;		/* number of nodes	          */
    int         nenl ;                  /* number of element nodes        */
    int		nPermDims ;		/* number of permanent data	  */
    int		nTmpDims ;		/* number of temporary data	  */
    int*       	rowp ;		        /* row of p for nonzero's of A    */
    int*       	colm ;		        /* start index for rowp vector    */
    double*     lhsK ;		        /* sparse K matrix (9,nnzeros)    */
    double*     lhsP ;		        /* sparse GoC matrix (4,nnzeros)  */
    double*     lhsS ;
    int         nnz_tot  ;              /* factor for number of nonzeros) */
    double*     CGsol;                  /* pdot after CG solve */
} Usr ;

typedef struct _Usr* UsrHd ;

/*===========================================================================
 *
 * C to Fortran naming conversion
 *
 *===========================================================================
 */

#ifdef SV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE

#define	usrNew		USRNEW
#define getSol          GETSOL

#define drvlesPrepDiag   DRVLESPREPDIAG
#define drvLesApG        DRVLESAPG
#define drvLesApKG       DRVLESAPKG
#define drvLesApNGt      DRVLESAPNGT
#define drvLesApNGtC     DRVLESAPNGTC
#define drvLesApFull     DRVLESAPFULL

#define commOut          COMMOUT
#define commIn           COMMIN
#define fLesSparseApSclr FLESSPARSEAPSCLR
#define fLesSparseApG        FLESSPARSEAPG
#define fLesSparseApKG       FLESSPARSEAPKG
#define fLesSparseApNGt      FLESSPARSEAPNGT
#define fLesSparseApNGtC     FLESSPARSEAPNGTC
#define fLesSparseApFull     FLESSPARSEAPFULL

#define drvsclrDiag      DRVSCLRDIAG
#define drvLesApSclr     DRVLESAPSCLR
#define drvAllreduce     DRVALLREDUCE
#define drvAllreducesclr DRVALLREDUCESCLR
#define	flesCp       FLESCP
#define	flesScale    FLESSCALE
#define	flesScaleCp  FLESSCALECP
#define	flesAdd      FLESADD
#define	flesSub      FLESSUB
#define	flesDot1     FLESDOT1
#define	flesDot2     FLESDOT2
#define	flesDaxpy    FLESDAXPY
#define	flesDxpay    FLESDXPAY
#define	flesInv      FLESINV
#define	flesZero     FLESZERO
#define fsclrDiag    FSCLRDIAG
#define flesApSclr   FLESAPSCLR
#define	fMtxVdimVecMult   FMTXVDIMVECMULT
#define	fMtxBlkDot2       FMTXBLKDOT2
#define	fMtxBlkDaxpy      FMTXBLKDAXPY
#define	fMtxBlkDyeax      FMTXBLKDYEAX
#define	fMtxBlkDmaxpy     FMTXBLKDMAXPY
#define	fMtxVdimVecCp     FMTXVDIMVECCP
#define	fMtxVdimVecDot2   FMTXVDIMVECDOT2
#define	fMtxVdimVecDaxpy  FMTXVDIMVECDAXPY

#define ramg_interface  RAMG_INTERFACE

#elif SV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE

#define	usrNew		usrnew_
#define getSol          getsol_

#define drvlesPrepDiag   drvlesprepdiag_
#define drvLesApG        drvlesapg_
#define drvLesApKG       drvlesapkg_
#define drvLesApNGt      drvlesapngt_
#define drvLesApNGtC     drvlesapngtc_
#define drvLesApFull     drvlesapfull_

#define commOut          commout_
#define commIn           commin_
#define fLesSparseApSclr flessparseapsclr_
#define fLesSparseApG        flessparseapg_
#define fLesSparseApKG       flessparseapkg_
#define fLesSparseApNGt      flessparseapngt_
#define fLesSparseApNGtC     flessparseapngtc_
#define fLesSparseApFull     flessparseapfull_

#define drvsclrDiag      drvsclrdiag_
#define drvLesApSclr     drvlesapsclr_
#define drvAllreduce     drvallreduce_
#define drvAllreducesclr drvallreducesclr_
#define	flesCp       flescp_
#define	flesScale    flesscale_
#define	flesScaleCp  flesscalecp_
#define	flesAdd      flesadd_
#define	flesSub      flessub_
#define	flesDot1     flesdot1_
#define	flesDot2     flesdot2_
#define	flesDaxpy    flesdaxpy_
#define	flesDxpay    flesdxpay_
#define	flesInv      flesinv_
#define	flesZero     fleszero_
#define fsclrDiag    fsclrdiag_
#define flesApSclr   flesapsclr_
#define	fMtxVdimVecMult   fmtxvdimvecmult_
#define	fMtxBlkDot2       fmtxblkdot2_
#define	fMtxBlkDaxpy      fmtxblkdaxpy_
#define	fMtxBlkDyeax      fmtxblkdyeax_
#define	fMtxBlkDmaxpy     fmtxblkdmaxpy_
#define	fMtxVdimVecCp     fmtxvdimveccp_
#define	fMtxVdimVecDot2   fmtxvdimvecdot2_
#define	fMtxVdimVecDaxpy  fmtxvdimvecdaxpy_

#define ramg_interface ramg_interface_

#endif

/*===========================================================================
 *
 * Function declaration
 *
 *===========================================================================
 */

void 		usrNew(			UsrHd		usrHd,
                                    int*            eqnType,
                                    double*		aperm,
                                    double*		atemp,
                                    double*         resf,
                                    double*         solinc,
                                    double*         flowDiag,
                                    double*         sclrDiag,
                                    double*         lesP,
                                    double*         lesQ,
                                    int*            iBC,
                                    double*         BC,
                                    int*            iper,
                                    int*            ilwork,
                                    int*            numpe,
                                    int*		nNodes,
                                    int*            nenl,
                                    int*		nPermDims,
                                    int*		nTmpDims,
                                    int*		rowp,
                                    int*		colm,
                                    double*		lhsK,
                                    double*		lhsP,
                                    double*         lhsS,
                                    int             nnz,
				    double*         CGsol) ;

double*	usrPointer(			UsrHd   usrHd,
                            int	id,
                            int	offset,
                            int	nDims ) ;

void              getSol(      UsrHd            usrHd,
                                        double*          Dy		) ;

/*===========================================================================
 *
 * Fortran Function declaration
 *
 *===========================================================================
 */
double    flesDot1(		double*		a,
                                    int*		m,
                                    int*		n		) ;
double	  flesDot2(		double*		a,
                                    double*		b,
                                    int*		m,
                                    int*		n		) ;

/*===========================================================================
 *
 * End of the file
 *
 *===========================================================================
 */

#endif	/* __USR_H__ */

