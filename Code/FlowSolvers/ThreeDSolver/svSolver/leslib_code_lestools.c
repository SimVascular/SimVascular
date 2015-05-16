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

/*======================================================================
 *
 * lestools.c : Linear Algebra Solver Tools
 *
 * small single character : vector or matrix
 *
 *======================================================================
 */
#include "leslib_code_usr.h"
#include "les.h"

/*----------------------------------------------------------------------
 *
 * lesPreDiag
 *
 *    operation : a = 1/sqrt(abs(a))
 *
 *----------------------------------------------------------------------
 */
void lesPrepDiag( UsrHd  usrHd  )
{
    char*   funcName = "lesPrepDiag" ; /* function name */

    if ( (usrHd->eqnType) == 1 ) {  /* continuity & momentum */

      drvlesPrepDiag( usrHd->flowDiag,
                      usrHd->ilwork,  usrHd->iBC,
		      usrHd->BC,      usrHd->iper,
		      usrHd->rowp,    usrHd->colm,
		      usrHd->lhsK,    usrHd->lhsP) ;
    }

    if ( (usrHd->eqnType) == 2 ) { /* temperature or scalar variable */
 
      drvsclrDiag ( usrHd->sclrDiag, usrHd->ilwork,
		    usrHd->iBC,      usrHd->BC,
		    usrHd->iper,     usrHd->rowp,
		    usrHd->colm,     usrHd->lhsS ) ;
		    
    }

}
	
/*----------------------------------------------------------------------
 *
 * lesDiagScaleCp
 *
 *    operation : c = a * b
 *
 *----------------------------------------------------------------------
 */
void lesDiagScaleCp ( UsrHd   usrHd,
                      Integer srcId,
                      Integer dstId,
                      Integer nSrcDims,
                      Integer srcOff,
                      Integer nDstDims,
                      Integer dstOff,
                      Integer diagOff,
                      Integer nDims )
{
    char*    funcName = "lesDiagScaleCp" ; /* function name */
    Integer  nDofs ;                       /* No. of Dofs   */
    Real*    dstpnt ;                      /* destination   */
    Real*    srcpnt ;                      /* source */
 
    if ( (usrHd->eqnType) == 1 ) {  

      nDofs    = 4 ;

      srcpnt   = usrPointer ( usrHd, srcId, srcOff, nSrcDims ) ;
      dstpnt   = usrPointer ( usrHd, dstId, dstOff, nDstDims ) ;

      fMtxVdimVecMult( srcpnt,
                       usrHd->flowDiag+diagOff*usrHd->nNodes,
                       dstpnt,
                       &nSrcDims,
                       &nDofs,
                       &nDstDims,
                       &nDims,
                       &(usrHd->nNodes) ) ;
    }
 
    if ( (usrHd->eqnType) == 2 )  {

      nDofs    = 1 ;

      srcpnt   = usrPointer ( usrHd, srcId, srcOff, nSrcDims ) ;
      dstpnt   = usrPointer ( usrHd, dstId, dstOff, nDstDims ) ;

      fMtxVdimVecMult( srcpnt,
                       usrHd->sclrDiag+diagOff*usrHd->nNodes,
                       dstpnt,
                       &nSrcDims,
                       &nDofs,
                       &nDstDims,
                       &nDims,
                       &(usrHd->nNodes) ) ;
    }

} 

/*----------------------------------------------------------------------
 *
 * lesZero
 *
 *    operation : a = 0
 *
 *----------------------------------------------------------------------
 */
void lesZero ( UsrHd   usrHd,
               Integer dstId,
               Integer nDims )
{   
    char*      funcName = "lesZero" ;  /* function namea        */
    Real*      dstpnt ;                /* destination           */   
    Integer    dstOff ;                /* destination offset    */


    dstOff     = 0 ;

    dstpnt     = usrPointer ( usrHd, dstId, dstOff, nDims );

    flesZero( dstpnt, &nDims, &(usrHd->nNodes) );
}

/*----------------------------------------------------------------------
 *
 * lesCp 
 *
 *    operation : b = a
 *
 *-----------------------------------------------------------------------
 */
void lesCp ( UsrHd   usrHd,
             Integer srcId,
             Integer dstId,
             Integer nDims )
{
    char*    funcName = "lesCp" ; /* function name      */
    Real*    srcpnt ;             /* source             */
    Real*    dstpnt ;             /* destination        */
    Integer  dim ;                /* a simple dimension */
    Integer  srcOff ;             /* source offset      */
    Integer  dstOff ;             /* destination offset */

    srcOff   = 0 ;
    dstOff   = 0 ;

    srcpnt   = usrPointer ( usrHd, srcId, srcOff, nDims ) ;
    dstpnt   = usrPointer ( usrHd, dstId, dstOff, nDims ) ;

    dim      = usrHd->nNodes ;

    flesCp( srcpnt, dstpnt, &nDims, &dim ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesScale
 *    
 *    operation : a = a * coef
 *
 *-----------------------------------------------------------------------
 */
void lesScale ( UsrHd   usrHd,
                Integer dstId,
                Real    coef,
                Integer nDims )
{
    char*       funcName = "lesScale" ; /* function name      */
    Real*       dstpnt ;                /* destination        */
    Integer     dstOff ;                /* destination offset */
    Integer     dim ;                   /* a simple dimension */

    dstOff      = 0 ;

    dim         = usrHd->nNodes ;

    dstpnt      = usrPointer( usrHd, dstId, dstOff, nDims ) ;

    flesScale( dstpnt,
               &coef,
               &nDims,
               &dim ) ;
}
/*-----------------------------------------------------------------------
 *
 * lesScaleCp
 *
 *    operation : b = a * coef
 *
 *-----------------------------------------------------------------------
 */
void lesScaleCp ( UsrHd   usrHd,
                  Integer srcId,
                  Integer dstId,
                  Real    coef,
                  Integer nDims )
{
    char*         funcName = "lesScaleCp" ; /* function name      */
    Real*         srcpnt ;                  /* source             */
    Real*         dstpnt ;                  /* destination        */
    Integer       dim ;                     /* a simple dimension */
    Integer       srcOff ;                  /* source offset      */
    Integer       dstOff ;                  /* destination offset */

    srcOff        = 0 ;
    dstOff        = 0 ;

    srcpnt        = usrPointer ( usrHd, srcId, srcOff, nDims ) ;
    dstpnt        = usrPointer ( usrHd, dstId, dstOff, nDims ) ;

    dim           = usrHd->nNodes ;

    flesScaleCp( srcpnt, 
                 dstpnt,
                 &coef,
                 &nDims,
                 &dim ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesAdd
 *
 *    operation : b = b + a
 *
 *-----------------------------------------------------------------------
 */
void lesAdd ( UsrHd   usrHd,
              Integer srcId,
              Integer dstId,
              Integer nDims )
{
    char*     funcName = "lesAdd" ; /* function name      */
    Real*     srcpnt ;              /* source             */
    Real*     dstpnt ;              /* destination        */
    Integer   srcOff ;              /* source offset      */  
    Integer   dstOff ;              /* destination offset */
    Integer   dim ;                 /* a simple dimension */ 

    srcOff    = 0 ;
    dstOff    = 0 ;
   
    srcpnt    = usrPointer ( usrHd, srcId, srcOff, nDims ) ;
    dstpnt    = usrPointer ( usrHd, dstId, dstOff, nDims ) ;

    dim       = usrHd->nNodes ;

    flesAdd( srcpnt, 
             dstpnt,
             &nDims,
             &dim ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesSub
 *
 *    operation : b = b - a
 *
 *-----------------------------------------------------------------------
 */
void lesSub ( UsrHd   usrHd,
              Integer srcId,
              Integer dstId,
              Integer nDims )
{
     char*    funcName = "lesSub" ; /* function name      */
     Real*    srcpnt ;              /* source             */
     Real*    dstpnt ;              /* destination        */
     Integer  srcOff ;              /* source offset      */
     Integer  dstOff ;              /* destination offset */
     Integer  dim ;                 /* a simple dimension */

     srcOff   = 0 ;
     dstOff   = 0 ;
   
     srcpnt   = usrPointer ( usrHd, srcId, srcOff, nDims ) ; 
     dstpnt   = usrPointer ( usrHd, dstId, dstOff, nDims ) ;

     dim      = usrHd->nNodes ;

     flesSub( srcpnt, 
              dstpnt,
              &nDims,
              &dim ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesDot1
 *
 *    operation : tmp = tmp + a * a
 *
 *-----------------------------------------------------------------------
 */
Real lesDot1 ( UsrHd   usrHd,
               Integer srcId,
               Integer nDims )
{
    char*      funcName = "lesDot1" ; /* function name                   */
    Real*      srcpnt ;               /* source                          */
    Integer    srcOff ;               /* source offset                   */
    Integer    dim ;                  /* a simple dimension              */
    Real       tmp ;                  /* a temporary value               */
    Real       tmpp ;                 /* a temporary value on each proc. */

    srcOff     = 0 ;

    srcpnt     = usrPointer ( usrHd, srcId, srcOff, nDims ) ;

    dim        = usrHd->nNodes ;

    tmpp        = flesDot1( srcpnt,
                            &nDims,
                            &dim ) ;

    drvAllreducesclr ( &tmpp,
                       &tmp ) ;
             

    return ( tmp ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesDot2
 *
 *    operation : tmp = tmp + a * b
 *
 *-----------------------------------------------------------------------
 */
Real lesDot2 ( UsrHd   usrHd,
               Integer src1Id,  
               Integer src2Id, 
               Integer nDims )
{
    char*      funcName = "lesDot2" ; /* function name                   */
    Real*      src1pnt ;              /* source 1                        */
    Real*      src2pnt ;              /* source 2                        */
    Integer    src1Off ;              /* source 1 offset                 */
    Integer    src2Off ;              /* source 2 offset                 */
    Integer    dim ;                  /* a simple dimension              */
    Real       tmp ;                  /* a temporary value               */
    Real       tmpp ;                 /* a temporary value on each proc. */

    src1Off    = 0 ;
    src2Off    = 0 ;

    src1pnt    = usrPointer ( usrHd, src1Id, src1Off, nDims );
    src2pnt    = usrPointer ( usrHd, src2Id, src2Off, nDims );

    dim        = usrHd->nNodes ;

    tmpp       = flesDot2( src1pnt,
                           src2pnt,
                           &nDims,
                           &dim ) ;

    drvAllreducesclr ( &tmpp,
                       &tmp ) ;

    return ( tmp ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesDaxpy
 *
 *    operation : y = y + coef * x
 *
 *-----------------------------------------------------------------------
 */
void lesDaxpy ( UsrHd   usrHd,
                Integer srcId,
                Integer dstId,
                Real    coef,
                Integer nDims )
{
    char*       funcName = "lesDapxy" ; /* function name      */
    Real*       srcpnt ;                /* source             */
    Real*       dstpnt ;                /* destination        */
    Integer     srcOff ;                /* source offset      */
    Integer     dstOff ;                /* destination offset */
    Integer     dim ;                   /* a simple dimension */
 
    srcOff      = 0 ;   
    dstOff      = 0 ;

    srcpnt      = usrPointer ( usrHd, srcId, srcOff, nDims ) ;
    dstpnt      = usrPointer ( usrHd, dstId, dstOff, nDims ) ;

    dim         = usrHd->nNodes ;

    flesDaxpy( srcpnt,
               dstpnt,
               &coef,
               &nDims,
               &dim ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesDxpay
 *
 *    operation : y = coef * y + x
 *
 *-----------------------------------------------------------------------
 */
void lesDxpay ( UsrHd   usrHd,
                Integer srcId,
                Integer dstId,
                Real    coef,
                Integer nDims )
{
    char*       funcName = "lesDxpay" ; /* function name      */
    Real*       srcpnt ;                /* source             */
    Real*       dstpnt ;                /* destination        */
    Integer     srcOff ;                /* source offset      */
    Integer     dstOff ;                /* destination offset */
    Integer     dim ;                   /* a simple dimension */

    srcOff      = 0 ;
    dstOff      = 0 ;

    srcpnt      = usrPointer ( usrHd, srcId, srcOff, nDims ) ;
    dstpnt      = usrPointer ( usrHd, dstId, dstOff, nDims ) ;

    dim         = usrHd->nNodes ;

    flesDxpay( srcpnt,
               dstpnt,
               &coef,
               &nDims,
               &dim ) ;
}
    
/*-----------------------------------------------------------------------
 *
 * lesInv
 *
 *    operation : x = 1. / x
 *
 *-----------------------------------------------------------------------
 */
void lesInv ( UsrHd   usrHd, 
              Integer dstId,
              Integer nDims )
{
    char*     funcName = "lesInv" ; /* function name      */
    Integer   dim ;                 /* a simple dimension */
    Real*     dstpnt ;              /* destination        */
    Integer   dstOff ;              /* destination offset */

    dstOff    = 0 ;

    dstpnt    = usrPointer ( usrHd, dstId, dstOff, nDims ) ;

    dim       = usrHd->nNodes ;

    flesInv( dstpnt, &nDims, &dim ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesBlkDot2
 *
 *    operation :
 *
 *-----------------------------------------------------------------------
 */
void lesBlkDot2 ( UsrHd   usrHd,
                  Integer src1Id,
                  Integer src2Id,
                  Real*   values,
                  Integer mDims,
                  Integer nDims )
{
    char*         funcName = "lesBlkDot2" ; /* function name      */
    Real*         src1pnt ;                 /* source 1           */
    Real*         src2pnt ;                 /* source 2           */ 
    Integer       src1Off ;                 /* source 1 offset    */
    Integer       src2Off ;                 /* source 2 offset    */
    Integer       dim ;                     /* a simple dimension */
    Real*         valuesp ;                 /* temporary values on proc */


    if ( mDims * nDims == 0 ) {
         return ;
    }

    valuesp = (double *) malloc (mDims * sizeof(double)) ;

    src1Off       = 0 ;
    src2Off       = 0 ;

    src1pnt       = usrPointer ( usrHd, src1Id, src1Off, nDims ) ;
    src2pnt       = usrPointer ( usrHd, src2Id, src2Off, nDims ) ;

    dim           = nDims * usrHd->nNodes ;

    fMtxBlkDot2( src1pnt, 
                 src2pnt,
                 valuesp,
                 &mDims,
                 &dim ) ;

    drvAllreduce ( valuesp,
                   values,
                   &mDims ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesBlkDaxpy
 *
 *    operation :
 *
 *-----------------------------------------------------------------------
 */
void lesBlkDaxpy ( UsrHd   usrHd,
                   Integer srcId,
                   Integer dstId,
                   Real*   coef,
                   Integer mDims,
                   Integer nDims )
{
    char*          funcName = "lesBlkDaxpy" ; /* function name      */
    Real*          srcpnt ;                   /* source             */
    Real*          dstpnt ;                   /* destination        */
    Integer        srcOff ;                   /* source offset      */
    Integer        dstOff ;                   /* destination offset */
    Integer        dim ;                      /* a simple dimension */

    if ( mDims * nDims == 0 ) {
         return ;
    }

    srcOff         = 0 ;
    dstOff         = 0 ;

    srcpnt         = usrPointer ( usrHd, srcId, srcOff, nDims ) ;
    dstpnt         = usrPointer ( usrHd, dstId, dstOff, nDims ) ;

    dim            = nDims * usrHd->nNodes ;

    fMtxBlkDaxpy( srcpnt,
                  dstpnt,
                  coef,
                  &mDims,
                  &dim );
}

/*-----------------------------------------------------------------------
 *
 * lesBlkDyeax
 *
 *    operation :
 *
 *-----------------------------------------------------------------------
 */
void lesBlkDyeax ( UsrHd   usrHd,
                   Integer srcId,
                   Integer dstId,
                   Real*   coef,
                   Integer mDims,
                   Integer nDims )
{   
    char*          funcName = "lesBlkDyeax" ; /* function name      */
    Real*          srcpnt ;                   /* source             */
    Real*          dstpnt ;                   /* destination        */
    Integer        srcOff ;                   /* source offset      */
    Integer        dstOff ;                   /* destination offset */
    Integer        dim ;                      /* a simple dimension */

    if ( mDims * nDims == 0 ) {
        lesZero ( usrHd, dstId, nDims ) ;
        return ;
    }

    srcOff         = 0 ;
    dstOff         = 0 ;

    srcpnt         = usrPointer ( usrHd, srcId, srcOff, nDims ) ;
    dstpnt         = usrPointer ( usrHd, dstId, dstOff, nDims ) ;

    dim            = nDims * usrHd->nNodes ;

    fMtxBlkDyeax( srcpnt,
                  dstpnt,
                  coef,
                  &mDims,
                  &dim ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesBlkDmaxpy
 *
 *    operation :
 *
 *-----------------------------------------------------------------------
 */
void lesBlkDmaxpy ( UsrHd   usrHd,
                    Integer srcId,
                    Integer dstId,
                    Real*   coef,
                    Integer mDims,
                    Integer nDims )
{
    char*           funcName = "lesBlkDmaxpy" ; /* function name      */
    Real*           srcpnt ;                    /* source             */
    Real*           dstpnt ;                    /* destination        */
    Integer         srcOff ;                    /* source offset      */
    Integer         dstOff ;                    /* destination offset */
    Integer         dim ;                       /* a simple dimension */

    if ( mDims * nDims == 0 ) {
        return ;
    }

    srcOff          = 0 ;
    dstOff          = 0 ;

    srcpnt          = usrPointer ( usrHd, srcId, srcOff, nDims ) ;
    dstpnt          = usrPointer ( usrHd, dstId, dstOff, nDims ) ;

    dim             = nDims * usrHd->nNodes ;

    fMtxBlkDmaxpy( srcpnt,
                   dstpnt,
                   coef,
                   &mDims,
                   &dim );
}

/*-----------------------------------------------------------------------
 *
 * lesVdimCp
 *
 *    operation :
 *
 *-----------------------------------------------------------------------
 */
void lesVdimCp ( UsrHd   usrHd,
                 Integer srcId,
                 Integer dstId,
                 Integer nSrcDims,
                 Integer srcOff,
                 Integer nDstDims,
                 Integer dstOff,
                 Integer nDims )
{
    char*        funcName = "lesVdimCp"; /* function name */
    Real*        srcpnt ;                /* source        */
    Real*        dstpnt ;                /* destination   */

    srcpnt       = usrPointer ( usrHd, srcId, srcOff, nSrcDims ) ;
    dstpnt       = usrPointer ( usrHd, dstId, dstOff, nDstDims ) ;

    fMtxVdimVecCp( srcpnt,
                   dstpnt,
                   &nSrcDims,
                   &nDstDims,
                   &nDims,
                   &(usrHd->nNodes) );
}

/*-----------------------------------------------------------------------
 *
 * lesVdimDot2
 *
 *    operation :
 *
 *-----------------------------------------------------------------------
 */
void lesVdimDot2 ( UsrHd   usrHd,
                   Integer src1Id,
                   Integer src2Id,
                   Real*   coef,
                   Integer nSrc1Dims,
                   Integer src1Off,
                   Integer nSrc2Dims,
                   Integer src2Off,
                   Integer nDims )
{
    char*          funcName = "lesVdimDot2" ; /* function name */
    Real*          src1pnt ;                  /* source 1      */
    Real*          src2pnt ;                  /* source 2      */
    Real*          coefp ;                    /* temporary coef on proc */

 
    if ( nDims == 0 ) {
        return ;
    }

    coefp = (double *) malloc (nDims * sizeof(double)) ;

    src1pnt        = usrPointer ( usrHd, src1Id, src1Off, nSrc1Dims ) ;
    src2pnt        = usrPointer ( usrHd, src2Id, src2Off, nSrc2Dims ) ;

    fMtxVdimVecDot2( src1pnt,
                     src2pnt,
                     coefp,
                     &nSrc1Dims,
                     &nSrc2Dims,
                     &nDims,
                     &(usrHd->nNodes) );

    drvAllreduce ( coefp,
                   coef,
                   &nDims ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesVdimDaxpy
 *
 *    operation :
 *
 *-----------------------------------------------------------------------
 */
void lesVdimDaxpy ( UsrHd   usrHd,
                    Integer srcId,
                    Integer dstId,
                    Real*   coef,
                    Integer nSrcDims,
                    Integer srcOff,
                    Integer nDstDims,
                    Integer dstOff,
                    Integer nDims )
{
    char*           funcName = "lesVdimDaxpy" ; /* function name */
    Real*           srcpnt ;                    /* source        */
    Real*           dstpnt ;                    /* destination   */

    if ( nDims == 0 ) {
        return ;
    }

    srcpnt          = usrPointer ( usrHd, srcId, srcOff, nSrcDims ) ;
    dstpnt          = usrPointer ( usrHd, dstId, dstOff, nDstDims ) ;

    fMtxVdimVecDaxpy( srcpnt,
                      dstpnt,
                      coef,
                      &nSrcDims,
                      &nDstDims,
                      &nDims,
                      &(usrHd->nNodes) ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesApG
 *
 *    operation : G(:,3*nenl,nenl) * Dp(:,nenl,1) = lesQ(;,nenl,3)
 *
 *-----------------------------------------------------------------------
 */
void lesApG ( UsrHd   usrHd,
              Integer srcId,
              Integer dstId,
              Integer nSrcDims,
              Integer srcOff,
              Integer nDstDims,
              Integer dstOff )
{
    char*     funcName = "lesApG" ; /* function name      */
    Integer   nDofs ;               /* No. of dofs        */
    Integer   nPs ;                 /* No. of P dimension */
    Integer   nQs ;                 /* No. of Q dimension */
    Integer   pOff ;                /* offset             */
    Integer   qOff ;                /* offset             */
    Real*     srcpnt ;              /* source             */
    Real*     dstpnt ;              /* destin             */

    nDofs     = 4 ;
    nPs       = 1 ;
    nQs       = 3 ;
    pOff      = 3 * usrHd->nNodes ;
    qOff      = 0 * usrHd->nNodes ;
    
    srcpnt    = usrPointer ( usrHd, srcId, srcOff, nSrcDims ) ;

    fMtxVdimVecMult( srcpnt,
                     usrHd->flowDiag+pOff,
                     usrHd->lesP,
                     &nSrcDims,
                     &nDofs,
                     &nPs,
                     &nPs,
                     &(usrHd->nNodes) ) ;

    commOut ( usrHd->lesP, usrHd->ilwork, &nPs,
	      usrHd->iper, usrHd->iBC, usrHd->BC );

    fLesSparseApG( usrHd->colm, usrHd->rowp, usrHd->lhsP,
		   usrHd->lesP, usrHd->lesQ, &(usrHd->nNodes),
		   &(usrHd->nnz_tot));

    commIn ( usrHd->lesQ, usrHd->ilwork, &nQs,
	     usrHd->iper, usrHd->iBC, usrHd->BC );
    
    dstpnt     = usrPointer ( usrHd, dstId, dstOff, nDstDims ) ;

    fMtxVdimVecMult( usrHd->lesQ,
                     usrHd->flowDiag+qOff,
                     dstpnt,
                     &nQs,
                     &nDofs,
                     &nDstDims,
                     &nQs,
                     &(usrHd->nNodes) ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesApKG
 *
 *    operation : K(:,3*nenl,3*nenl) * Du(:,nenl,3) = lesQ(:,nenl,3)
 *                G(:,3*nenl,  nenl) * Dp(:,nenl,1) = lesQ(:,nenl,3) 
 *
 *-----------------------------------------------------------------------
 */
void lesApKG ( UsrHd   usrHd,
               Integer src1Id,
               Integer src2Id,
               Integer dstId,
               Integer nSrc1Dims,
               Integer src1Off,
               Integer nSrc2Dims,
               Integer src2Off,
               Integer nDstDims,
               Integer dstOff )
{
    char*      funcName = "lesApKG" ; /* function name      */
    Integer    nDofs ;                /* No. of Dofs        */
    Integer    p1Off ;                /* Diag offset for P  */
    Integer    p2Off ;                /* Diag offset for Q  */
    Integer    nP1s ;                 /* No. of P dimension */
    Integer    nP2s ;                 /* No. of P dimension */
    Integer    nPs ;                  /* No. of P dimension */
    Integer    nQs ;                  /* No. of Q dimension */
    Integer    qOff ;                 /* offset             */
    Real*      src1pnt ;              /* Source 1           */
    Real*      src2pnt ;              /* Source 2           */
    Real*      dstpnt ;               /* destination        */
 
    nDofs      = 4 ;
    nP1s       = 3 ;
    nP2s       = 1 ;
    nPs        = nP1s + nP2s ;
    nQs        = 3 ;
    p1Off      = 0 * usrHd->nNodes ;
    p2Off      = 3 * usrHd->nNodes ;
    qOff       = 0 * usrHd->nNodes ;

    src1pnt    = usrPointer ( usrHd, src1Id, src1Off, nSrc1Dims ) ;
    src2pnt    = usrPointer ( usrHd, src2Id, src2Off, nSrc2Dims ) ;

    fMtxVdimVecMult( src1pnt,
                     usrHd->flowDiag+p1Off,
                     usrHd->lesP+p1Off,
                     &nSrc1Dims,
                     &nDofs,
                     &nPs,
                     &nP1s,
                     &(usrHd->nNodes) ) ;

    fMtxVdimVecMult( src2pnt,
                     usrHd->flowDiag+p2Off,
                     usrHd->lesP+p2Off,
                     &nSrc2Dims,
                     &nDofs,
                     &nPs,
                     &nP2s,
                     &(usrHd->nNodes) );

    commOut ( usrHd->lesP, usrHd->ilwork, &nPs,
	      usrHd->iper, usrHd->iBC, usrHd->BC  );

    fLesSparseApKG( usrHd->colm, usrHd->rowp, usrHd->lhsK,
		    usrHd->lhsP, usrHd->lesP, usrHd->lesQ, 
		    &(usrHd->nNodes),&(usrHd->nnz_tot));

    commIn ( usrHd->lesQ, usrHd->ilwork, &nQs,
	     usrHd->iper, usrHd->iBC, usrHd->BC  );
    
      
    dstpnt     = usrPointer ( usrHd, dstId, dstOff, nDstDims ) ; 

    fMtxVdimVecMult( usrHd->lesQ,
		     usrHd->flowDiag+qOff,
		     dstpnt,
		     &nQs,
		     &nDofs,
		     &nDstDims,
		     &nQs,
		     &(usrHd->nNodes) ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesApNGt
 *
 *    operation : -G^t(:,nenl,3*nenl) * Du(:,nenl,3) = lesQ(:,nenl,1)
 *
 *-----------------------------------------------------------------------
 */
void lesApNGt ( UsrHd   usrHd,
                Integer srcId,
                Integer dstId,
                Integer nSrcDims,
                Integer srcOff,
                Integer nDstDims,
                Integer dstOff )
{   
    char*       funcName = "lesApNGt" ; /* function name      */
    Integer     pOff ;                  /* Diag offset for P  */
    Integer     qOff ;                  /* Diag offset for Q  */
    Integer     nDofs ;                 /* No. of Dofs        */
    Integer     nPs ;                   /* No. of P dimension */
    Integer     nQs ;                   /* No. of Q dimension */
    Real*       srcpnt ;                /* Source             */
    Real*       dstpnt ;                /* Destination        */

    nDofs       = 4 ;
    nPs         = 3 ;
    nQs         = 1 ;
    pOff        = 0 * usrHd->nNodes ;
    qOff        = 3 * usrHd->nNodes ;

    srcpnt      = usrPointer ( usrHd, srcId, srcOff, nSrcDims ) ;

    fMtxVdimVecMult( srcpnt,
                     usrHd->flowDiag+pOff,
                     usrHd->lesP,
                     &nSrcDims,
                     &nDofs,
                     &nPs,
                     &nPs,
                     &(usrHd->nNodes) ) ;

    commOut ( usrHd->lesP, usrHd->ilwork, &nPs,
	      usrHd->iper, usrHd->iBC, usrHd->BC  );

    fLesSparseApNGt( usrHd->colm, usrHd->rowp, 
		     usrHd->lhsP, usrHd->lesP, usrHd->lesQ, 
		     &(usrHd->nNodes),&(usrHd->nnz_tot));

    commIn ( usrHd->lesQ, usrHd->ilwork, &nQs,
	     usrHd->iper, usrHd->iBC, usrHd->BC  );
    
    dstpnt     = usrPointer ( usrHd, dstId, dstOff, nDstDims ) ;

    fMtxVdimVecMult( usrHd->lesQ,
		     usrHd->flowDiag+qOff,
		     dstpnt,
		     &nQs,
		     &nDofs,
		     &nDstDims,
		     &nQs,
		     &(usrHd->nNodes) ) ;
} 

/*-----------------------------------------------------------------------
 *
 * lesApNGtC
 *
 *    operation : -G^t(:,nenl,3*nenl) * Du(:,nenl,3) = lesQ(:,nenl,1)
 *                   C(:,nenl,  nenl) * Dp(:,nenl,1) = lesQ(:,nenl,1) 
 *
 *-----------------------------------------------------------------------
 */
void lesApNGtC ( UsrHd   usrHd,
                 Integer src1Id,
                 Integer src2Id,
                 Integer dstId,
                 Integer nSrc1Dims,
                 Integer src1Off,
                 Integer nSrc2Dims,
                 Integer src2Off,
                 Integer nDstDims,
                 Integer dstOff )
{
     char*       funcName = "lesApNGtC" ; /* function name      */
     Integer     p1Off ;                  /* Diag offset for P  */
     Integer     p2Off ;                  /* Diag offset for P  */
     Integer     qOff ;                   /* Diag offset for Q  */
     Integer     nDofs ;                  /* No. of Dofs        */
     Integer     nP1s ;                   /* No. of P dimension */
     Integer     nP2s ;                   /* No. of P dimension */
     Integer     nPs ;                    /* No. of P dimension */
     Integer     nQs ;                    /* No. of Q dimension */
     Real*       dstpnt ;                 /* Destination        */
     Real*       src1pnt ;                /* Source 1           */
     Real*       src2pnt ;                /* Source 2           */

     nDofs       = 4 ;
     nP1s        = 3 ;
     nP2s        = 1 ;
     nPs         = nP1s + nP2s ;
     nQs         = 1 ;
     p1Off       = 0 * usrHd->nNodes ;
     p2Off       = 3 * usrHd->nNodes ;
     qOff        = 3 * usrHd->nNodes ;

     src1pnt     = usrPointer ( usrHd, src1Id, src1Off, nSrc1Dims ) ;
     src2pnt     = usrPointer ( usrHd, src2Id, src2Off, nSrc2Dims ) ;

     fMtxVdimVecMult( src1pnt,
                      usrHd->flowDiag+p1Off,
                      usrHd->lesP+p1Off,
                      &nSrc1Dims,
                      &nDofs,
                      &nPs,
                      &nP1s,
                      &(usrHd->nNodes) ) ; 

     fMtxVdimVecMult( src2pnt,
                      usrHd->flowDiag+p2Off,
                      usrHd->lesP+p2Off,
                      &nSrc2Dims,
                      &nDofs,
                      &nPs,
                      &nP2s,
                      &(usrHd->nNodes) ) ;
     commOut ( usrHd->lesP, usrHd->ilwork, &nPs,
	       usrHd->iper, usrHd->iBC, usrHd->BC  );

     fLesSparseApNGtC( usrHd->colm, usrHd->rowp, 
		       usrHd->lhsP, usrHd->lesP, usrHd->lesQ, 
		       &(usrHd->nNodes),&(usrHd->nnz_tot));

     commIn ( usrHd->lesQ, usrHd->ilwork, &nQs,
	      usrHd->iper, usrHd->iBC, usrHd->BC  );

     dstpnt    = usrPointer ( usrHd, dstId, dstOff, nDstDims ) ;

     fMtxVdimVecMult( usrHd->lesQ,
                      usrHd->flowDiag+qOff,
                      dstpnt,
                      &nQs,
                      &nDofs,
                      &nDstDims,
                      &nQs,
                      &(usrHd->nNodes) ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesApFull
 *
 *    operation :    K * Du + G * Dp = lesQ(:,nenl,1:3)
 *                -G^t * Du + C * Dp = lesQ(:,nenl,4:4) 
 *
 *-----------------------------------------------------------------------
 */
void lesApFull ( UsrHd   usrHd,
                 Integer srcId,
                 Integer dstId,
                 Integer nSrcDims,
                 Integer srcOff,
                 Integer nDstDims,
                 Integer dstOff )
{    
     char*       funcName = "lesApFull" ; /* function name      */
     Integer     pOff ;                   /* Diag offset for P  */
     Integer     qOff ;                   /* Diag offset for Q  */
     Integer     nDofs ;                  /* No. of Dofs        */
     Integer     nPs ;                    /* No. of P dimension */
     Integer     nQs ;                    /* No. of Q dimension */
     Real*       srcpnt ;                 /* Source             */
     Real*       dstpnt ;                 /* Destination        */

     nDofs       = 4 ;
     nPs         = 4 ;
     nQs         = 4 ;
     pOff        = 0 * usrHd->nNodes ;
     qOff        = 0 * usrHd->nNodes ;

     srcpnt      = usrPointer ( usrHd, srcId, srcOff, nSrcDims ) ;
     
     fMtxVdimVecMult( srcpnt,
                      usrHd->flowDiag+pOff,
                      usrHd->lesP,
                      &nSrcDims,
                      &nDofs,
                      &nPs,
                      &nPs,
                      &(usrHd->nNodes) ) ;
     commOut ( usrHd->lesP, usrHd->ilwork, &nPs,
	       usrHd->iper, usrHd->iBC, usrHd->BC  );

     fLesSparseApFull( usrHd->colm, usrHd->rowp, usrHd->lhsK, 
		       usrHd->lhsP, usrHd->lesP, usrHd->lesQ, 
		       &(usrHd->nNodes),&(usrHd->nnz_tot));

     commIn ( usrHd->lesQ, usrHd->ilwork, &nQs,
	     usrHd->iper, usrHd->iBC, usrHd->BC  );

     dstpnt     = usrPointer ( usrHd, dstId, dstOff, nDstDims ) ;
    
     fMtxVdimVecMult( usrHd->lesQ,
		      usrHd->flowDiag+qOff,
		      dstpnt,
		      &nQs,
		      &nDofs,
		      &nDstDims,
		      &nQs,
		      &(usrHd->nNodes) ) ;
}

/*-----------------------------------------------------------------------
 *
 * lesApSclr
 *
 *    operation : M(:,nenl,nenl) * Ds(:,nenl,1)  = lesQ(:,nenl,1)
 *
 *-----------------------------------------------------------------------
 */
void lesApSclr ( UsrHd   usrHd,
                 Integer srcId,
                 Integer dstId,
                 Integer nSrcDims,
                 Integer srcOff,
                 Integer nDstDims,
                 Integer dstOff )
{
     char*       funcName = "lesApSclr" ; /* function name      */
     Integer     pOff ;                   /* Diag offset for P  */
     Integer     qOff ;                   /* Diag offset for Q  */
     Integer     nDofs ;                  /* No. of Dofs        */
     Integer     nPs ;                    /* No. of P dimension */
     Integer     nQs ;                    /* No. of Q dimension */
     Real*       srcpnt ;                 /* Source             */
     Real*       dstpnt ;                 /* Destination        */
     Integer     lhsStiffFlag ;
     double     sclrRegFct ;

     nDofs       = 1 ;
     nPs         = 1 ;
     nQs         = 1 ;
     pOff        = 0 ;
     qOff        = 0 ;

     lhsStiffFlag = 0 ;
     sclrRegFct   = 0 ;

     srcpnt      = usrPointer ( usrHd, srcId, srcOff, nSrcDims ) ;
     dstpnt      = usrPointer ( usrHd, dstId, dstOff, nDstDims ) ;


     fMtxVdimVecMult ( srcpnt,
                       usrHd->sclrDiag+pOff,
                       usrHd->lesP,
                       &nSrcDims,
                       &nDofs,
                       &nPs,
                       &nPs,
                       &(usrHd->nNodes) ) ;
     commOut ( usrHd->lesP, usrHd->ilwork, &nPs,
	       usrHd->iper, usrHd->iBC, usrHd->BC  );

     fLesSparseApSclr( usrHd->colm, usrHd->rowp, usrHd->lhsS, 
		       usrHd->lesP, usrHd->lesQ, 
		       &(usrHd->nNodes),&(usrHd->nnz_tot));

     commIn ( usrHd->lesQ, usrHd->ilwork, &nQs,
	      usrHd->iper, usrHd->iBC, usrHd->BC  );


     if ( lhsStiffFlag && sclrRegFct != 0 ) {

            fMtxVdimVecMult ( usrHd->lesQ,
                              usrHd->sclrDiag+qOff,
                              usrHd->lesP,
                              &nQs,
                              &nDofs,
                              &nDstDims,
                              &nQs,
                              &(usrHd->nNodes) ) ;

            flesDaxpy ( srcpnt,
                        usrHd->lesP,
                        &sclrRegFct,
                        &nDstDims,
                        &(usrHd->nNodes) ) ;

            flesCp ( usrHd->lesP,
                     dstpnt,
                     &nDstDims, 
                     &(usrHd->nNodes) ) ;

        } else {

            fMtxVdimVecMult ( usrHd->lesQ,
                              usrHd->sclrDiag+qOff,
                              dstpnt,
                              &nQs,
                              &nDofs,
                              &nDstDims,
                              &nQs,
                              &(usrHd->nNodes) ) ;
        }
    
}

#ifndef ACUSIM_LESLIB_VER_1_4

/*********************************************************************
 * lesPrecPPE
 *      outer routine to solve PPE
 *******************************************************************/

void lesPrecPPE(UsrHd usrHd,
        Integer srcId,
        Integer dstId,
        Integer nSrcDims,
        Integer srcOff,
        Integer nDstDims,
        Integer dstOff)
{
     Real*       srcpnt ;                 /* Source          R   */
     Real*       dstpnt ;                 /* Destination     Z   */
     srcpnt      = usrPointer ( usrHd, srcId, srcOff, nSrcDims ) ;
     dstpnt      = usrPointer ( usrHd, dstId, dstOff, nDstDims ) ;

     ramg_interface( usrHd->colm,
		     usrHd->rowp,usrHd->lhsK,usrHd->lhsP,usrHd->flowDiag,
		     srcpnt,dstpnt,
		     usrHd->ilwork,usrHd->BC,usrHd->iBC,usrHd->iper
		     );
     
     return;
}

#endif

