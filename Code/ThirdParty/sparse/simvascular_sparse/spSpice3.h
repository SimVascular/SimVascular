/*
 *  EXPORTS for sparse matrix routines with SPICE3.
 *
 *  Author:                     Advising professor:
 *      Kenneth S. Kundert          Alberto Sangiovanni-Vincentelli
 *      UC Berkeley
 *
 *  This file contains definitions that are useful to the calling
 *  program.  In particular, this file contains error keyword
 *  definitions, some macro functions that are used to quickly enter
 *  data into the matrix and the type definition of a data structure
 *  that acts as a template for entering admittances into the matrix.
 *  Also included is the type definitions for the various functions
 *  available to the user.
 *
 *  This file is a modified version of spMatrix.h that is used when
 *  interfacing to Spice3.
 */


/*
 *  Revision and copyright information.
 *
 *  Copyright (c) 1985-2003 by Kenneth S. Kundert
 *
 *  $Date: 2003/06/29 04:19:52 $
 *  $Revision: 1.2 $
 */




#ifndef  spOKAY

/*
 *  IMPORTS
 *
 *  >>> Import descriptions:
 *  spConfig.h
 *      Macros that customize the sparse matrix routines.
 */

#include "spConfig.h"






/*
 *  ERROR KEYWORDS
 *
 *  The actual numbers used in the error codes are not sacred, they can be
 *  changed under the condition that the codes for the nonfatal errors are
 *  less than the code for spFATAL and similarly the codes for the fatal
 *  errors are greater than that for spFATAL.
 *
 *  >>> Error descriptions:
 *  spOKAY
 *      No error has occurred.
 *  spSMALL_PIVOT
 *      When reordering the matrix, no element was found which satisfies the
 *      threshold criteria.  The largest element in the matrix was chosen
 *      as pivot.  Non-fatal.
 *  spZERO_DIAG
 *      Fatal error.  A zero was encountered on the diagonal the matrix.  This
 *      does not necessarily imply that the matrix is singular.  When this
 *      error occurs, the matrix should be reconstructed and factored using
 *      spOrderAndFactor().  In spCOMPATIBILITY mode, spZERO_DIAG is
 *      indistinguishable from spSINGULAR.
 *  spSINGULAR
 *      Fatal error.  Matrix is singular, so no unique solution exists.
 *  spNO_MEMORY
 *      Fatal error.  Indicates that not enough memory is available to handle
 *      the matrix.
 *  spPANIC
 *      Fatal error indicating that the routines are not prepared to
 *      handle the matrix that has been requested.  This may occur when
 *      the matrix is specified to be real and the routines are not
 *      compiled for real matrices, or when the matrix is specified to
 *      be complex and the routines are not compiled to handle complex
 *      matrices.
 *  spFATAL
 *      Not an error flag, but rather the dividing line between fatal errors
 *      and warnings.
 */

#include "../include/SPerror.h"  /* Spice error definitions. */

/* Begin error macros. */
#define  spOKAY                 OK
#define  spSMALL_PIVOT          OK
#define  spZERO_DIAG            E_SINGULAR
#define  spSINGULAR             E_SINGULAR
#define  spNO_MEMORY            E_NOMEM
#define  spPANIC                E_BADMATRIX

#define  spFATAL                E_BADMATRIX


#if spCOMPATIBILITY
#define  NO_ERROR               spOKAY
#define  UNDER_FLOW             spOKAY
#define  OVER_FLOW              spOKAY
#define  ILL_CONDITIONED        spSMALL_PIVOT
#define  SINGULAR               spSINGULAR
#define  NO_MEMORY              spNO_MEMORY
#define  RANGE                  spPANIC

#define  FATAL                  spFATAL

#undef   spZERO_DIAG
#define  spZERO_DIAG            spSINGULAR
#endif /* spCOMPATIBILITY */





/*
 *  KEYWORD DEFINITIONS
 *
 *  Here we define what precision arithmetic Sparse will use.  Double
 *  precision is suggested as being most appropriate for circuit
 *  simulation and for C.  However, it is possible to change spREAL
 *  to a float for single precision arithmetic.  Note that in C, single
 *  precision arithmetic is often slower than double precision.  Sparse
 *  internally refers to spREALs as RealNumbers.
 *
 *  Some C compilers, notably the old VMS compiler, do not handle the keyword
 *  "void" correctly.  If this is true for your compiler, remove the
 *  comment delimiters from the redefinition of void to int below.
 */

#define  spREAL double
/* #define  void    int   */

#if spCOMPATIBILITY
#define SPARSE_REAL     spREAL
#endif



/*
 *  PARTITION TYPES
 *
 *  When factoring a previously ordered matrix using spFactor(), Sparse
 *  operates on a row-at-a-time basis.  For speed, on each step, the row
 *  being updated is copied into a full vector and the operations are
 *  performed on that vector.  This can be done one of two ways, either
 *  using direct addressing or indirect addressing.  Direct addressing
 *  is fastest when the matrix is relatively dense and indirect addressing
 *  is quite sparse.  The user can select which partitioning mode is used.
 *  The following keywords are passed to spPartition() and indicate that
 *  Sparse should use only direct addressing, only indirect addressing, or
 *  that it should choose the best mode on a row-by-row basis.  The time
 *  required to choose a partition is of the same order of the cost to factor
 *  the matrix.
 *
 *  If you plan to factor a large number of matrices with the same structure,
 *  it is best to let Sparse choose the partition.  Otherwise, you should
 *  choose the partition based on the predicted density of the matrix.
 */

/* Begin partition keywords. */

#define spDEFAULT_PARTITION     0
#define spDIRECT_PARTITION      1
#define spINDIRECT_PARTITION    2
#define spAUTO_PARTITION        3





/*
 *  MACRO FUNCTION DEFINITIONS
 *
 *  >>> Macro descriptions:
 *  spADD_REAL_ELEMENT
 *      Macro function that adds data to a real element in the matrix by a
 *      pointer.
 *  spADD_IMAG_ELEMENT
 *      Macro function that adds data to a imaginary element in the matrix by
 *      a pointer.
 *  spADD_COMPLEX_ELEMENT
 *      Macro function that adds data to a complex element in the matrix by a
 *      pointer.
 *  spADD_REAL_QUAD
 *      Macro function that adds data to each of the four real matrix elements
 *      specified by the given template.
 *  spADD_IMAG_QUAD
 *      Macro function that adds data to each of the four imaginary matrix
 *      elements specified by the given template.
 *  spADD_COMPLEX_QUAD
 *      Macro function that adds data to each of the four complex matrix
 *      elements specified by the given template.
 */

/* Begin Macros. */
#define  spADD_REAL_ELEMENT(element,real)       *(element) += real

#define  spADD_IMAG_ELEMENT(element,imag)       *(element+1) += imag

#define  spADD_COMPLEX_ELEMENT(element,real,imag)       \
{   *(element) += real;                                 \
    *(element+1) += imag;                               \
}

#define  spADD_REAL_QUAD(template,real)         \
{   *((template).Element1) += real;             \
    *((template).Element2) += real;             \
    *((template).Element3Negated) -= real;      \
    *((template).Element4Negated) -= real;      \
}

#define  spADD_IMAG_QUAD(template,imag)         \
{   *((template).Element1+1) += imag;           \
    *((template).Element2+1) += imag;           \
    *((template).Element3Negated+1) -= imag;    \
    *((template).Element4Negated+1) -= imag;    \
}

#define  spADD_COMPLEX_QUAD(template,real,imag) \
{   *((template).Element1) += real;             \
    *((template).Element2) += real;             \
    *((template).Element3Negated) -= real;      \
    *((template).Element4Negated) -= real;      \
    *((template).Element1+1) += imag;           \
    *((template).Element2+1) += imag;           \
    *((template).Element3Negated+1) -= imag;    \
    *((template).Element4Negated+1) -= imag;    \
}

#if spCOMPATIBILITY
#define  ADD_REAL_ELEMENT_TO_MATRIX             spADD_REAL_ELEMENT
#define  ADD_IMAG_ELEMENT_TO_MATRIX             spADD_IMAG_ELEMENT
#define  ADD_COMPLEX_ELEMENT_TO_MATRIX          spADD_COMPLEX_ELEMENT
#define  ADD_REAL_QUAD_ELEMENT_TO_MATRIX        spADD_REAL_ELEMENT
#define  ADD_IMAG_QUAD_ELEMENT_TO_MATRIX        spADD_IMAG_ELEMENT
#define  ADD_COMPLEX_QUAD_ELEMENT_TO_MATRIX     spADD_COMPLEX_ELEMENT
#endif






/*
 *   TYPE DEFINITION FOR COMPONENT TEMPLATE
 *
 *   This data structure is used to hold pointers to four related elements in
 *   matrix.  It is used in conjunction with the routines
 *       spGetAdmittance
 *       spGetQuad
 *       spGetOnes
 *   These routines stuff the structure which is later used by the spADD_QUAD
 *   macro functions above.  It is also possible for the user to collect four
 *   pointers returned by spGetElement and stuff them into the template.
 *   The spADD_QUAD routines stuff data into the matrix in locations specified
 *   by Element1 and Element2 without changing the data.  The data is negated
 *   before being placed in Element3 and Element4.
 */

#if spCOMPATIBILITY
#define spTemplate TemplateStruct
#endif

/* Begin `spTemplate'. */
struct  spTemplate
{   spREAL    *Element1       ;
    spREAL    *Element2       ;
    spREAL    *Element3Negated;
    spREAL    *Element4Negated;
};





/*
 *   FUNCTION TYPE DEFINITIONS
 *
 *   The type of every user accessible function is declared here.
 */

/* Begin function declarations. */

spcEXTERN  void     spClear( char* );
spcEXTERN  spREAL   spCondition( char*, spREAL, int* );
spcEXTERN  char    *spCreate( int, int, int* );
spcEXTERN  void     spDeleteRowAndCol( char*, int, int );
spcEXTERN  void     spDestroy( char* );
spcEXTERN  int      spElementCount( char* );
spcEXTERN  int      spError( char* );
spcEXTERN  int      spFactor( char* );
spcEXTERN  int      spFileMatrix( char*, char*, char*, int, int, int );
spcEXTERN  int      spFileStats( char*, char*, char* );
spcEXTERN  int      spFillinCount( char* );
spcEXTERN  int      spGetAdmittance( char*, int, int,
				struct spTemplate* );
spcEXTERN  spREAL  *spGetElement( char*, int, int );
spcEXTERN  char    *spGetInitInfo( spREAL* );
spcEXTERN  int      spGetOnes( char*, int, int, int,
				struct spTemplate* );
spcEXTERN  int      spGetQuad( char*, int, int, int, int,
				struct spTemplate* );
spcEXTERN  int      spGetSize( char*, int );
spcEXTERN  int      spInitialize( char*, int (*)() );
spcEXTERN  void     spInstallInitInfo( spREAL*, char* );
spcEXTERN  spREAL   spLargestElement( char* );
spcEXTERN  void     spMNA_Preorder( char* );
spcEXTERN  spREAL   spNorm( char* );
spcEXTERN  int      spOrderAndFactor( char*, spREAL[], spREAL,
				spREAL, int );
spcEXTERN  void     spPartition( char*, int );
spcEXTERN  void     spPrint( char*, int, int, int );
spcEXTERN  spREAL   spPseudoCondition( char* );
spcEXTERN  spREAL   spRoundoff( char*, spREAL );
spcEXTERN  void     spScale( char*, spREAL[], spREAL[] );
spcEXTERN  void     spSetComplex( char* );
spcEXTERN  void     spSetReal( char* );
spcEXTERN  void     spStripFills( char* );
spcEXTERN  void     spWhereSingular( char*, int*, int* );

/* Functions with argument lists that are dependent on options. */

#if spCOMPLEX
spcEXTERN  void     spDeterminant( char*, int*, spREAL*, spREAL* );
#else /* NOT spCOMPLEX */
spcEXTERN  void     spDeterminant( char*, int*, spREAL* );
#endif /* NOT spCOMPLEX */
#if spCOMPLEX && spSEPARATED_COMPLEX_VECTORS
spcEXTERN  int      spFileVector( char*, char* , spREAL[], spREAL[]);
spcEXTERN  void     spMultiply( char*, spREAL[], spREAL[], spREAL[],
				spREAL[] );
spcEXTERN  void     spMultTransposed( char*, spREAL[], spREAL[],
				spREAL[], spREAL[] );
spcEXTERN  void     spSolve( char*, spREAL[], spREAL[], spREAL[],
				spREAL[] );
spcEXTERN  void     spSolveTransposed( char*, spREAL[], spREAL[],
				spREAL[], spREAL[] );
#else /* NOT  (spCOMPLEX && spSEPARATED_COMPLEX_VECTORS) */
spcEXTERN  int      spFileVector( char*, char* , spREAL[] );
spcEXTERN  void     spMultiply( char*, spREAL[], spREAL[] );
spcEXTERN  void     spMultTransposed( char*, spREAL[], spREAL[] );
spcEXTERN  void     spSolve( char*, spREAL[], spREAL[] );
spcEXTERN  void     spSolveTransposed( char*, spREAL[], spREAL[] );
#endif /* NOT  (spCOMPLEX && spSEPARATED_COMPLEX_VECTORS) */
#endif  /* spOKAY */
