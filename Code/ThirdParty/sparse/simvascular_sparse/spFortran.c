/*
 *  SPARSE FORTRAN MODULE
 *
 *  Author:			Advising professor:
 *     Kenneth S. Kundert	    Alberto Sangiovanni-Vincentelli
 *     UC Berkeley
 */
/*! \file
*
 *  This module contains routines that interface Sparse1.4 to a calling
 *  program written in fortran.  Almost every externally available Sparse1.4
 *  routine has a counterpart defined in this file, with the name the
 *  same except the \a sp prefix is changed to \a sf.  The \a spADD_ELEMENT
 *  and \a spADD_QUAD macros are also replaced with the \a sfAdd1 and \a sfAdd4
 *  functions defined in this file.
 *
 *  To ease porting this file to different operating systems, the names of
 *  the functions can be easily redefined (search for `Routine Renaming').
 *  A simple example of a FORTRAN program that calls Sparse is included in
 *  this file (search for Example).  When interfacing to a FORTRAN program,
 *  the ARRAY_OFFSET option should be set to NO (see spConfig.h).
 *
 *  DISCLAIMER:
 *  These interface routines were written by a C programmer who has little
 *  experience with FORTRAN.  The routines have had minimal testing.
 *  Any interface between two languages is going to have portability
 *  problems, this one is no exception.
 */
/*  >>> User accessible functions contained in this file:
 *  sfCreate()
 *  sfDestroy()
 *  sfStripFills()
 *  sfClear()
 *  sfGetElement()
 *  sfGetAdmittance()
 *  sfGetQuad()
 *  sfGetOnes()
 *  sfAdd1Real()
 *  sfAdd1Imag()
 *  sfAdd1Complex()
 *  sfAdd4Real()
 *  sfAdd4Imag()
 *  sfAdd4Complex()
 *  sfOrderAndFactor()
 *  sfFactor()
 *  sfPartition()
 *  sfSolve()
 *  sfSolveTransposed()
 *  sfPrint()
 *  sfFileMatrix()
 *  sfFileVector()
 *  sfFileStats()
 *  sfMNA_Preorder()
 *  sfScale()
 *  sfMultiply()
 *  sfMultTransposed()
 *  sfDeterminant()
 *  sfError()
 *  sfErrorMessage()
 *  sfWhereSingular()
 *  sfGetSize()
 *  sfSetReal()
 *  sfSetComplex()
 *  sfFillinCount()
 *  sfElementCount()
 *  sfDeleteRowAndCol()
 *  sfPseudoCondition()
 *  sfCondition()
 *  sfNorm()
 *  sfLargestElement()
 *  sfRoundoff()
 */

/*
 *  FORTRAN -- C COMPATIBILITY
 *
 *  Fortran and C data types correspond in the following way:
 *  -- C --	-- FORTRAN --
 *  int		INTEGER*4 or INTEGER*2 (machine dependent, usually int*4)
 *  long	INTEGER*4
 *  float	REAL
 *  double	DOUBLE PRECISION (used by default in preference to float)
 *  
 *  The complex number format used by Sparse is compatible with that
 *  used by FORTRAN.  C pointers are passed to FORTRAN as longs, they should
 *  not be used in any way in FORTRAN.
 */
  


/*
 *  Revision and copyright information.
 *
 *  Copyright (c) 1985-2003 by Kenneth S. Kundert
 */

#ifndef lint
static char copyright[] =
    "Sparse1.4: Copyright (c) 1985-2003 by Kenneth S. Kundert";
static char RCSid[] =
    "@(#)$Header: /cvsroot/sparse/src/spFortran.c,v 1.3 2003/06/30 19:40:51 kundert Exp $";
#endif




/*
 *  IMPORTS
 *
 *  >>> Import descriptions:
 *  spConfig.h
 *     Macros that customize the sparse matrix routines.
 *  spMatrix.h
 *     Macros and declarations to be imported by the user.
 *  spDefs.h
 *     Matrix type and macro definitions for the sparse matrix routines.
 */

#define spINSIDE_SPARSE
#include <stdio.h>
#include "spConfig.h"
#include "spMatrix.h"
#include "spDefs.h"

#if FORTRAN




/*
 *  Routine Renaming
 */

#define sfCreate		sfcreate
#define sfStripFills		sfstripfills
#define sfDestroy		sfdestroy
#define sfClear			sfclear
#define sfGetElement		sfgetelement
#define sfGetAdmittance		sfgetadmittance
#define sfGetQuad		sfgetquad
#define sfGetOnes		sfgetones
#define sfAdd1Real		sfadd1real
#define sfAdd1Imag		sfadd1imag
#define sfAdd1Complex		sfadd1complex
#define sfAdd4Real		sfadd4real
#define sfAdd4Imag		sfadd4imag
#define sfAdd4Complex		sfadd4complex
#define sfOrderAndFactor	sforderandfactor
#define sfFactor		sffactor
#define sfPartition		sfpartition
#define sfSolve			sfsolve
#define sfSolveTransposed	sfsolvetransposed
#define sfPrint			sfprint
#define sfFileMatrix		sffilematrix
#define sfFileVector		sffilevector
#define sfFileStats		sffilestats
#define sfMNA_Preorder		sfmna_preorder
#define sfScale			sfscale
#define sfMultiply		sfmultiply
#define sfMultTransposed	sfmulttransposed
#define sfDeterminant		sfdeterminant
#define sfError			sferror
#define sfErrorMessage		sferrormessage
#define sfWhereSingular		sfwheresingular
#define sfGetSize		sfgetsize
#define sfSetReal		sfsetreal
#define sfSetComplex		sfsetcomplex
#define sfFillinCount		sffillincount
#define sfElementCount		sfelementcount
#define sfDeleteRowAndCol	sfdeleterowandcol
#define sfPseudoCondition	sfpseudocondition
#define sfCondition		sfcondition
#define sfNorm			sfnorm
#define sfLargestElement	sflargestelement
#define sfRoundoff		sfroundoff


#define MATRIX_FILE_NAME	"spMatrix"
#define STATS_FILE_NAME		"spStats"

/*
 *
 *  Example of a FORTRAN Program Calling Sparse
 *

      integer matrix, error, sfCreate, sfGetElement, spFactor
      integer element(10)
      double precision rhs(4), solution(4)
c
      matrix = sfCreate(4,0,error)
      element(1) = sfGetElement(matrix,1,1)
      element(2) = sfGetElement(matrix,1,2)
      element(3) = sfGetElement(matrix,2,1)
      element(4) = sfGetElement(matrix,2,2)
      element(5) = sfGetElement(matrix,2,3)
      element(6) = sfGetElement(matrix,3,2)
      element(7) = sfGetElement(matrix,3,3)
      element(8) = sfGetElement(matrix,3,4)
      element(9) = sfGetElement(matrix,4,3)
      element(10) = sfGetElement(matrix,4,4)
      call sfClear(matrix)
      call sfAdd1Real(element(1), 2d0)
      call sfAdd1Real(element(2), -1d0)
      call sfAdd1Real(element(3), -1d0)
      call sfAdd1Real(element(4), 3d0)
      call sfAdd1Real(element(5), -1d0)
      call sfAdd1Real(element(6), -1d0)
      call sfAdd1Real(element(7), 3d0)
      call sfAdd1Real(element(8), -1d0)
      call sfAdd1Real(element(9), -1d0)
      call sfAdd1Real(element(10), 3d0)
      call sfprint(matrix, .false., .false.)
      rhs(1) = 34d0
      rhs(2) = 0d0
      rhs(3) = 0d0
      rhs(4) = 0d0
      error = sfFactor(matrix)
      call sfSolve(matrix, rhs, solution)
      write (6, 10) rhs(1), rhs(2), rhs(3), rhs(4)
   10 format (f 10.2)
      end

 *
 */






/*  MATRIX ALLOCATION */
/*!
 *  Allocates and initializes the data structures associated with a matrix.
 *
 *  \return [INTEGER]
 *  A pointer to the matrix is returned cast into an integer.  This pointer
 *  is then passed and used by the other matrix routines to refer to a
 *  particular matrix.  If an error occurs, the NULL pointer is returned.
 *
 *  \param Size [INTEGER]
 *	Size of matrix or estimate of size of matrix if matrix is \a EXPANDABLE.
 *  \param Complex  [INTEGER or INTEGER*2]
 *	Type of matrix.  If \a Complex is 0 then the matrix is real, otherwise
 *	the matrix will be complex.
 *	Note that if a matrix will be both real and complex, it must
 *	be specified here as being complex.
 *  \param Error  [INTEGER or INTEGER*2]
 *	Returns error flag, needed because function spError() will not work
 *	correctly if spCreate() returns NULL. Possible errors include \a spNO_MEMORY.
 */

long
sfCreate( int *Size, int *Complex, int *Error )
{
/* Begin `sfCreate'. */
    return (long)spCreate(*Size, *Complex, Error );
}






/*  MATRIX DEALLOCATION */
/*!
 *  Deallocates pointers and elements of matrix.
 *
 *  \param Matrix [INTEGER]
 *	Pointer to the matrix frame which is to be removed from memory.
 */

void
sfDestroy( long *Matrix )
{
/* Begin `sfDestroy'. */
    spDestroy((spMatrix)*Matrix);
    return;
}






#if STRIP

/*  STRIP FILL-INS FROM MATRIX */
/*!
 *  Strips the matrix of all fill-ins.
 *
 *  \param Matrix [INTEGER]
 *	Pointer to the matrix to be stripped.
 */

void
sfStripFills( long *Matrix )
{
/* Begin `sfStripFills'. */
    spStripFills((spMatrix)*Matrix);
    return;
}
#endif







/*  CLEAR MATRIX */
/*!
 *  Sets every element of the matrix to zero and clears the error flag.
 *
 *  \param Matrix [INTEGER]
 *     Pointer to matrix that is to be cleared.
 */

void
sfClear( long *Matrix )
{
/* Begin `sfClear'. */
    spClear((spMatrix)*Matrix);
    return;
}






/*  SINGLE ELEMENT ADDITION TO MATRIX BY INDEX */
/*!
 *  Finds element [Row,Col] and returns a pointer to it.  If element is
 *  not found then it is created and spliced into matrix.  This routine
 *  is only to be used after spCreate() and before spMNA_Preorder(),
 *  spFactor() or spOrderAndFactor().  Returns a pointer to the
 *  Real portion of a matrix element.  This pointer is later used by
 *  sfAddxxxxx() to directly access element.
 *
 *  \return [INTEGER]
 *  Returns a pointer to the element.  This pointer is then used to directly
 *  access the element during successive builds. Returns NULL if \a spNO_MEMORY
 *  error occurs. Error is not cleared in this routine.
 *
 *  \param Matrix [INTEGER]
 *     Pointer to the matrix that the element is to be added to.
 *  \param Row [INTEGER or INTEGER*2]
 *     Row index for element.  Must be in the range of [0..Size] unless
 *     the options \a EXPANDABLE or \a TRANSLATE are used. Elements placed in
 *     row zero are discarded.  In no case may \a Row be less than zero.
 *  \param Col [INTEGER or INTEGER*2]
 *     Column index for element.  Must be in the range of [0..Size] unless
 *     the options \a EXPANDABLE or \a TRANSLATE are used. Elements placed in
 *     column zero are discarded.  In no case may \a Col be less than zero.
 */

long
sfGetElement( long *Matrix, int *Row, int *Col )
{
/* Begin `sfGetElement'. */
    return (long)spGetElement((spMatrix)*Matrix, *Row, *Col);
}







#if QUAD_ELEMENT
/*  ADDITION OF ADMITTANCE TO MATRIX BY INDEX */
/*!
 *  Performs same function as sfGetElement() except rather than one
 *  element, all four Matrix elements for a floating component are
 *  added.  This routine also works if component is grounded.  Positive
 *  elements are placed at [Node1,Node2] and [Node2,Node1].  This
 *  routine is only to be used after sfCreate() and before
 *  sfMNA_Preorder(), sfFactor() or sfOrderAndFactor().
 *
 *  \return [INTEGER or INTEGER*2]
 *  The error code. Possible errors include \a spNO_MEMORY.
 *  Error is not cleared in this routine.
 *
 *  \param Matrix [INTEGER]
 *     Pointer to the matrix that component is to be entered in.
 *  \param Node1 [INTEGER or INTEGER*2]
 *     Row and column indices for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Node zero is the
 *     ground node.  In no case may \a Node1 be less than zero.
 *  \param Node2 [INTEGER or INTEGER*2]
 *     Row and column indices for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Node zero is the
 *     ground node.  In no case may \a Node2 be less than zero.
 *  \param Template [INTEGER (4)]
 *     Collection of pointers to four elements that are later used to directly
 *     address elements.  User must supply the template, this routine will
 *     fill it.
 */

int
sfGetAdmittance( long *Matrix, int *Node1, int *Node2, long Template[4] )
{
/* Begin `spGetAdmittance'. */
    return
    (   spGetAdmittance((spMatrix)*Matrix, *Node1, *Node2,
			(struct spTemplate *)Template )
    );
}
#endif /* QUAD_ELEMENT */









#if QUAD_ELEMENT
/*  ADDITION OF FOUR ELEMENTS TO MATRIX BY INDEX */
/*!
 *  Similar to sfGetAdmittance(), except that sfGetAdmittance() only
 *  handles 2-terminal components, whereas sfGetQuad() handles simple
 *  4-terminals as well.  These 4-terminals are simply generalized
 *  2-terminals with the option of having the sense terminals different
 *  from the source and sink terminals.  sfGetQuad() adds four
 *  elements to the matrix.  Positive elements occur at Row1,Col1
 *  Row2,Col2 while negative elements occur at Row1,Col2 and Row2,Col1.
 *  The routine works fine if any of the rows and columns are zero.
 *  This routine is only to be used after sfCreate() and before
 *  sfMNA_Preorder(), sfFactor() or sfOrderAndFactor()
 *  unless TRANSLATE is set true.
 *
 *  \return [INTEGER or INTEGER*2]
 *  Error code. Possible errors include \a spNO_MEMORY.
 *  Error is not cleared in this routine.
 *
 *  \param Matrix [INTEGER]
 *     Pointer to the matrix that component is to be entered in.
 *  \param Row1 [INTEGER or INTEGER*2]
 *     First row index for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground row.  In no case may \a Row1 be less than zero.
 *  \param Row2 [INTEGER or INTEGER*2]
 *     Second row index for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground row.  In no case may \a Row2 be less than zero.
 *  \param Col1 [INTEGER or INTEGER*2]
 *     First column index for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground column.  In no case may \a Col1 be less than zero.
 *  \param Col2 [INTEGER or INTEGER*2]
 *     Second column index for elements. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground column.  In no case may \a Col2 be less than zero.
 *  \param Template [INTEGER (4)]
 *     Collection of pointers to four elements that are later used to directly
 *     address elements.  User must supply the template, this routine will
 *     fill it.
 */

int
sfGetQuad( long *Matrix, int *Row1, int *Row2, int *Col1, int *Col2, long Template[4] )
{
/* Begin `spGetQuad'. */
    return
    (   spGetQuad( (spMatrix)*Matrix, *Row1, *Row2, *Col1, *Col2,
		   (struct spTemplate *)Template )
    );
}
#endif /* QUAD_ELEMENT */









#if QUAD_ELEMENT
/*  ADDITION OF FOUR STRUCTURAL ONES TO MATRIX BY INDEX */
/*!
 *  Performs similar function to sfGetQuad() except this routine is
 *  meant for components that do not have an admittance representation.
 *
 *  The following stamp is used: \code
 *         Pos  Neg  Eqn
 *  Pos  [  .    .    1  ]
 *  Neg  [  .    .   -1  ]
 *  Eqn  [  1   -1    .  ]
 *  \endcode
 *
 *  \return [INTEGER or INTEGER*2]
 *  Error code. Possible errors include \a spNO_MEMORY.
 *  Error is not cleared in this routine.
 *
 *  \param Matrix [INTEGER]
 *     Pointer to the matrix that component is to be entered in.
 *  \param Pos [INTEGER or INTEGER*2]
 *     See stamp above. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground row.  In no case may \a Pos be less than zero.
 *  \param Neg [INTEGER or INTEGER*2]
 *     See stamp above. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground row.  In no case may \a Neg be less than zero.
 *  \param Eqn [INTEGER or INTEGER*2]
 *     See stamp above. Must be in the range of [0..Size]
 *     unless the options \a EXPANDABLE or \a TRANSLATE are used. Zero is the
 *     ground row.  In no case may \a Eqn be less than zero.
 *  \param Template [4]) [INTEGER (4)]
 *     Collection of pointers to four elements that are later used to directly
 *     address elements.  User must supply the template, this routine will
 *     fill it.
 */

int
sfGetOnes( long *Matrix, int *Pos, int *Neg, int *Eqn, long Template[4] )
{
/* Begin `sfGetOnes'. */
    return
    (   spGetOnes( (spMatrix)*Matrix, *Pos, *Neg, *Eqn,
		   (struct spTemplate *)Template )
    );
}
#endif /* QUAD_ELEMENT */







/*  ADD ELEMENT(S) DIRECTLY TO MATRIX */
/*!
 *  Adds a real value to a matrix element.
 *  These elements are referenced by pointer, and so must already have
 *  been created by spGetElement().
 *
 *  \param Element [INTEGER]
 *	Pointer to the element that is to be added to.
 *  \param Real [REAL or DOUBLE PRECISION]
 *	Real portion of the number to be added to the element.
 */

void
sfAdd1Real( long *Element, spREAL *Real )
{
/* Begin `sfAdd1Real'. */
    *((RealNumber *)*Element) += *Real;
}


#if spCOMPLEX
/*!
 *  Adds an imaginary value to a matrix element.
 *  These elements are referenced by pointer, and so must already have
 *  been created by spGetElement().
 *
 *  \param Element [INTEGER]
 *	Pointer to the element that is to be added to.
 *  \param Imag [REAL or DOUBLE PRECISION]
 *	Imaginary portion of the number to be added to the element.
 */

void
sfAdd1Imag( long *Element, spREAL *Imag )
{
/* Begin `sfAdd1Imag'. */
    *(((RealNumber *)*Element)+1) += *Imag;
}


/*!
 *  Adds a complex value to a matrix element.
 *  These elements are referenced by pointer, and so must already have
 *  been created by spGetElement().
 *
 *  \param Element [INTEGER]
 *	Pointer to the element that is to be added to.
 *  \param Real [REAL or DOUBLE PRECISION]
 *	Real portion of the number to be added to the element.
 *  \param Imag [REAL or DOUBLE PRECISION]
 *	Imaginary portion of the number to be added to the element.
 */
void
sfAdd1Complex( long *Element, spREAL *Real, spREAL *Imag )
{
/* Begin `sfAdd1Complex'. */
    *((RealNumber *)*Element) += *Real;
    *(((RealNumber *)*Element)+1) += *Imag;
}
#endif /* spCOMPLEX */


#if QUAD_ELEMENT
/*!
 *  Adds a real value to a set of four elements in a matrix.
 *  These elements are referenced by pointer, and so must already have
 *  been created by spGetAdmittance(), spGetQuad(), or spGetOnes().
 *
 *  \param Template [4]) [INTEGER (4)]
 *	Pointer to the element that is to be added to.
 *  \param Real [REAL or DOUBLE PRECISION]
 *	Real portion of the number to be added to the element.
 */

void
sfAdd4Real( long Template[4], spREAL *Real )
{
/* Begin `sfAdd4Real'. */
    *((RealNumber *)Template[0]) += *Real;
    *((RealNumber *)Template[1]) += *Real;
    *((RealNumber *)Template[2]) -= *Real;
    *((RealNumber *)Template[3]) -= *Real;
}


#if spCOMPLEX
/*!
 *  Adds an imaginary value to a set of four elements in a matrix.
 *  These elements are referenced by pointer, and so must already have
 *  been created by spGetAdmittance(), spGetQuad(), or spGetOnes().
 *
 *  \param Template [4]) [INTEGER (4)]
 *	Pointer to the element that is to be added to.
 *  \param Imag [REAL or DOUBLE PRECISION]
 *	Imaginary portion of the number to be added to the element.
 */

void
sfAdd4Imag( long Template[4], spREAL *Imag )
{
/* Begin `sfAdd4Imag'. */
    *(((RealNumber *)Template[0])+1) += *Imag;
    *(((RealNumber *)Template[1])+1) += *Imag;
    *(((RealNumber *)Template[2])+1) -= *Imag;
    *(((RealNumber *)Template[3])+1) -= *Imag;
}


/*!
 *  Adds a complex value to a set of four elements in a matrix.
 *  These elements are referenced by pointer, and so must already have
 *  been created by spGetAdmittance(), spGetQuad(), or spGetOnes().
 *
 *  \param Template [4]) [INTEGER (4)]
 *	Pointer to the element that is to be added to.
 *  \param Real [REAL or DOUBLE PRECISION]
 *	Real portion of the number to be added to the element.
 *  \param Imag [REAL or DOUBLE PRECISION]
 *	Imaginary portion of the number to be added to the element.
 */

void
sfAdd4Complex( long Template[4], spREAL *Real, spREAL *Imag )
{
/* Begin `sfAdd4Complex'. */
    *((RealNumber *)Template[0]) += *Real;
    *((RealNumber *)Template[1]) += *Real;
    *((RealNumber *)Template[2]) -= *Real;
    *((RealNumber *)Template[3]) -= *Real;
    *(((RealNumber *)Template[0])+1) += *Imag;
    *(((RealNumber *)Template[1])+1) += *Imag;
    *(((RealNumber *)Template[2])+1) -= *Imag;
    *(((RealNumber *)Template[3])+1) -= *Imag;
}
#endif /* spCOMPLEX */
#endif /* QUAD_ELEMENT */






/*  ORDER AND FACTOR MATRIX */
/*!
 *  This routine chooses a pivot order for the matrix and factors it
 *  into LU form.  It handles both the initial factorization and subsequent
 *  factorizations when a reordering is desired.  This is handled in a manner
 *  that is transparent to the user.  The routine uses a variation of
 *  Gauss's method where the pivots are associated with L and the
 *  diagonal terms of U are one.
 *
 *  \return [INTEGER of INTEGER*2]
 *  The error code is returned.  Possible errors include \a spNO_MEMORY,
 *  \a spSINGULAR, and \a spSMALL_PIVOT.
 *  Error is cleared in this function.
 *
 *  \return Matrix [INTEGER]
 *	Pointer to matrix.
 *  \return RHS [REAL (1) or DOUBLE PRECISION (1)]
 *	Representative right-hand side vector that is used to determine
 *	pivoting order when the right hand side vector is sparse.  If
 *	\a RHS is a NULL pointer then the RHS vector is assumed to
 *	be full and it is not used when determining the pivoting
 *	order.
 *  \return RelThreshold [REAL or DOUBLE PRECISION]
 *      This number determines what the pivot relative threshold will
 *      be.  It should be between zero and one.  If it is one then the
 *      pivoting method becomes complete pivoting, which is very slow
 *      and tends to fill up the matrix.  If it is set close to zero
 *      the pivoting method becomes strict Markowitz with no
 *      threshold.  The pivot threshold is used to eliminate pivot
 *      candidates that would cause excessive element growth if they
 *      were used.  Element growth is the cause of roundoff error.
 *      Element growth occurs even in well-conditioned matrices.
 *      Setting the \a RelThreshold large will reduce element growth and
 *      roundoff error, but setting it too large will cause execution
 *      time to be excessive and will result in a large number of
 *      fill-ins.  If this occurs, accuracy can actually be degraded
 *      because of the large number of operations required on the
 *      matrix due to the large number of fill-ins.  A good value seems
 *      to be 0.001.  The default is chosen by giving a value larger
 *      than one or less than or equal to zero.  This value should be
 *      increased and the matrix resolved if growth is found to be
 *      excessive.  Changing the pivot threshold does not improve
 *      performance on matrices where growth is low, as is often the
 *      case with ill-conditioned matrices.  Once a valid threshold is
 *      given, it becomes the new default.  The default value of
 *      RelThreshold was chosen for use with nearly diagonally
 *      dominant matrices such as node- and modified-node admittance
 *      matrices.  For these matrices it is usually best to use
 *      diagonal pivoting.  For matrices without a strong diagonal, it
 *      is usually best to use a larger threshold, such as 0.01 or
 *      0.1.
 *  \return AbsThreshold [REAL or DOUBLE PRECISION]
 *	The absolute magnitude an element must have to be considered
 *	as a pivot candidate, except as a last resort.  This number
 *	should be set significantly smaller than the smallest diagonal
 *	element that is is expected to be placed in the matrix.  If
 *	there is no reasonable prediction for the lower bound on these
 *	elements, then \a AbsThreshold should be set to zero.
 *	\a AbsThreshold is used to reduce the possibility of choosing as a
 *	pivot an element that has suffered heavy cancellation and as a
 *	result mainly consists of roundoff error.  Once a valid
 *	threshold is given, it becomes the new default.
 *  \return DiagPivoting [LOGICAL]
 *	A flag indicating that pivot selection should be confined to the
 *	diagonal if possible.  If DiagPivoting is nonzero and if
 *	\a DIAGONAL_PIVOTING is enabled pivots will be chosen only from
 *	the diagonal unless there are no diagonal elements that satisfy
 *	the threshold criteria.  Otherwise, the entire reduced
 *	submatrix is searched when looking for a pivot.  The diagonal
 *	pivoting in Sparse is efficient and well refined, while the
 *	off-diagonal pivoting is not.  For symmetric and near symmetric
 *	matrices, it is best to use diagonal pivoting because it
 *	results in the best performance when reordering the matrix and
 *	when factoring the matrix without ordering.  If there is a
 *	considerable amount of nonsymmetry in the matrix, then
 *	off-diagonal pivoting may result in a better equation ordering
 *	simply because there are more pivot candidates to choose from.
 *	A better ordering results in faster subsequent factorizations.
 *	However, the initial pivot selection process takes considerably
 *	longer for off-diagonal pivoting.
 */

int
sfOrderAndFactor(
	long *Matrix,
	spREAL RHS[],
	spREAL *RelThreshold,
	spREAL *AbsThreshold,
	long *DiagPivoting
)
{
/* Begin `sfOrderAndFactor'. */
    return spOrderAndFactor( (spMatrix)*Matrix, RHS, *RelThreshold,
			     *AbsThreshold, (BOOLEAN)*DiagPivoting );
}







/*  FACTOR MATRIX */
/*!
 *  This routine is the companion routine to spOrderAndFactor().
 *  Unlike sfOrderAndFactor(), sfFactor() cannot change the ordering.
 *  It is also faster than sfOrderAndFactor().  The standard way of
 *  using these two routines is to first use sfOrderAndFactor() for the
 *  initial factorization.  For subsequent factorizations, sfFactor()
 *  is used if there is some assurance that little growth will occur
 *  (say for example, that the matrix is diagonally dominant).  If
 *  sfFactor() is called for the initial factorization of the matrix,
 *  then sfOrderAndFactor() is automatically called with the default
 *  threshold.  This routine uses "row at a time" LU factorization.
 *  Pivots are associated with the lower triangular matrix and the
 *  diagonals of the upper triangular matrix are ones.
 *
 *  \return [INTEGER or INTEGER*2]
 *  The error code is returned.  Possible errors include
 *  \a spNO_MEMORY
 *  \a spSINGULAR
 *  \a spZERO_DIAG
 *  \a spSMALL_PIVOT
 *  Error is cleared in this function.
 *
 *  \param Matrix [INTEGER]
 *	Pointer to matrix.
 */

int
sfFactor( long *Matrix )
{
/* Begin `sfFactor'. */
    return spFactor((spMatrix)*Matrix);
}






/*  PARTITION MATRIX */
/*!
 *  This routine determines the cost to factor each row using both
 *  direct and indirect addressing and decides, on a row-by-row basis,
 *  which addressing mode is fastest.  This information is used in
 *  sfFactor() to speed the factorization.
 *
 *  When factoring a previously ordered matrix using sfFactor(), Sparse
 *  operates on a row-at-a-time basis.  For speed, on each step, the
 *  row being updated is copied into a full vector and the operations
 *  are performed on that vector.  This can be done one of two ways,
 *  either using direct addressing or indirect addressing.  Direct
 *  addressing is fastest when the matrix is relatively dense and
 *  indirect addressing is best when the matrix is quite sparse.  The
 *  user selects the type of partition used with \a Mode.  If \a Mode is set
 *  to \a spDIRECT_PARTITION, then the all rows are placed in the direct
 *  addressing partition.  Similarly, if \a Mode is set to
 *  \a spINDIRECT_PARTITION, then the all rows are placed in the indirect
 *  addressing partition.  By setting \a Mode to \a spAUTO_PARTITION, the
 *  user allows \a Sparse to select the partition for each row
 *  individually.  sfFactor() generally runs faster if Sparse is
 *  allowed to choose its own partitioning, however choosing a
 *  partition is expensive.  The time required to choose a partition is
 *  of the same order of the cost to factor the matrix.  If you plan to
 *  factor a large number of matrices with the same structure, it is
 *  best to let \a Sparse choose the partition.  Otherwise, you should
 *  choose the partition based on the predicted density of the matrix.
 *
 *  \param Matrix [INTEGER]
 *	Pointer to matrix.
 *  \param Mode [INTEGER or INTEGER*2]
 *	Mode must be one of three special codes: \a spDIRECT_PARTITION,
 *	\a spINDIRECT_PARTITION, or \a spAUTO_PARTITION.
 */

void
sfPartition( long *Matrix, int *Mode )
{
/* Begin `sfPartition'. */
    spPartition((spMatrix)*Matrix, *Mode);
}







/*  SOLVE MATRIX EQUATION */
/*!
 *  Performs forward elimination and back substitution to find the
 *  unknown vector from the RHS vector and factored matrix.  This
 *  routine assumes that the pivots are associated with the lower
 *  triangular (L) matrix and that the diagonal of the upper triangular
 *  (U) matrix consists of ones.  This routine arranges the computation
 *  in different way than is traditionally used in order to exploit the
 *  sparsity of the right-hand side.  See the reference in spRevision.
 *
 *  \param Matrix [INTEGER]
 *      Pointer to matrix.
 *  \param RHS [REAL (1) or DOUBLE PRECISION (1)]
 *      \a RHS is the input data array, the right hand side. This data is
 *      undisturbed and may be reused for other solves.
 *  \param Solution [REAL (1) or DOUBLE PRECISION (1)]
 *      \a Solution is the output data array. This routine is constructed such that
 *      \a RHS and \a Solution can be the same array.
 *  \param iRHS [REAL (1) or DOUBLE PRECISION (1)]
 *      \a iRHS is the imaginary portion of the input data array, the right
 *      hand side. This data is undisturbed and may be reused for other solves.
 *      This argument is only necessary if matrix is complex and if
 *      \a spSEPARATED_COMPLEX_VECTOR is set true.
 *  \param iSolution [REAL (1) or DOUBLE PRECISION (1)]
 *      \a iSolution is the imaginary portion of the output data array. This
 *      routine is constructed such that \a iRHS and \a iSolution can be
 *      the same array.  This argument is only necessary if matrix is complex
 *      and if \a spSEPARATED_COMPLEX_VECTOR is set true.
 */

/*VARARGS3*/

void
sfSolve(
    long *Matrix,
    spREAL RHS[],
    spREAL Solution[]
#   if spCOMPLEX AND spSEPARATED_COMPLEX_VECTORS
	, spREAL iRHS[]
	, spREAL iSolution[]
#   endif
)
{
/* Begin `sfSolve'. */
    spSolve( (spMatrix)*Matrix, RHS, Solution IMAG_VECTORS );
}






#if TRANSPOSE
/*  SOLVE TRANSPOSED MATRIX EQUATION */
/*!
 *  Performs forward elimination and back substitution to find the
 *  unknown vector from the RHS vector and transposed factored
 *  matrix. This routine is useful when performing sensitivity analysis
 *  on a circuit using the adjoint method.  This routine assumes that
 *  the pivots are associated with the untransposed lower triangular
 *  (L) matrix and that the diagonal of the untransposed upper
 *  triangular (U) matrix consists of ones.
 *
 *  \param Matrix [INTEGER]
 *      Pointer to matrix.
 *  \param RHS [REAL (1) or DOUBLE PRECISION (1)]
 *      \a RHS is the input data array, the right hand side. This data is
 *      undisturbed and may be reused for other solves.
 *  \param Solution [REAL (1) or DOUBLE PRECISION (1)]
 *      \a Solution is the output data array. This routine is constructed such that
 *      \a RHS and \a Solution can be the same array.
 *  \param iRHS [REAL (1) or DOUBLE PRECISION (1)]
 *      \a iRHS is the imaginary portion of the input data array, the right
 *      hand side. This data is undisturbed and may be reused for other solves.
 *      If \a spSEPARATED_COMPLEX_VECTOR is set false, or if matrix is real, there
 *      is no need to supply this array.
 *  \param iSolution [REAL (1) or DOUBLE PRECISION (1)]
 *      \a iSolution is the imaginary portion of the output data array. This
 *      routine is constructed such that \a iRHS and \a iSolution can be
 *      the same array.  If \a spSEPARATED_COMPLEX_VECTOR is set false, or if
 *      matrix is real, there is no need to supply this array.
 */

/*VARARGS3*/

void
sfSolveTransposed( 
    long *Matrix,
    spREAL RHS[],
    spREAL Solution[]
#   if spCOMPLEX AND spSEPARATED_COMPLEX_VECTORS
	, spREAL iRHS[]
	, spREAL iSolution[]
#   endif
)
{
/* Begin `sfSolveTransposed'. */
    spSolveTransposed( (spMatrix)*Matrix, RHS, Solution IMAG_VECTORS );
}
#endif /* TRANSPOSE */





#if DOCUMENTATION
/*  PRINT MATRIX */
/*!
 *  Formats and send the matrix to standard output.  Some elementary
 *  statistics are also output.  The matrix is output in a format that is
 *  readable by people.
 *
 *  \param Matrix [INTEGER]
 *	Pointer to matrix.
 *  \param PrintReordered [LOGICAL]
 *	Indicates whether the matrix should be printed out in its original
 *	form, as input by the user, or whether it should be printed in its
 *	reordered form, as used by the matrix routines.  A zero indicates that
 *	the matrix should be printed as inputed, a one indicates that it
 *	should be printed reordered.
 *  \param Data [LOGICAL]
 *	Boolean flag that when false indicates that output should be
 *	compressed such that only the existence of an element should be
 *	indicated rather than giving the actual value. Thus 10 times as many
 *	can be printed on a row.  A zero signifies that the matrix should
 *	be printed compressed.  A one indicates that the matrix should be
 *	printed in all its glory.
 *  \param Header [LOGICAL]
 *	Flag indicating that extra information such as the row and column
 *	numbers should be printed.
 */

void
sfPrint( long *Matrix, long *PrintReordered, long *Data, long *Header )
{
/* Begin `sfPrint'. */
    spPrint( (spMatrix)*Matrix, (int)*PrintReordered,
				(int)*Data, (int)*Header );
}
#endif /* DOCUMENTATION */






#if DOCUMENTATION
/*  OUTPUT MATRIX TO FILE */
/*!
 *  Writes matrix to file in format suitable to be read back in by the
 *  matrix test program.  Data is sent to a file with a fixed name 
 *  (MATRIX_FILE_NAME) because it is impossible to pass strings from
 *  FORTRAN to C in a manner that is portable.
 *
 *  \return
 *  One is returned if routine was successful, otherwise zero is returned.
 *  The calling function can query errno (the system global error variable)
 *  as to the reason why this routine failed.
 *
 *  \param Matrix [INTEGER]
 *      Pointer to matrix.
 *  \param Reordered [LOGICAL]
 *      Specifies whether matrix should be output in reordered form,
 *	or in original order.
 *  \param Data [LOGICAL]
 *      Indicates that the element values should be output along with
 *      the indices for each element.  This parameter must be true if
 *      matrix is to be read by the sparse test program.
 *  \param Header [LOGICAL]
 *      Indicates that header is desired.  This parameter must be true if
 *      matrix is to be read by the sparse test program.
 */

long
sfFileMatrix( long *Matrix, long *Reordered, long *Data, long *Header )
{
/* Begin `sfFileMatrix'. */
    return spFileMatrix( (spMatrix)*Matrix, MATRIX_FILE_NAME, "",
			 (int)*Reordered, (int)*Data, (int)*Header );
}
#endif /* DOCUMENTATION */






#if DOCUMENTATION
/*  OUTPUT SOURCE VECTOR TO FILE */
/*!
 *  Writes vector to file in format suitable to be read back in by the
 *  matrix test program.  This routine should be executed after the function
 *  sfFileMatrix.
 *
 *  \return
 *  One is returned if routine was successful, otherwise zero is returned.
 *  The calling function can query errno (the system global error variable)
 *  as to the reason why this routine failed.
 *
 *  \param Matrix [INTEGER]
 *      Pointer to matrix.
 *  \param RHS [REAL (1) or DOUBLE PRECISION (1)]
 *      Right-hand side vector. This is only the real portion if
 *      \a spSEPARATED_COMPLEX_VECTORS is true.
 *  \param iRHS [REAL (1) or DOUBLE PRECISION (1)]
 *      Right-hand side vector, imaginary portion.  Not necessary if matrix
 *      is real or if \a spSEPARATED_COMPLEX_VECTORS is set false.
 */

int
sfFileVector( 
    long *Matrix,
    spREAL RHS[]
#   if spCOMPLEX AND spSEPARATED_COMPLEX_VECTORS
	, spREAL iRHS[]
#   endif
)
{
/* Begin `sfFileVector'. */
    return spFileVector( (spMatrix)*Matrix, MATRIX_FILE_NAME, RHS IMAG_RHS );
}
#endif /* DOCUMENTATION */







#if DOCUMENTATION
/*  OUTPUT STATISTICS TO FILE */
/*!
 *  Writes useful information concerning the matrix to a file.  Should be
 *  executed after the matrix is factored.
 *  Data is sent to a file with a fixed name (STATS_FILE_NAME) because
 *  it is impossible to pass strings from FORTRAN to C in a manner that is
 *  portable.
 * 
 *  \return [LOGICAL]
 *  One is returned if routine was successful, otherwise zero is returned.
 *  The calling function can query errno (the system global error variable)
 *  as to the reason why this routine failed.
 *
 *  \param Matrix [INTEGER]
 *      Pointer to matrix.
 */

int
sfFileStats( long *Matrix )
{
/* Begin `sfFileStats'. */
    return spFileStats( (spMatrix)*Matrix, STATS_FILE_NAME, "" );
}
#endif /* DOCUMENTATION */




#if MODIFIED_NODAL
/*  PREORDER MODIFIED NODE ADMITTANCE MATRIX TO REMOVE ZEROS FROM DIAGONAL */
/*!
 *  This routine massages modified node admittance matrices to remove
 *  zeros from the diagonal.  It takes advantage of the fact that the
 *  row and column associated with a zero diagonal usually have
 *  structural ones placed symmetricly.  This routine should be used
 *  only on modified node admittance matrices and should be executed
 *  after the matrix has been built but before the factorization
 *  begins.  It should be executed for the initial factorization only
 *  and should be executed before the rows have been linked.  Thus it
 *  should be run before using spScale(), spMultiply(),
 *  spDeleteRowAndCol(), or spNorm().
 *
 *  This routine exploits the fact that the structural one are placed
 *  in the matrix in symmetric twins.  For example, the stamps for
 *  grounded and a floating voltage sources are \code
 *  grounded:              floating:
 *  [  x   x   1 ]         [  x   x   1 ]
 *  [  x   x     ]         [  x   x  -1 ]
 *  [  1         ]         [  1  -1     ]
 *  \endcode
 *  Notice for the grounded source, there is one set of twins, and for
 *  the grounded, there are two sets.  We remove the zero from the diagonal
 *  by swapping the rows associated with a set of twins.  For example:
 *  grounded:              floating 1:            floating 2: \code
 *  [  1         ]         [  1  -1     ]         [  x   x   1 ]
 *  [  x   x     ]         [  x   x  -1 ]         [  1  -1     ]
 *  [  x   x   1 ]         [  x   x   1 ]         [  x   x  -1 ]
 *  \endcode
 *
 *  It is important to deal with any zero diagonals that only have one
 *  set of twins before dealing with those that have more than one because
 *  swapping row destroys the symmetry of any twins in the rows being
 *  swapped, which may limit future moves.  Consider \code
 *  [  x   x   1     ]
 *  [  x   x  -1   1 ]
 *  [  1  -1         ]
 *  [      1         ]
 *  \endcode
 *  There is one set of twins for diagonal 4 and two for diagonal3.
 *  Dealing with diagonal for first requires swapping rows 2 and 4. \code
 *  [  x   x   1     ]
 *  [      1         ]
 *  [  1  -1         ]
 *  [  x   x  -1   1 ]
 *  \endcode
 *  We can now deal with diagonal 3 by swapping rows 1 and 3. \code
 *  [  1  -1         ]
 *  [      1         ]
 *  [  x   x   1     ]
 *  [  x   x  -1   1 ]
 *  \endcode
 *  And we are done, there are no zeros left on the diagonal.  However, if
 *  we originally dealt with diagonal 3 first, we could swap rows 2 and 3 \code
 *  [  x   x   1     ]
 *  [  1  -1         ]
 *  [  x   x  -1   1 ]
 *  [      1         ]
 *  \endcode
 *  Diagonal 4 no longer has a symmetric twin and we cannot continue.
 *
 *  So we always take care of lone twins first.  When none remain, we
 *  choose arbitrarily a set of twins for a diagonal with more than one set
 *  and swap the rows corresponding to that twin.  We then deal with any
 *  lone twins that were created and repeat the procedure until no
 *  zero diagonals with symmetric twins remain.
 *
 *  In this particular implementation, columns are swapped rather than rows.
 *  The algorithm used in this function was developed by Ken Kundert and
 *  Tom Quarles.
 *
 *  \param Matrix [INTEGER]
 *      Pointer to the matrix to be preordered.
 */

void
sfMNA_Preorder( long *Matrix )
{
/* Begin `sfMNA_Preorder'. */
    spMNA_Preorder( (spMatrix)*Matrix );
}
#endif /* MODIFIED_NODAL */






#if SCALING
/*  SCALE MATRIX */
/*!
 *  This function scales the matrix to enhance the possibility of
 *  finding a good pivoting order.  Note that scaling enhances accuracy
 *  of the solution only if it affects the pivoting order, so it makes
 *  no sense to scale the matrix before spFactor().  If scaling is
 *  desired it should be done before spOrderAndFactor().  There
 *  are several things to take into account when choosing the scale
 *  factors.  First, the scale factors are directly multiplied against
 *  the elements in the matrix.  To prevent roundoff, each scale factor
 *  should be equal to an integer power of the number base of the
 *  machine.  Since most machines operate in base two, scale factors
 *  should be a power of two.  Second, the matrix should be scaled such
 *  that the matrix of element uncertainties is equilibrated.  Third,
 *  this function multiplies the scale factors by the elements, so if
 *  one row tends to have uncertainties 1000 times smaller than the
 *  other rows, then its scale factor should be 1024, not 1/1024.
 *  Fourth, to save time, this function does not scale rows or columns
 *  if their scale factors are equal to one.  Thus, the scale factors
 *  should be normalized to the most common scale factor.  Rows and
 *  columns should be normalized separately.  For example, if the size
 *  of the matrix is 100 and 10 rows tend to have uncertainties near
 *  1e-6 and the remaining 90 have uncertainties near 1e-12, then the
 *  scale factor for the 10 should be 1/1,048,576 and the scale factors
 *  for the remaining 90 should be 1.  Fifth, since this routine
 *  directly operates on the matrix, it is necessary to apply the scale
 *  factors to the RHS and Solution vectors.  It may be easier to
 *  simply use spOrderAndFactor() on a scaled matrix to choose the
 *  pivoting order, and then throw away the matrix.  Subsequent
 *  factorizations, performed with spFactor(), will not need to have
 *  the RHS and Solution vectors descaled.  Lastly, this function
 *  should not be executed before the function spMNA_Preorder.
 *
 *  \param Matrix [INTEGER]
 *      Pointer to the matrix to be scaled.
 *  \param SolutionScaleFactors [REAL(1) or DOUBLE PRECISION(1)]
 *      The array of Solution scale factors.  These factors scale the columns.
 *      All scale factors are real valued.
 *  \param RHS_ScaleFactors [REAL(1) or DOUBLE PRECISION(1)]
 *      The array of RHS scale factors.  These factors scale the rows.
 *      All scale factors are real valued.
 */

void
sfScale( long *Matrix, spREAL RHS_ScaleFactors[], spREAL SolutionScaleFactors[] )
{
/* Begin `sfScale'. */
    spScale( (spMatrix)*Matrix, RHS_ScaleFactors, SolutionScaleFactors );
}
#endif /* SCALING */






#if MULTIPLICATION
/*  MATRIX MULTIPLICATION */
/*!
 *  Multiplies matrix by solution vector to find source vector.
 *  Assumes matrix has not been factored.  This routine can be used
 *  as a test to see if solutions are correct.  It should not be used
 *  before PreorderFoModifiedNodal().
 *
 *  \param Matrix [INTEGER]
 *      Pointer to the matrix.
 *  \param RHS [REAL(1) or DOUBLE PRECISION(1)]
 *      RHS is the right hand side. This is what is being solved for.
 *  \param Solution [REAL(1) or DOUBLE PRECISION(1)]
 *      Solution is the vector being multiplied by the matrix.
 *  \param iRHS [REAL(1) or DOUBLE PRECISION(1)]
 *      iRHS is the imaginary portion of the right hand side. This is
 *      what is being solved for.  This is only necessary if the matrix is
 *      complex and spSEPARATED_COMPLEX_VECTORS is true.
 *  \param iSolution [REAL(1) or DOUBLE PRECISION(1)]
 *      iSolution is the imaginary portion of the vector being multiplied
 *      by the matrix. This is only necessary if the matrix is
 *      complex and spSEPARATED_COMPLEX_VECTORS is true.
 */

void
sfMultiply( 
    long *Matrix,
    spREAL RHS[],
    spREAL Solution[]
#if spCOMPLEX AND spSEPARATED_COMPLEX_VECTORS
    , spREAL iRHS[]
    , spREAL iSolution[]
#endif
)
{
/* Begin `sfMultiply'. */
    spMultiply( (spMatrix)*Matrix, RHS, Solution IMAG_VECTORS );
}
#endif /* MULTIPLICATION */






#if MULTIPLICATION AND TRANSPOSE
/*  TRANSPOSED MATRIX MULTIPLICATION */
/*!
 *  Multiplies transposed matrix by solution vector to find source vector.
 *  Assumes matrix has not been factored.  This routine can be used
 *  as a test to see if solutions are correct.  It should not be used
 *  before PreorderFoModifiedNodal().
 *
 *  \param Matrix [INTEGER]
 *      Pointer to the matrix.
 *  \param RHS [REAL(1) or DOUBLE PRECISION(1)]
 *      RHS is the right hand side. This is what is being solved for.
 *  \param Solution [REAL(1) or DOUBLE PRECISION(1)]
 *      Solution is the vector being multiplied by the matrix.
 *  \param iRHS [REAL(1) or DOUBLE PRECISION(1)]
 *      iRHS is the imaginary portion of the right hand side. This is
 *      what is being solved for.  This is only necessary if the matrix is
 *      complex and spSEPARATED_COMPLEX_VECTORS is true.
 *  \param iSolution [REAL(1) or DOUBLE PRECISION(1)]
 *      iSolution is the imaginary portion of the vector being multiplied
 *      by the matrix. This is only necessary if the matrix is
 *      complex and spSEPARATED_COMPLEX_VECTORS is true.
 */

void
sfMultTransposed(
    long *Matrix,
    spREAL RHS[],
    spREAL Solution[]
#if spCOMPLEX AND spSEPARATED_COMPLEX_VECTORS
    , spREAL iRHS[]
    , spREAL iSolution[]
#endif
)
{
/* Begin `sfMultTransposed'. */
    spMultTransposed( (spMatrix)*Matrix, RHS, Solution IMAG_VECTORS );
}
#endif /* MULTIPLICATION AND TRANSPOSE */






#if DETERMINANT

/*  CALCULATE DETERMINANT */
/*!
 *  This routine in capable of calculating the determinant of the
 *  matrix once the LU factorization has been performed.  Hence, only
 *  use this routine after spFactor() and before spClear().
 *  The determinant equals the product of all the diagonal elements of
 *  the lower triangular matrix L, except that this product may need
 *  negating.  Whether the product or the negative product equals the
 *  determinant is determined by the number of row and column
 *  interchanges performed.  Note that the determinants of matrices can
 *  be very large or very small.  On large matrices, the determinant
 *  can be far larger or smaller than can be represented by a floating
 *  point number.  For this reason the determinant is scaled to a
 *  reasonable value and the logarithm of the scale factor is returned.
 *
 *  \param Matrix [INTEGER]
 *      A pointer to the matrix for which the determinant is desired.
 *  \param pExponent [INTEGER or INTEGER*2]
 *      The logarithm base 10 of the scale factor for the determinant.  To
 *	find
 *      the actual determinant, Exponent should be added to the exponent of
 *      DeterminantReal.
 *  \param pDeterminant [REAL or DOUBLE PRECISION]
 *      The real portion of the determinant.   This number is scaled to be
 *      greater than or equal to 1.0 and less than 10.0.
 *  \param piDeterminant [REAL or DOUBLE PRECISION]
 *      The imaginary portion of the determinant.  When the matrix is real
 *      this pointer need not be supplied, nothing will be returned.   This
 *      number is scaled to be greater than or equal to 1.0 and less than 10.0.
 */

#if spCOMPLEX

void
sfDeterminant(
    long *Matrix,
    spREAL *pDeterminant,
    spREAL *piDeterminant,
    int  *pExponent
)
{
/* Begin `sfDeterminant'. */
    spDeterminant( (spMatrix)*Matrix, pExponent, pDeterminant, piDeterminant );
}

#else /* spCOMPLEX */

void
sfDeterminant( Matrix, pExponent, pDeterminant )

long *Matrix;
RealNumber *pDeterminant;
int  *pExponent;
{
/* Begin `sfDeterminant'. */
    spDeterminant( (spMatrix)*Matrix, pExponent, pDeterminant );
}
#endif /* spCOMPLEX */
#endif /* DETERMINANT */






/*  RETURN MATRIX ERROR STATUS */
/*!
 *  This function is used to determine the error status of the given matrix.
 *
 *  \return [INTEGER or INTEGER*2]
 *     The error status of the given matrix.
 *
 *  \param Matrix [INTEGER]
 *     The matrix for which the error status is desired.
 */

int
sfErrorState( long  *Matrix )
{
/* Begin `sfError'. */
    return spErrorState( (spMatrix)*Matrix );
}






/*  PRINT MATRIX ERROR MESSAGE */
/*!
 *  This function prints a Sparse error message to stderr.
 *
 *  \param Matrix [INTEGER]
 *     The matrix for which the error message is desired.
 */

void
sfErrorMessage( long  *Matrix )
{
/* Begin `sfErrorMessage'. */
    spErrorMessage( (spMatrix)*Matrix, stderr, NULL );
}






/*  WHERE IS MATRIX SINGULAR */
/*!
 *  This function returns the row and column number where the matrix was
 *  detected as singular or where a zero was detected on the diagonal.
 *
 *  \param Matrix [INTEGER]
 *     The matrix for which the error status is desired.
 *  \param pRow  [INTEGER or INTEGER*2]
 *     The row number.
 *  \param pCol  [INTEGER or INTEGER*2]
 *     The column number.
 */

void
sfWhereSingular( long *Matrix, int *Row, int *Col )
{
/* Begin `sfWhereSingular'. */
    spWhereSingular( (spMatrix)*Matrix, Row, Col );
}





/*   MATRIX SIZE */
/*!
 *   Returns the size of the matrix.  Either the internal or external size of
 *   the matrix is returned.
 *
 *   \param Matrix [INTEGER]
 *       Pointer to matrix.
 *   \param External [LOGICAL]
 *       If External is set true, the external size , i.e., the value of the
 *       largest external row or column number encountered is returned.
 *       Otherwise the true size of the matrix is returned.  These two sizes
 *       may differ if the TRANSLATE option is set true.
 */

int
sfGetSize( long  *Matrix, long *External )
{
/* Begin `sfGetSize'. */
    return spGetSize( (spMatrix)*Matrix, (BOOLEAN)*External );
}








/*   SET MATRIX REAL */
/*!
 *   Forces matrix to be real.
 *
 *   \param Matrix [INTEGER]
 *       Pointer to matrix.
 */

void
sfSetReal( long *Matrix )
{
/* Begin `sfSetReal'. */
    spSetReal( (spMatrix)*Matrix );
}


/*   SET MATRIX COMPLEX */
/*!
 *   Forces matrix to be complex.
 *
 *   \param Matrix [INTEGER]
 *       Pointer to matrix.
 */

void
sfSetComplex( long *Matrix )
{
/* Begin `sfSetComplex'. */
    spSetComplex( (spMatrix)*Matrix );
}









/*   FILL-IN COUNT */
/*!
 *   Returns the number of fill-ins in the matrix.
 *
 *   >>> Arguments:
 *   Matrix [INTEGER]
 *       Pointer to matrix.
 */

int
sfFillinCount( long *Matrix )
{
/* Begin `sfFillinCount'. */
    return spFillinCount( (spMatrix)*Matrix );
}


/*   ELEMENT COUNT */
/*!
 *   Returns the total number of total elements in the matrix.
 *
 *   >>> Arguments:
 *   Matrix [INTEGER]
 *       Pointer to matrix.
 */

int
sfElementCount( long *Matrix )
{
/* Begin `sfElementCount'. */
    return spElementCount( (spMatrix)*Matrix );
}






#if TRANSLATE AND DELETE

/*  DELETE A ROW AND COLUMN FROM THE MATRIX */
/*!
 *  Deletes a row and a column from a matrix.
 *
 *  Sparse will abort if an attempt is made to delete a row or column that
 *  doesn't exist.
 *
 *  \param Matrix [INTEGER]
 *     Pointer to the matrix in which the row and column are to be deleted.
 *  \param Row [INTEGER or INTEGER*2]
 *     Row to be deleted.
 *  \param Col [INTEGER or INTEGER*2]
 *     Column to be deleted.
 */

void
sfDeleteRowAndCol( long *Matrix, int *Row, int *Col )
{
/* Begin `sfDeleteRowAndCol'. */
    spDeleteRowAndCol( (spMatrix)*Matrix, *Row, *Col );
}
#endif





#if PSEUDOCONDITION

/*  CALCULATE PSEUDOCONDITION */
/*!
 *  Computes the magnitude of the ratio of the largest to the smallest
 *  pivots.  This quantity is an indicator of ill-conditioning in the
 *  matrix.  If this ratio is large, and if the matrix is scaled such
 *  that uncertainties in the RHS and the matrix entries are
 *  equilibrated, then the matrix is ill-conditioned.  However, a small
 *  ratio does not necessarily imply that the matrix is
 *  well-conditioned.  This routine must only be used after a matrix
 *  has been factored by sfOrderAndFactor() or sfFactor() and before it
 *  is cleared by sfClear() or spInitialize().  The pseudocondition is faster
 *  to compute than the condition number calculated by sfCondition(), but
 *  is not as informative.
 *
 *  \return [REAL or DOUBLE PRECISION]
 *  The magnitude of the ratio of the largest to smallest pivot used during
 *  previous factorization.  If the matrix was singular, zero is returned.
 *
 *  \param Matrix [INTEGER]
 *     Pointer to the matrix.
 */

spREAL
sfPseudoCondition( long *Matrix )
{
/* Begin `sfPseudoCondition'. */
    return spPseudoCondition( (spMatrix)Matrix );
}
#endif







#if CONDITION

/*  ESTIMATE CONDITION NUMBER */
/*!
 *  Computes an estimate of the condition number using a variation on
 *  the LINPACK condition number estimation algorithm.  This quantity is
 *  an indicator of ill-conditioning in the matrix.  To avoid problems
 *  with overflow, the reciprocal of the condition number is returned.
 *  If this number is small, and if the matrix is scaled such that
 *  uncertainties in the RHS and the matrix entries are equilibrated,
 *  then the matrix is ill-conditioned.  If the this number is near
 *  one, the matrix is well conditioned.  This routine must only be
 *  used after a matrix has been factored by sfOrderAndFactor() or
 *  sfFactor() and before it is cleared by sfClear() or spInitialize().
 *
 *  Unlike the LINPACK condition number estimator, this routines
 *  returns the L infinity condition number.  This is an artifact of
 *  Sparse placing ones on the diagonal of the upper triangular matrix
 *  rather than the lower.  This difference should be of no importance.
 *
 *  \b References:
 *  A.K. Cline, C.B. Moler, G.W. Stewart, J.H. Wilkinson.  An estimate
 *  for the condition number of a matrix.  SIAM Journal on Numerical
 *  Analysis.  Vol. 16, No. 2, pages 368-375, April 1979.
 *  
 *  J.J. Dongarra, C.B. Moler, J.R. Bunch, G.W. Stewart.  LINPACK
 *  User's Guide.  SIAM, 1979.
 *  
 *  Roger G. Grimes, John G. Lewis.  Condition number estimation for
 *  sparse matrices.  SIAM Journal on Scientific and Statistical
 *  Computing.  Vol. 2, No. 4, pages 384-388, December 1981.
 *  
 *  Dianne Prost O'Leary.  Estimating matrix condition numbers.  SIAM
 *  Journal on Scientific and Statistical Computing.  Vol. 1, No. 2,
 *  pages 205-209, June 1980.
 *
 *  \return [REAL or DOUBLE PRECISION]
 *  The reciprocal of the condition number.  If the matrix was singular,
 *  zero is returned.
 *
 *  \param Matrix [INTEGER]
 *	Pointer to the matrix.
 *  \param NormOfMatrix [REAL or DOUBLE PRECISION]
 *	The L-infinity norm of the unfactored matrix as computed by
 *	spNorm().
 *  \param pError [INTEGER or INTEGER*2]
 *	Used to return error code.  Possible errors include \a spSINGULAR
 *      and \a spNO_MEMORY.
 */

spREAL
sfCondition( long *Matrix, spREAL *NormOfMatrix, int *pError )
{
/* Begin `sfCondition'. */
    return spCondition( (spMatrix)*Matrix, *NormOfMatrix, pError );
}





/*  L-INFINITY MATRIX NORM */
/*!
 *  Computes the L-infinity norm of an unfactored matrix.  It is a fatal
 *  error to pass this routine a factored matrix.
 *
 *  \return [REAL or DOUBLE PRECISION]
 *  The largest absolute row sum of matrix.
 *
 *  \param Matrix [INTEGER]
 *     Pointer to the matrix.
 */

spREAL
sfNorm( long *Matrix )
{
/* Begin `sfNorm'. */
    return spNorm( (spMatrix)*Matrix );
}
#endif /* CONDITION */





#if STABILITY

/*  LARGEST ELEMENT IN MATRIX */
/*!
 *  spLargestElement() finds the magnitude on the largest element in the
 *  matrix.  If the matrix has not yet been factored, the largest
 *  element is found by direct search.  If the matrix is factored, a
 *  bound on the largest element in any of the reduced submatrices is
 *  computed.
 *
 *  \return [REAL or DOUBLE PRECISION]
 *  If matrix is not factored, returns the magnitude of the largest element in
 *  the matrix.  If the matrix is factored, a bound on the magnitude of the
 *  largest element in any of the reduced submatrices is returned.
 *
 *  \param Matrix [INTEGER]
 *     Pointer to the matrix.
 *
 *  \see spLargestElement()
 */

spREAL
sfLargestElement( long *Matrix )
{
/* Begin `sfLargestElement'. */
    return spLargestElement( (spMatrix)Matrix );
}




/*  MATRIX ROUNDOFF ERROR */
/*!
 *  This routine, along with spLargestElement(), are used to gauge the
 *  stability of a factorization. See description of spLargestElement()
 *  for more information.
 *
 *  \return [REAL or DOUBLE PRECISION]
 *  Returns a bound on the magnitude of the largest element in E = A - LU.
 *
 *  \param Matrix [INTEGER]
 *	Pointer to the matrix.
 *  \param Rho [REAL or DOUBLE PRECISION]
 *	The bound on the magnitude of the largest element in any of the
 *	reduced submatrices.  This is the number computed by the function
 *	spLargestElement() when given a factored matrix.  If this number is
 *	negative, the bound will be computed automatically.
 *  \see spRoundoff()
 */

spREAL
sfRoundoff( long *Matrix, spREAL *Rho )
{
/* Begin `sfRoundoff'. */
    return spRoundoff( (spMatrix)*Matrix, *Rho );
}
#endif

#endif /* FORTRAN */
