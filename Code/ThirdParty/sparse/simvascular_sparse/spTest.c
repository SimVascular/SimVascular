/*
 *  TEST MODULE for the sparse matrix routines
 *
 *  Author:                     Advisor:
 *     Kenneth S. Kundert           Alberto Sangiovanni-Vincentelli
 *     UC Berkeley
 *
 *  This file contains the test routine for the sparse matrix routines.
 *  They are able to read matrices from files and solve them.
 *
 *  >>> Functions contained in this file:
 *  main
 *  ReadMatrixFromFile
 *  Init
 *  CheckOutComplexArray
 *  CheckInAllComplexArrays
 *  PrintMatrixErrorMessage
 *  InterpretCommandLine
 *  GetProgramName
 *  Usage
 *  EnlargeVectors
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
    "@(#)$Header: /cvsroot/sparse/src/spTest.c,v 1.5 2003/06/30 19:40:51 kundert Exp $";
#endif



/*
 *  IMPORTS
 *
 *  >>> Import descriptions:
 *  stdio.h math.h ctype.h
 *      Standard C libraries.
 *  spConfig.h
 *      Macros that customize the sparse matrix package. It is not normally
 *      necessary, nor is normally particularly desirable to include this
 *      file into the calling routines.  Nor should spINSIDE_SPARSE be defined.
 *      It is done in this test file so that the complex test routines may be
 *      removed when they don't exist in Sparse.
 *  spMatrix.h
 *      Macros and declarations to be imported by the user.
 */

#define  YES    1
#define  NO     0

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#define spINSIDE_SPARSE
#include "spConfig.h"
#undef spINSIDE_SPARSE
#include "spMatrix.h"


/* Declarations that should be in strings.h. */
extern int strcmp(), strncmp(), strlen();

#ifdef lint
#undef  MULTIPLICATION
#undef  DETERMINANT

#define  MULTIPLICATION                 YES
#define  DETERMINANT                    YES

#define LINT    YES
#else /* not lint */
#define LINT    NO
#endif /* not lint */







/*
 *   TIMING
 */


#ifndef vms
#   include <sys/param.h>
#endif
#ifndef HZ
#       ifdef __STDC__
#       include <time.h>
#       include <limits.h>
#       define HZ   CLK_TCK
#   else
#       ifdef mips
#           include <time.h>
#           define HZ   CLK_TCK
#       endif
#   endif
#endif
#ifndef HZ
#   ifdef vax
#       ifdef unix
#           define HZ   60
#       endif
#       ifdef vms
#           define HZ   100
#       endif
#   endif
#   ifdef hpux
#       ifdef hp300
#           define HZ   50
#       endif
#       ifdef hp500
#           define HZ   60
#       endif
#   endif
#   ifdef hpux
#       ifdef hp300
#           define HZ   50
#       endif
#       ifdef hp500
#           define HZ   60
#       endif
#   endif
#endif

/* Routine that queries the system to find the process time. */
double
Time()
{
    struct time {long user, system, childuser, childsystem;} time;
    (void)times(&time);
    return (double)time.user / (double)HZ;
}





/*
 *  MACROS
 */

#define  BOOLEAN  int
#define  NOT  !
#define  AND  &&
#define  OR  ||
#define  ALLOC(type,number) ((type *)malloc((unsigned)(sizeof(type)*(number))))
#define  ABS(a)         ((a) < 0.0 ? -(a) : (a))
#ifdef MAX
#undef  MAX
#endif
#ifdef MIN
#undef  MIN
#endif
#define  MAX(a,b)       ((a) > (b) ? (a) : (b))
#define  MIN(a,b)       ((a) < (b) ? (a) : (b))






/*
 *  IMAGINARY VECTORS
 *
 *  The imaginary vectors iRHS and iSolution are only needed when the
 *  options spCOMPLEX and spSEPARATED_COMPLEX_VECTORS are set.  The following
 *  macro makes it easy to include or exclude these vectors as needed.
 */

#if spCOMPLEX AND spSEPARATED_COMPLEX_VECTORS
#define IMAG_VECTORS    , iRHS, iSolution
#else
#define IMAG_VECTORS
#endif







/*
 *   GLOBAL DECLARATIONS
 *
 *   These variables, types, and macros are used throughout this file.
 *
 *   >>> Macros
 *   PRINT_LIMIT
 *       The maximum number of terms to be printed form the solution vector.
 *       May be overridden by the -n option.
 *
 *   >>> Variable types:
 *   ElementRecord
 *       A structure for holding data for each matrix element.
 *
 *   >>> Global variables:
 *  ProgramName  (char *)
 *      The name of the test program with any path stripped off.
 *  FileName  (char *)
 *      The name of the current file name.
 *  Complex  (BOOLEAN)
 *      The flag that indicates whether the matrix is complex or real.
 *  Element  (ElementRecord[])
 *      Array used to hold information about all the elements.
 *  Matrix  (char *)
 *      The pointer to the matrix.
 *  Size  (int)
 *      The size of the matrix.
 *  RHS  (RealVector)
 *      The right-hand side vector (b in Ax = b).
 *  Solution  (RealVector)
 *      The solution vector (x in Ax = b).
 *  RHS_Verif  (RealVector)
 *      The calculated RHS vector.
 */

/* Begin global declarations. */
#define  PRINT_LIMIT  9

typedef  spREAL  RealNumber, *RealVector;

typedef  struct
{   RealNumber Real;
    RealNumber Imag;
} ComplexNumber, *ComplexVector;

static char *ProgramName;
static char *FileName;
static int  Size, MaxSize = 0;
static int  PrintLimit = PRINT_LIMIT, Iterations = 1, ColumnAsRHS = 1;
static BOOLEAN  Complex, SolutionOnly = NO, RealAsComplex = NO, Transposed = NO;
static BOOLEAN  CreatePlotFiles = NO, UseColumnAsRHS = NO, PrintLimitSet = NO;
static BOOLEAN ExpansionWarningGiven, DiagPivoting = DIAGONAL_PIVOTING;
static char  Message[BUFSIZ], *Matrix = NULL;
static RealNumber AbsThreshold = (RealNumber)0, RelThreshold = (RealNumber)0;
static RealVector RHS, Solution, RHS_Verif;
static RealVector iRHS, iSolution, iRHS_Verif;
static ComplexVector  cRHS, cSolution, cRHS_Verif;






/*
 * Function declarations
 */

static int  ReadMatrixFromFile();
static int  Init();
static ComplexVector CheckOutComplexArray();
static void CheckInAllComplexArrays();
static void PrintMatrixErrorMessage();
static int  InterpretCommandLine();
static char *GetProgramName();
static void Usage();
static void EnlargeVectors();







/*
 *  MAIN TEST ROUTINE for sparse matrix routines
 *
 *  This routine reads a matrix from a file and solves it several
 *  times.  The solution is printed along with some statistics.
 *  The format expected for the input matrix is the same as what is output by
 *  spFileMatrix and spFileVector.
 *
 *  >>> Local variables:
 *  Determinant  (RealNumber)
 *      Real portion of the determinant of the matrix.
 *  iDeterminant  (RealNumber)
 *      Imaginary portion of the determinant of the matrix.
 *  Error  (int)
 *      Holds error status.
 *  Last  (int)
 *      The index of the last term to be printed of the solution.
 *  Iterations  (int)
 *      The number of times that the matrix will be factored and solved.
 *  MaxRHS  (RealNumber)
 *      The largest term in the given RHS vector.
 *  Residual  (RealNumber)
 *      The sum of the magnitude of the differences in each corresponding
 *      term of the given and calculated RHS vector.
 *  Exponent  (int)
 *      Exponent for the determinant.
 *  Growth  (RealNumber)
 *      The growth that has occurred in the matrix during the factorization.
 *
 *  >>> Timing variables:
 *  BuildTime  (double)
 *      The time required to build up the matrix including the time to clear
 *      the matrix.
 *  ConditionTime  (double)
 *      The time required to compute the condition number.
 *  DeterminantTime  (double)
 *      The time required to compute the determinant.
 *  FactorTime  (double)
 *      The time required to factor the matrix without ordering.
 *  InitialFactorTime  (double)
 *      The time required to factor the matrix with ordering.
 *  SolveTime  (double)
 *      The time required to perform the forward and backward elimination.
 *  StartTime  (double)
 *      The time that a timing interval was started.
 *
 *  >>> Global variables:
 *  Complex  <used>
 *  Matrix  <set>
 *  Size  <used>
 *  RHS  <used>
 *  iRHS  <used>
 */


int
main( int ArgCount, char **Args )
{
register  long I;
int  Error, Last, Elements, Fillins;
RealNumber  MaxRHS, Residual;
RealNumber  Determinant, iDeterminant;
int  Exponent;
RealNumber  ConditionNumber, PseudoCondition;
RealNumber  LargestBefore, LargestAfter, Roundoff, InfNorm;
BOOLEAN StandardInput;
char j, PlotFile[BUFSIZ], ErrMsg[BUFSIZ];
double  StartTime, BeginTime, BuildTime, FactorTime, SolveTime, PartitionTime;
double  InitialFactorTime, ConditionTime, DeterminantTime;
extern double  Time();
extern char *sbrk();

/* Begin `main'. */

    BeginTime = Time();
    ArgCount = InterpretCommandLine( ArgCount, Args );

/* Assure that the Sparse is compatible with this test program.*/
#   if NOT EXPANDABLE OR NOT INITIALIZE OR NOT ARRAY_OFFSET
        fprintf(stderr, 
                "%s: Sparse is configured inappropriately for test program.\n",
                ProgramName);
        fprintf(stderr,
      "    Enable EXPANDABLE, INITIALIZE, and ARRAY_OFFSET in `spConfig.h'.\n");
        exit(1);
#   endif

/* Print copyright notice. */
    printf("Sparse1.4\nCopyright (c) 2003, Kenneth S. Kundert.\nAll rights reserved.\n\n");

    do
    {
/* Initialization. */
        BuildTime = FactorTime = SolveTime = 0.0;
        ExpansionWarningGiven = NO;
    
/* Create matrix. */
        Matrix = spCreate( 0, spCOMPLEX, &Error );
        if (Matrix == NULL)
        {   fprintf(stderr, "%s: insufficient memory available.\n",
                            ProgramName);
            exit(1);
        }
        if( Error >= spFATAL ) goto End;

/* Read matrix. */
        if (ArgCount == 0)
            FileName = NULL;
        else
            FileName = *(++Args);
        Error = ReadMatrixFromFile();
        if( Error) goto End;
        StandardInput = (FileName == NULL);

/* Clear solution vector if row and column numbers are not densely packed. */
        if (spGetSize(Matrix, YES) != spGetSize(Matrix, NO))
        {   if (Complex OR RealAsComplex)
            {   for (I = Size; I > 0; I--)
                    cSolution[I].Real = cSolution[I].Imag = 0.0;
            }
            else
            {   for (I = Size; I > 0; I--)
                    Solution[I] = 0.0;
            }
        }

/* Perform initial build, factor, and solve. */
        (void)spInitialize(Matrix, Init);

#if MODIFIED_NODAL
        spMNA_Preorder( Matrix );
#endif
#if DOCUMENTATION
        if (CreatePlotFiles)
        {   if (StandardInput)
                (void)sprintf(PlotFile, "bef");
            else
                (void)sprintf(PlotFile, "%s.bef", FileName);
            if (NOT spFileMatrix( Matrix, PlotFile, FileName, NO, NO, NO ))
            {   (void)sprintf(ErrMsg,"%s: plotfile `%s'",ProgramName,PlotFile);
                perror( ErrMsg );
            }
        }
#if NO
        spPrint( Matrix, NO /*reodered*/, NO /*data*/, NO /*header*/ );
#endif
#endif /* DOCUMENTATION */
#if STABILITY
        if (NOT SolutionOnly) LargestBefore = spLargestElement(Matrix);
#endif
#if CONDITION
        if (NOT SolutionOnly) InfNorm = spNorm(Matrix);
#endif

        StartTime = Time();
        Error = spOrderAndFactor( Matrix, RHS, RelThreshold, AbsThreshold,
                                  DiagPivoting );
        InitialFactorTime = Time() - StartTime;
        if (Error != spOKAY) spErrorMessage( Matrix, stderr, ProgramName );
        if( Error >= spFATAL )
            goto End;

#if DOCUMENTATION
        if (CreatePlotFiles)
        {   if (StandardInput)
                (void)sprintf(PlotFile, "aft");
            else
                (void)sprintf(PlotFile, "%s.aft", FileName);
            if (NOT spFileMatrix( Matrix, PlotFile, FileName, YES, NO, NO ))
            {   (void)sprintf(ErrMsg,"%s: plotfile `%s'",ProgramName,PlotFile);
                perror( ErrMsg );
            }
        }
#if NO
        spFileStats( Matrix, FileName, "stats" );
#endif
#endif /* DOCUMENTATION */

/*
 * IMAG_VECTORS is a macro that replaces itself with `, iRHS, iSolution'
 * if the options spCOMPLEX and spSEPARATED_COMPLEX_VECTORS are set,
 * otherwise it disappears without a trace.
 */
#if TRANSPOSE
        if (Transposed)
            spSolveTransposed( Matrix, RHS, Solution IMAG_VECTORS );
        else
#endif
            spSolve( Matrix, RHS, Solution IMAG_VECTORS );

        if (SolutionOnly)
            Iterations = 0;
        else
        {
#if STABILITY
            LargestAfter = spLargestElement(Matrix);
            Roundoff = spRoundoff(Matrix, LargestAfter);
#endif
#if CONDITION
            StartTime = Time();
            ConditionNumber = spCondition(Matrix, InfNorm, &Error);
            ConditionTime = Time() - StartTime;
            spErrorMessage( Matrix, stderr, ProgramName );
#endif
#if PSEUDOCONDITION
            PseudoCondition = spPseudoCondition(Matrix);
#endif

            StartTime = Time();
            spPartition( Matrix, spDEFAULT_PARTITION );
            PartitionTime = Time() - StartTime;
        }

/* Solve system of equations Iterations times. */
        for(I = 1; I <= Iterations; I++)
        {   StartTime = Time();
            (void)spInitialize(Matrix, Init);
            BuildTime += Time() - StartTime;

            StartTime = Time();
            Error = spFactor( Matrix );
            FactorTime += Time() - StartTime;
            if (Error != spOKAY) spErrorMessage( Matrix, stderr, ProgramName );
            if (Error >= spFATAL) goto End;

            StartTime = Time();
/*
 * IMAG_VECTORS is a macro that replaces itself with `, iRHS, iSolution'
 * if the options spCOMPLEX and spSEPARATED_COMPLEX_VECTORS are set,
 * otherwise it disappears without a trace.
 */
#if TRANSPOSE
            if (Transposed)
                spSolveTransposed(Matrix, RHS, Solution IMAG_VECTORS );
            else
#endif
                spSolve( Matrix, RHS, Solution IMAG_VECTORS );
            SolveTime += Time() - StartTime;
        }

/* Print Solution. */
        if (SolutionOnly)
        {   if (PrintLimitSet)
                Last = MIN( PrintLimit, Size );
            else
                Last = Size;
            j = ' ';
        }
        else
        {   Last = (PrintLimit > Size) ? Size : PrintLimit;
            if (Last > 0) printf("Solution:\n");
            j = 'j';
        }

        if (Complex OR RealAsComplex)
        {
#if spSEPARATED_COMPLEX_VECTORS
            for (I = 1; I <= Last; I++)
            {   printf("%-16.9g   %-.9g%c\n", (double)Solution[I],
                                              (double)iSolution[I], j);
            }
#else
            for (I = 1; I <= Last; I++)
            {   printf("%-16.9g   %-.9g%c\n", (double)cSolution[I].Real,
                                              (double)cSolution[I].Imag, j);
            }
#endif
        }
        else
        {   for (I = 1; I <= Last; I++)
                printf("%-.9g\n", (double)Solution[I]);
        }
        if (Last < Size AND Last != 0)
            printf("Solution list truncated.\n");
        printf("\n");

#if DETERMINANT
/* Calculate determinant. */
        if (NOT SolutionOnly)
        {   StartTime = Time();
#if spCOMPLEX
            spDeterminant( Matrix, &Exponent, &Determinant, &iDeterminant );
#else
            spDeterminant( Matrix, &Exponent, &Determinant );
#endif
            DeterminantTime = Time() - StartTime;
            if (Complex OR RealAsComplex)
            {   Determinant = hypot( Determinant, iDeterminant );
                while (Determinant >= 10.0)
                {   Determinant *= 0.1;
                    Exponent++;
                }
            }
        }
#else
        Determinant = 0.0;   Exponent = 0;
#endif

#if MULTIPLICATION
        if (NOT SolutionOnly)
        {
/* Calculate difference between actual RHS vector and RHS vector
 * calculated from solution. */

/* Find the largest element in the given RHS vector. */
            MaxRHS = 0.0;

            if (Complex OR RealAsComplex)
            {
#if spSEPARATED_COMPLEX_VECTORS
                for (I = 1; I <= Size; I++)
                {   if (ABS(RHS[I]) > MaxRHS)
                        MaxRHS = ABS(RHS[I]);
                    if (ABS(iRHS[I]) > MaxRHS)
                        MaxRHS = ABS(iRHS[I]);
                }
#else
                for (I = 1; I <= Size; I++)
                {   if (ABS(cRHS[I].Real) > MaxRHS)
                        MaxRHS = ABS(cRHS[I].Real);
                    if (ABS(cRHS[I].Imag) > MaxRHS)
                        MaxRHS = ABS(cRHS[I].Imag);
                }
#endif
            }
            else
            {   for (I = 1; I <= Size; I++)
                {   if (ABS(RHS[I]) > MaxRHS)
                        MaxRHS = ABS(RHS[I]);
                }
            }

/* Rebuild matrix. */
            (void)spInitialize(Matrix, Init);
#if spCOMPLEX AND spSEPARATED_COMPLEX_VECTORS
            if (Transposed)
            {   spMultTransposed( Matrix, RHS_Verif, Solution,
                                          iRHS_Verif, iSolution);
            }
            else spMultiply(Matrix, RHS_Verif, Solution, iRHS_Verif, iSolution);
#else
            if (Transposed)
                spMultTransposed( Matrix, RHS_Verif, Solution );
            else
                spMultiply( Matrix, RHS_Verif, Solution );
#endif

/* Calculate residual. */
            Residual = 0.0;
            if (Complex OR RealAsComplex)
            {
#if spSEPARATED_COMPLEX_VECTORS
                for (I = 1; I <= Size; I++)
                {   Residual += ABS(RHS[I] - RHS_Verif[I])
                                   + ABS(iRHS[I] - iRHS_Verif[I]);
                }
#else
                for (I = 1; I <= Size; I++)
                {   Residual += ABS(cRHS[I].Real - cRHS_Verif[I].Real)
                                   + ABS(cRHS[I].Imag - cRHS_Verif[I].Imag);
                }
#endif
            }
            else
            {   for (I = 1; I <= Size; I++)
                    Residual += ABS(RHS[I] - RHS_Verif[I]);
            }
        }
#endif

/* Print statistics. */
        if (NOT SolutionOnly)
        {   Elements = spElementCount(Matrix);
            Fillins = spFillinCount(Matrix);
            
            printf("Initial factor time = %.2f.\n", InitialFactorTime);
            printf("Partition time = %.2f.\n", PartitionTime);
            if (Iterations > 0)
            {   printf("Build time = %.3f.\n", (BuildTime/Iterations));
                printf("Factor time = %.3f.\n",(FactorTime/Iterations));
                printf("Solve time = %.3f.\n", (SolveTime/Iterations));
            }
#if STABILITY
            printf("Condition time = %.2f.\n", ConditionTime);
#endif
#if DETERMINANT
            printf("Determinant time = %.2f.\n", DeterminantTime);
#endif
            printf("\nTotal number of elements = %d.\n", Elements);
            printf("Average number of elements per row initially = %.2f.\n",
                        (double)(Elements - Fillins) /
                        (double)spGetSize(Matrix, NO));
            printf("Total number of fill-ins = %d.\n", Fillins);
#if DETERMINANT OR MULTIPLICATION OR PSEUDOCONDITION OR CONDITION OR STABILITY
            putchar('\n');
#endif
#if STABILITY
            if (LargestBefore != 0.0)
                printf("Growth = %.2g.\n", LargestAfter / LargestBefore);
            printf("Max error in matrix = %.2g.\n", Roundoff);
#endif
#if STABILITY
            if(ABS(ConditionNumber) > 10 * SMALLEST_REAL);
                printf("Condition number = %.2g.\n", 1.0 / ConditionNumber);
#endif
#if CONDITION AND STABILITY
            printf("Estimated upper bound of error in solution = %.2g.\n",
                    Roundoff / ConditionNumber);
#endif
#if PSEUDOCONDITION
            printf("PseudoCondition = %.2g.\n", PseudoCondition);
#endif
#if DETERMINANT
            printf("Determinant = %.3g", (double)Determinant );
            if (Determinant != 0.0 AND Exponent != 0)
                printf("e%d", Exponent);
            putchar('.'); putchar('\n');
#endif
#if MULTIPLICATION
            if (MaxRHS != 0.0)
                printf("Normalized residual = %.2g.\n", (Residual / MaxRHS));
#endif
        }

End:;
        (void)fflush(stdout);
        CheckInAllComplexArrays();
        spDestroy(Matrix);
        Matrix = NULL;
    } while( --ArgCount > 0 );

    if (NOT SolutionOnly)
    {   printf("\nAggregate resource usage:\n");
        printf("    Time required = %.2f seconds.\n", Time() - BeginTime);
        printf("    Virtual memory used = %d kBytes.\n\n", ((int)sbrk(0))/1000);
    }
    exit (0);
}











/*
 *   READ MATRIX FROM FILE
 *
 *   This function reads the input file for the matrix and the RHS vector.
 *   If no RHS vector exists, one is created.  If there is an error in the
 *   file, the appropriate error messages are delivered to standard output.
 *
 *   >>> Returned:
 *   The error status is returned.  If no error occurred, a zero is returned.
 *   Otherwise, a one is returned.
 *
 *   >>> Local variables:
 *   pMatrixFile  (FILE *)
 *       The pointer to the file that holds the matrix.
 *   InputString  (char [])
 *       String variable for holding input from the matrix file.
 *   Message  (char [])
 *       String variable that contains a one line descriptor of the matrix.
 *       Descriptor is taken from matrix file.
 *
 *   >>> Global variables:
 *   Complex  <set>
 *   Size  <set>
 *   Element  <set>
 *   RHS  <set>
 *   iRHS  <set>
 */

static int
ReadMatrixFromFile()
{
long I, Reads;
FILE *pMatrixFile;
char  sInput[BUFSIZ], sType[BUFSIZ], *p;
int Error, Row, Col, Count = 0, LineNumber, RHS_Col, IntSize;
double Real, Imag = 0.0;
ComplexNumber *pValue, *pInitInfo, *CheckOutComplexArray();
RealNumber *pElement;
static char *EndOfFile = "%s: unexpected end of file `%s' at line %d.\n";
static char *Syntax = "%s: syntax error in file `%s' at line %d.\n";

/* Begin `ReadMatrixFromFile'. */

/* Open matrix file in read mode. */
    if (NOT SolutionOnly) putchar('\n');

    if (FileName == NULL)
    {   FileName = "standard input";
        pMatrixFile = stdin;
    }
    else
    {   pMatrixFile = fopen(FileName, "r");
        if (pMatrixFile == NULL)
        {   fprintf(stderr, "%s: file %s was not found.\n",
                    ProgramName, FileName);
            return 1;
        }
    }

    Complex = NO;
    LineNumber = 1;

/* Read and print label. */
    if (NULL == fgets( Message, BUFSIZ, pMatrixFile ))
    {   fprintf(stderr, EndOfFile, ProgramName, FileName, LineNumber);
        return 1;
    }

/* For compatibility with the old file syntax. */
    if (!strncmp( Message, "Starting", 8 ))
    {   /* Test for complex matrix. */
        if (strncmp( Message, "Starting complex", 15 ) == 0)
            Complex = YES;
        LineNumber++;
        if (NULL == fgets( Message, BUFSIZ, pMatrixFile ))
        {   fprintf(stderr, EndOfFile, ProgramName, FileName, LineNumber);
            return 1;
        }
    }
    if (NOT SolutionOnly) printf("%-s\n", Message);

/* Read size of matrix and determine type of matrix. */
    LineNumber++;
    if (NULL == fgets( sInput, BUFSIZ, pMatrixFile ))
    {   fprintf(stderr, EndOfFile, ProgramName, FileName, LineNumber);
        return 1;
    }
    if ((Reads = sscanf( sInput,"%d %s", &Size, sType )) < 1)
    {   fprintf(stderr, Syntax, ProgramName, FileName, LineNumber);
        return 1;
    }
    if (Reads == 2)
    {   for (p = sType; *p != '\0'; p++)
            if (isupper(*p)) *p += 'a'-'A';
        if (strncmp( sType, "complex", 7 ) == 0)
            Complex = YES;
        else if (strncmp( sType, "real", 7 ) == 0)
            Complex = NO;
        else
        {   fprintf(stderr, Syntax, ProgramName, FileName, LineNumber);
            return 1;
        }
    }
    EnlargeVectors( Size, YES );
    RHS_Col = MIN( Size, ColumnAsRHS );

#if NOT spCOMPLEX
    if (Complex)
    {   fprintf(stderr,
                    "%s: Sparse is not configured to solve complex matrices.\n",
                    ProgramName);
        fprintf(stderr,"    Enable spCOMPLEX in `spConfig.h'.\n");
        return 1;
    }
#endif
#if NOT REAL
    if (NOT (Complex OR RealAsComplex))
    {   fprintf(stderr,
                    "%s: Sparse is not configured to solve real matrices.\n",
                    ProgramName);
        fprintf(stderr,"    Enable REAL in `spConfig.h'.\n");
        return 1;
    }
#endif

/* Read matrix elements. */
    do
    {   if (Count == 0)
            pValue = CheckOutComplexArray( Count = 1000 );

        LineNumber++;
        if (NULL == fgets( sInput, BUFSIZ, pMatrixFile ))
        {   fprintf(stderr, "%s: unexpected end of file `%s' at line %d.\n",
                    ProgramName, FileName, LineNumber);
            return 1;
        }

        if (Complex)
        {   Reads = sscanf( sInput,"%d%d%lf%lf", &Row, &Col, &Real, &Imag );
            if (Reads != 4)
            {   fprintf(stderr, "%s: syntax error in file `%s' at line %d.\n",
                        ProgramName, FileName, LineNumber);
                return 1;
            }
        }
        else
        {   Reads = sscanf( sInput,"%d%d%lf", &Row, &Col, &Real );
            if (Reads != 3)
            {   fprintf(stderr, "%s: syntax error in file `%s' at line %d.\n",
                        ProgramName, FileName, LineNumber);
                return 1;
            }
        }
        if(Row < 0 OR Col < 0)
        {   fprintf(stderr, "%s: index not positive in file `%s' at line %d.\n",
                        ProgramName, FileName, LineNumber);
            return 1;
        }
        if(Row > Size OR Col > Size)
        {   if (NOT ExpansionWarningGiven)
            {   fprintf( stderr,
         "%s: computed and given matrix size differ in file `%s' at line %d.\n",
                        ProgramName, FileName, LineNumber);
                ExpansionWarningGiven = YES;
            }
            Size = MAX(Row, Col);
            EnlargeVectors( Size, NO );
        }
        pElement = spGetElement( Matrix, Row, Col );
        if (pElement == NULL)
        {   fprintf(stderr, "%s: insufficient memory available.\n",
                            ProgramName);
            exit(1);
        }
        pInitInfo = (ComplexNumber *)spGetInitInfo( pElement );
        if (pInitInfo == NULL)
        {   pValue[--Count].Real = Real;
            pValue[Count].Imag = Imag;
            spInstallInitInfo( pElement, (char *)(pValue + Count) );
        }
        else
        {   pInitInfo->Real += Real;
            pInitInfo->Imag += Imag;
        }

/* Save into RHS vector if in desired column. */
        if (Col == RHS_Col)
        {   if (Complex OR RealAsComplex)
            {
#if spSEPARATED_COMPLEX_VECTORS
                RHS[Row] = Real;
                iRHS[Row] = Imag;
#else
                cRHS[Row].Real = Real;
                cRHS[Row].Imag = Imag;
#endif
            }
            else RHS[Row] = Real;
        }
    } while (Row != 0 AND Col != 0);

    Size = spGetSize( Matrix, YES );
    if (Error = spErrorState( Matrix ) != spOKAY)
    {   spErrorMessage( Matrix, stderr, ProgramName );
        if( Error >= spFATAL ) return 1;
    }

/* Read RHS vector. */
    if (NOT UseColumnAsRHS AND (NULL != fgets( sInput, BUFSIZ, pMatrixFile )))
    {
/* RHS vector exists, read it. */
        LineNumber++;
        for (I = 1; I <= Size; I++)
        {   if (I != 1 OR (strncmp( sInput, "Beginning", 8 ) == 0))
            {   LineNumber++;
                if (NULL == fgets( sInput, BUFSIZ, pMatrixFile ))
                {   fprintf(stderr,
                            "%s: unexpected end of file `%s' at line %d.\n",
                            ProgramName, FileName, LineNumber);
                    return 1;
                }
            }

            if (Complex)
            {
#if spSEPARATED_COMPLEX_VECTORS
                Reads = sscanf( sInput,"%lf%lf", &RHS[I], &iRHS[I] );
#else
                Reads = sscanf( sInput, "%lf%lf", &cRHS[I].Real,
                                                  &cRHS[I].Imag );
#endif
                if (Reads != 2)
                {   fprintf(stderr,
                            "%s: syntax error in file `%s' at line %d.\n",
                            ProgramName, FileName, LineNumber);
                    return 1;
                }
            }
            else /* Not complex. */
            {   Reads = sscanf( sInput, "%lf", &RHS[I] );
                if (Reads != 1)
                {   fprintf(stderr,
                            "%s: syntax error in file `%s' at line %d.\n",
                            ProgramName, FileName, LineNumber);
                    return 1;
                }
            }
        }

        if (RealAsComplex AND NOT Complex)
        {
#if spSEPARATED_COMPLEX_VECTORS
            for (I = 1; I <= Size; I++) iRHS[I] = 0.0;
#else
            for (I = Size; I > 0; I--)
            {   cRHS[I].Real = RHS[I];
                cRHS[I].Imag = 0.0;
            }
#endif
        }
    }

/* Print out the size and the type of the matrix. */
    if (NOT SolutionOnly)
    {   IntSize = spGetSize( Matrix, NO );
        printf("Matrix is %d x %d ", IntSize, IntSize);
        if (IntSize != Size)
            printf("(external size is %d x %d) ", Size, Size);
        if (Complex OR RealAsComplex)
            printf("and complex.\n");
        else
            printf("and real.\n");
    }


    if (Complex OR RealAsComplex)
        spSetComplex( Matrix );
    else
        spSetReal( Matrix );

    (void)fclose(pMatrixFile);
    return 0;
}










/*
 *   INITIALIZE MATRIX ELEMENT
 *
 *   This function copys the InitInfo to the Real and Imag matrix element
 *   values.
 *
 *   >>> Returned:
 *   A zero is returns, signifying that no error can be made.
 */

/*ARGSUSED*/

static int
Init( RealNumber *pElement, char *pInitInfo, int Row, int Col )
{
/* Begin `Init'. */
    *pElement = *(RealNumber *)pInitInfo;
    if (Complex OR RealAsComplex)
        *(pElement+1) = *((RealNumber *)pInitInfo+1);
    return 0;
}








/*
 *  COMPLEX ARRAY ALLOCATION
 *
 *  These routines are used to check out and in arrays of complex numbers.
 */

static struct Bin {
    ComplexVector   Array;
    struct Bin     *Next;
}  *FirstArray, *CurrentArray = NULL;


static ComplexVector
CheckOutComplexArray( int Count )
{
struct Bin Bin, *pBin;
ComplexVector Temp;

/* Begin `CheckOutComplexArray'. */

    if (CurrentArray == NULL OR CurrentArray->Next == NULL)
    {   pBin = ALLOC( struct Bin, 1);
        Temp = ALLOC( ComplexNumber, Count );
        if (pBin == NULL OR Temp == NULL)
        {   fprintf( stderr, "%s: insufficient memory available.\n",
                             ProgramName);
            exit(1);
        }
        Bin.Array = Temp;
        Bin.Next = NULL;
        *pBin = Bin;
        if (CurrentArray == NULL)
            FirstArray = CurrentArray = pBin;
        else
        {   CurrentArray->Next = pBin;
            CurrentArray = pBin;
        }
    }
    else
    {   pBin = CurrentArray;
        CurrentArray = pBin->Next;
    }

    return pBin->Array;
}


static void
CheckInAllComplexArrays()
{

/* Begin `CheckInAllComplexArrays'. */
    if (CurrentArray != NULL)
        CurrentArray = FirstArray;
    return;
}





/*
 *  INTERPRET THE COMMAND LINE ARGUMENTS
 */

static int
InterpretCommandLine( int ArgCount, char *Args[] )
{
int I, FileCount = 0;
char *GetProgramName();

/* Begin `InterpretCommandLine'. */

/* Determine the name of the program. */
    ProgramName = GetProgramName(Args[0]);

/* Step through the argument list, interpreting and deleting the options. */
    for (I = 1; I < ArgCount; I++)
    {   if (!strcmp(Args[I], "-a"))
        {   if (ArgCount == I OR (sscanf(Args[I+1],"%lf", &AbsThreshold) != 1))
            {   AbsThreshold = 0.0;
                Usage(Args[I]);
            }
            else I++;
        }
        else if (!strcmp(Args[I], "-r"))
        {   if (ArgCount == I OR (sscanf(Args[I+1],"%lf", &RelThreshold) != 1))
            {   RelThreshold = 0.0;
                Usage(Args[I]);
            }
            else I++;
        }
        else if (!strcmp(Args[I], "-x"))
        {
#if spCOMPLEX
            RealAsComplex = YES;
#else
            fprintf(stderr,
                    "%s: Sparse is not configured to solve complex matrices.\n",
                    ProgramName);
            fprintf(stderr,"    Enable spCOMPLEX in `spConfig.h'.\n");
#endif
        }
        else if (!strcmp(Args[I], "-s"))
            SolutionOnly = YES;
        else if (!strcmp(Args[I], "-c"))
            DiagPivoting = NO;
        else if (!strcmp(Args[I], "-t"))
        {
#if TRANSPOSE
            Transposed = YES;
#else
            fprintf(stderr,
                   "%s: Sparse is not configured to solve transposed system.\n",
                    ProgramName);
            fprintf(stderr,"    Enable TRANSPOSE in `spConfig.h'.\n");
#endif
        }
        else if (!strcmp(Args[I], "-n"))
        {   if (ArgCount == I OR (sscanf(Args[I+1],"%d", &PrintLimit) != 1))
            {   PrintLimit = PRINT_LIMIT;
                Usage(Args[I]);
            }
            else
            {   PrintLimitSet = YES;
                I++;
            }
        }
        else if (!strcmp(Args[I], "-i"))
        {   if (ArgCount == I OR (sscanf(Args[I+1],"%d", &Iterations) != 1))
            {   Iterations = 1;
                Usage(Args[I]);
            }
            else I++;
        }
        else if (!strcmp(Args[I], "-b"))
        {   if (ArgCount == I OR (sscanf(Args[I+1],"%d", &ColumnAsRHS) != 1))
            {   ColumnAsRHS = 1;
                UseColumnAsRHS = YES;
            }
            else
            {   UseColumnAsRHS = YES;
                ColumnAsRHS = MAX( ColumnAsRHS, 1 );
                I++;
            }
        }
        else if (!strcmp(Args[I], "-p"))
        {
#if DOCUMENTATION
            CreatePlotFiles = YES;
#else
            fprintf(stderr,
                    "%s: Sparse is not configured to generate plot files.\n",
                    ProgramName);
            fprintf(stderr,"    Enable DOCUMENTATION in `spConfig.h'.\n");
#endif
        }
        else if (!strcmp(Args[I], "-u"))
            Usage( (char *)NULL );
        else if (Args[I][0] == '-')
            Usage(Args[I]);
        else Args[++FileCount] = Args[I];
    }

    return FileCount;
}







/*
 *  PROGRAM NAME
 *
 *  Removes path from argv[0] and returns program name.
 *  Assumes UNIX style path names.
 */

static char *
GetProgramName( char *Arg0 )
{
char *pTail;
/* Begin `GetProgramName'. */

#   ifdef vms
        return "sparse";
#   else
    {   pTail = Arg0 + strlen(Arg0)-1;
        while (pTail != Arg0 AND *(pTail-1) != '/')
            --pTail;
        return pTail;
    }
#   endif
}





/*
 *  USAGE WARNING
 *
 *  Sends a warning to the user when the command line syntax is wrong.
 */

static void
Usage( char *sBadOpt )
{
/* Begin `Usage'. */

    if (sBadOpt != NULL)
    {   fprintf(stderr, "%s: unknown or deformed option `%s'.\n",
                ProgramName, sBadOpt);
    }

    fprintf(stderr, "\nUsage: %s [options] [file names]\n\n", ProgramName);
    fprintf(stderr, "Options:\n");
    fprintf(stderr,
    "    -s      Print solution rather than run statistics.\n");
    fprintf( stderr, "    -r x    Use x as relative threshold.\n");
    fprintf( stderr, "    -a x    Use x as absolute threshold.\n");
    fprintf( stderr, "    -n n    Print first n terms of solution vector.\n");
    fprintf( stderr, "    -i n    Repeat build/factor/solve n times.\n");
    fprintf( stderr, "    -b n    Use n'th column of matrix as b in Ax=b.\n");
#if DIAGONAL_PIVOTING
    fprintf( stderr,
    "    -c      Use complete (as opposed to diagonal) pivoting.\n");
#endif
#if DOCUMENTATION
    fprintf( stderr,
    "    -p      Create plot files `filename.bef' and `filename.aft'.\n");
#endif
#if spCOMPLEX
    fprintf( stderr,
"    -x      Treat real matrix as complex with imaginary part zero.\n");
#endif
#if TRANSPOSE
    fprintf( stderr, "    -t      Solve transposed system.\n");
#endif
    fprintf( stderr, "    -u      Print usage message.\n");
    exit(1);
}






/*
 *  ENLARGE VECTORS
 *
 *  Allocate or enlarge vectors.
 */

static void
EnlargeVectors( int NewSize, int Clear )
{
int I, PrevSize = MaxSize;
RealVector OldRHS = RHS, iOldRHS = iRHS;
ComplexVector cOldRHS = cRHS;
#if spCOMPLEX
#   define SCALE 2
#else
#   define SCALE 1
#endif

/* Begin `EnlargeVectors'. */
    if (NewSize > PrevSize)
    {   if (MaxSize != 0)
        {   free( (char *)Solution );
            free( (char *)RHS_Verif );
        }
        RHS = ALLOC( RealNumber, SCALE*(NewSize+1) );
        Solution = ALLOC( RealNumber, SCALE*(NewSize+1) );
        RHS_Verif = ALLOC( RealNumber, SCALE*(NewSize+1) );
        if (NOT RHS OR NOT Solution OR NOT RHS_Verif)
        {   fprintf(stderr, "%s: insufficient memory available.\n",
                            ProgramName);
            exit(1);
        }
        cRHS = (ComplexVector)RHS;
        cSolution = (ComplexVector)Solution;
        cRHS_Verif = (ComplexVector)RHS_Verif;
        iRHS = RHS + NewSize + 1;
        iSolution = Solution + NewSize + 1;
        iRHS_Verif = RHS_Verif + NewSize + 1;

/* Copy data from old RHS to new RHS. */
        if (NOT Clear)
        {
/* Copy old RHS vector to new. */
            if (Complex OR RealAsComplex)
            {   for (I = PrevSize; I > 0; I--)
                {
#if spSEPARATED_COMPLEX_VECTORS OR LINT
                    RHS[I] = OldRHS[I];
                    iRHS[I] = iOldRHS[I];
#endif
#if (NOT spSEPARATED_COMPLEX_VECTORS) OR LINT
                    cRHS[I] = cOldRHS[I];
#endif
                }
            }
            else
            {   for (I = PrevSize; I > 0; I--)
                    RHS[I] = OldRHS[I];
            }
        }
        if (MaxSize != 0) free( (char *)OldRHS );
        MaxSize = NewSize;
    }

/* Either completely clear or clear remaining portion of RHS vector. */
    if ((NewSize > PrevSize) OR Clear)
    {   if (Clear)
        {   NewSize = MAX( NewSize, PrevSize );
            PrevSize = 0;
        }

        if (Complex OR RealAsComplex)
        {   for (I = NewSize; I > PrevSize; I--)
            {
#if spSEPARATED_COMPLEX_VECTORS OR LINT
                RHS[I] = 0.0;
                iRHS[I] = 0.0;
#endif
#if NOT spSEPARATED_COMPLEX_VECTORS OR LINT
                cRHS[I].Real = 0.0;
                cRHS[I].Imag = 0.0;
#endif
            }
        }
        else
        {   for (I = NewSize; I > PrevSize; I--)
                RHS[I] = 0.0;
        }
    }

    return;
}

