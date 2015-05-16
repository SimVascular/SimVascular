! Distributed under the GNU LGPL license from Sparse Linear Algebra Package.
!

subroutine gmresfortran (n,NNZ,IA,JA,m,F,u)
!&nunknown,&NNZ,IA,JA, M, b,u
!IMPLICIT NONE
!INTEGER*4:: NNZ,nunknown
!INTEGER*4,dimension(NNZ)::IA,JA
!REAL*8, dimension(NNZ)::xM
!REAL*8, dimension(nunknown)::b,u
!write(*,*)"hello fortran NNZ",NNZ
!write(*,*)"IA",IA
!write(*,*)"JA",JA
!write(*,*)"M",xM
!write(*,*)"b",b
!return


!GMRES(int *n, int *NNZ,int *IA,int *JA,double *M,double *F,double *u);
       IMPLICIT NONE
       INTEGER*4 :: n,nnz
        REAL*8, DIMENSION(nnz),intent(in)::m
       INTEGER*4, DIMENSION(nnz),intent(in)::IA,JA
      REAL*8, DIMENSION(n),intent(in)::F
        REAL*8, DIMENSION(n),intent(inout)::u

       integer ( kind = 4 ), parameter :: nsave = 20
       real ( kind = 8 ) err

      integer ( kind = 4 ) ierr
 ! integer ( kind = 4 ) igwk(ligw)
      integer ( kind = 4 ) isym
      integer ( kind = 4 ) iter
      integer ( kind = 4 ) itmax
      integer ( kind = 4 ) itol
      integer ( kind = 4 ) iunit
      integer ( kind = 4 ) lenw,leniw
      integer ( kind = 8 ), ALLOCATABLE, DIMENSION(:) :: iwork
      real ( kind = 8 ) , ALLOCATABLE, DIMENSION(:):: rwork
      real ( kind = 8 ) tol
        lenw = 700+n*20 +NNZ*20
       leniw=NNZ+4*n+32
       isym = 1
       itol = 1
       tol = 1D-07
       itmax = 5000
        iunit = 0

  ALLOCATE(rwork(lenw))
  ALLOCATE(iwork(leniw))

    ! call CG

     CALL DSDCG(n, F, u, NNZ, IA, JA, M, isym, itol, tol, &
           itmax, iter, err, ierr, iunit, rwork, lenw, iwork, leniw)

       write ( *, '(a)' ) 'CG is called '
       write ( *, '(a,i6)' ) '  Number of iterations:  ', iter
      write ( *, '(a,g14.6)' ) '  Error estimate ', err
      write ( *, '(a,i6)' ) '  Error code is ', ierr


      if (ierr==6) then
      write ( *, '(a)' ) 'Warning: Matrix is not positive definite. GMRES is called.'
      ! call GMRES
        itol=0
      CALL   DSLUGM(n, F, u, NNZ, IA, JA, M, isym, nsave, itol, tol, &
      itmax, iter, err, ierr, iunit, rwork, lenw, iwork, leniw )
      write ( *, '(a,i6)' ) '  Number of iterations:  ', iter
      write ( *, '(a,g14.6)' ) '  Error estimate ', err
      write ( *, '(a,i6)' ) '  Error code is ', ierr
      endif

    !  write (*,*)"solution u=",u
      return

end subroutine



      SUBROUTINE DBCG(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC, &
           MSOLVE, MTSOLV, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           R, Z, P, RR, ZZ, PP, DZ, RWORK, IWORK)
!***BEGIN PROLOGUE  DBCG
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DBCG-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition,  BiConjugate Gradient
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned BiConjugate Gradient Sparse Ax=b solver.
!            Routine to solve a Non-Symmetric linear system Ax = b
!            using the Preconditioned BiConjugate Gradient method.
!***DESCRIPTION
! *Usage:
!      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!      INTEGER ITER, IERR, IUNIT, IWORK(USER DEFINABLE)
!      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N), P(N)
!      DOUBLE PRECISION RR(N), ZZ(N), PP(N), DZ(N)
!      DOUBLE PRECISION RWORK(USER DEFINABLE)
!      EXTERNAL MATVEC, MTTVEC, MSOLVE, MTSOLV
!
!      CALL DBCG(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC,
!     $     MSOLVE, MTSOLV, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT,
!     $     R, Z, P, RR, ZZ, PP, DZ, RWORK, IWORK)
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See "Description", below for more
!         late breaking details...
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MATVEC :EXT      External.
!         Name of a routine which  performs the matrix vector multiply
!         operation  Y = A*X  given A and X.  The  name of  the MATVEC
!         routine must  be declared external  in the  calling program.
!         The calling sequence of MATVEC is:
!             CALL MATVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!         Where N is the number of unknowns, Y is the product A*X upon
!         return,  X is an input  vector.  NELT, IA,  JA,  A and  ISYM
!         define the SLAP matrix data structure: see Description,below.
! MTTVEC :EXT      External.
!         Name of a routine which performs the matrix transpose vector
!         multiply y = A'*X given A and X (where ' denotes transpose).
!         The name of the MTTVEC routine must be declared external  in
!         the calling program.  The calling sequence to MTTVEC is  the
!         same as that for MTTVEC, viz.:
!             CALL MTTVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!         Where N  is the number  of unknowns, Y is the   product A'*X
!         upon return, X is an input vector.  NELT, IA, JA, A and ISYM
!         define the SLAP matrix data structure: see Description,below.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R  for Z
!         given R with the preconditioning matrix M (M is supplied via
!         RWORK  and IWORK arrays).   The name  of  the MSOLVE routine
!         must be declared  external  in the  calling   program.   The
!         calling sequence of MSLOVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is  the right-hand side
!         vector, and Z is the solution upon return.  NELT,  IA, JA, A
!         and  ISYM define the SLAP  matrix  data structure: see
!         Description, below.  RWORK is a  double precision array that
!         can be used
!         to  pass   necessary  preconditioning     information and/or
!         workspace to MSOLVE.  IWORK is an integer work array for the
!         same purpose as RWORK.
! MTSOLV :EXT      External.                              T
!         Name of a routine which solves a linear system M ZZ = RR for
!         ZZ given RR with the preconditioning matrix M (M is supplied
!         via RWORK and IWORK arrays).  The name of the MTSOLV routine
!         must be declared external in the calling program.  The call-
!         ing sequence to MTSOLV is:
!            CALL MTSOLV(N, RR, ZZ, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, RR is the right-hand side
!         vector, and ZZ is the solution upon return.  NELT, IA, JA, A
!         and  ISYM define the SLAP  matrix  data structure: see
!         Description, below.  RWORK is a  double precision array that
!         can be used
!         to  pass   necessary  preconditioning     information and/or
!         workspace to MTSOLV.  IWORK is an integer work array for the
!         same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :WORK     Double Precision R(N).
! Z      :WORK     Double Precision Z(N).
! P      :WORK     Double Precision P(N).
! RR     :WORK     Double Precision RR(N).
! ZZ     :WORK     Double Precision ZZ(N).
! PP     :WORK     Double Precision PP(N).
! DZ     :WORK     Double Precision DZ(N).
! RWORK  :WORK     Double Precision RWORK(USER DEFINED).
!         Double Precision array that can be used for workspace in
!         MSOLVE and MTSOLV.
! IWORK  :WORK     Integer IWORK(USER DEFINED).
!         Integer array that can be used for workspace in MSOLVE
!         and MTSOLV.
!
! *Description
!       This routine does  not care  what matrix data   structure is
!       used for  A and M.  It simply   calls  the MATVEC and MSOLVE
!       routines, with  the arguments as  described above.  The user
!       could write any type of structure and the appropriate MATVEC
!       and MSOLVE routines.  It is assumed  that A is stored in the
!       IA, JA, A  arrays in some fashion and  that M (or INV(M)) is
!       stored  in  IWORK  and  RWORK   in  some fashion.   The SLAP
!       routines SDBCG and DSLUBC are examples of this procedure.
!
!       Two  examples  of  matrix  data structures  are the: 1) SLAP
!       Triad  format and 2) SLAP Column format.
!
!       =================== S L A P Triad format ===================
!       In  this   format only the  non-zeros are  stored.  They may
!       appear  in *ANY* order.   The user  supplies three arrays of
!       length NELT, where  NELT  is the number  of non-zeros in the
!       matrix:  (IA(NELT), JA(NELT),  A(NELT)).  For each  non-zero
!       the  user puts   the row  and  column index   of that matrix
!       element in the IA and JA arrays.  The  value of the non-zero
!       matrix  element is  placed in  the corresponding location of
!       the A  array.  This is  an extremely easy data  structure to
!       generate.  On  the other hand it  is  not too  efficient  on
!       vector  computers   for the  iterative  solution  of  linear
!       systems.  Hence, SLAP  changes this input  data structure to
!       the SLAP   Column  format for the  iteration (but   does not
!       change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *See Also:
!       SDBCG, DSLUBC
!***REFERENCES  (NONE)
!***ROUTINES CALLED  MATVEC, MTTVEC, MSOLVE, MTSOLV, ISDBCG,
!                    DCOPY, DDOT, DAXPY, D1MACH
!***END PROLOGUE  DBCG
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
      INTEGER ITER, IERR, IUNIT, IWORK(*)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N), P(N)
      DOUBLE PRECISION RR(N), ZZ(N), PP(N), DZ(N), RWORK(*)
      EXTERNAL MATVEC, MTTVEC, MSOLVE, MTSOLV
!
!  Check some of the input data.
!
!***FIRST EXECUTABLE STATEMENT  DBCG
      ITER = 0
      IERR = 0
      IF( N.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      FUZZ = epsilon ( fuzz )
      TOLMIN = 500.0*FUZZ
      FUZZ = FUZZ*FUZZ
      IF( TOL.LT.TOLMIN ) THEN
         TOL = TOLMIN
         IERR = 4
      end if
!
!  Calculate initial residual and pseudo-residual, and check
!  stopping criterion.
!
      CALL MATVEC(N, X, R, NELT, IA, JA, A, ISYM)

      R(1:n)  = B(1:n) - R(1:n)
      RR(1:n) = R(1:n)

      CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
      CALL MTSOLV(N, RR, ZZ, NELT, IA, JA, A, ISYM, RWORK, IWORK)

      IF( ISDBCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, RR, ZZ, PP, &
           DZ, RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 ) &
           GO TO 200
      IF( IERR.NE.0 ) RETURN
!
!  iteration loop
!
      DO 100 K=1,ITMAX

         ITER = K
!
!  Calculate coefficient BK and direction vectors P and PP.
!
         BKNUM = DDOT(N, Z, 1, RR, 1)
         IF( ABS(BKNUM).LE.FUZZ ) THEN
            IERR = 6
            RETURN
         end if
         IF(ITER .EQ. 1) THEN
            CALL DCOPY(N, Z, 1, P, 1)
            CALL DCOPY(N, ZZ, 1, PP, 1)
         ELSE
            BK = BKNUM/BKDEN
            DO 20 I = 1, N
               P(I) = Z(I) + BK*P(I)
               PP(I) = ZZ(I) + BK*PP(I)
 20         CONTINUE
         end if
         BKDEN = BKNUM
!
!  Calculate coefficient AK, new iterate X, new resids R and RR,
!  and new pseudo-resids Z and ZZ.
!
         CALL MATVEC(N, P, Z, NELT, IA, JA, A, ISYM)
         AKDEN = DDOT(N, PP, 1, Z, 1)
         AK = BKNUM/AKDEN
         IF( ABS(AKDEN).LE.FUZZ ) THEN
            IERR = 6
            RETURN
         end if
         CALL DAXPY(N, AK, P, 1, X, 1)
         CALL DAXPY(N, -AK, Z, 1, R, 1)
         CALL MTTVEC(N, PP, ZZ, NELT, IA, JA, A, ISYM)
         CALL DAXPY(N, -AK, ZZ, 1, RR, 1)
         CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
         CALL MTSOLV(N, RR, ZZ, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!
!  Check stopping criterion.
!
         IF( ISDBCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL, &
              ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, RR, ZZ, &
              PP, DZ, RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 ) &
              GO TO 200

 100  CONTINUE
!
!  Stopping criterion not satisfied.
!
      ITER = ITMAX + 1
      IERR = 2

 200  RETURN

      END
      SUBROUTINE DSDBCG(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSDBCG
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(SSDBCG-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Diagonally Scaled BiConjugate Gradient Sparse Ax=b solver.
!            Routine to solve a linear system  Ax = b  using the
!            BiConjugate Gradient method with diagonal scaling.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK(10), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(8*N)
!
!     CALL DSDBCG(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "Description",
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= 8*N.
! IWORK  :WORK     Integer IWORK(LENIW).
!         Used to hold pointers into the RWORK array.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.  LENIW >= 10.
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
!
! *Description:
!       This  routine performs  preconditioned  BiConjugate gradient
!       method on the Non-Symmetric positive definite  linear system
!       Ax=b. The preconditioner is M = DIAG(A), the diagonal of the
!       matrix   A.   This is the  simplest   of preconditioners and
!       vectorizes very well.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A) is  modified internally to
!       be   the SLAP  Column format.   See above.
!
! *See Also:
!       DBCG, DLUBCG
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DS2Y, DCHKW, DSDS, DBCG
!***END PROLOGUE  DSDBCG
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
      INTEGER IERR, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(N), TOL, ERR, RWORK(LENW)
      EXTERNAL DSMV, DSMTV, DSDI
      PARAMETER (LOCRB=1, LOCIB=11)
!
!         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
!***FIRST EXECUTABLE STATEMENT  DSDBCG
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Set up the workspace.  Compute the inverse of the
!         diagonal of the matrix.
      LOCIW = LOCIB

      LOCDIN = LOCRB
      LOCR = LOCDIN + N
      LOCZ = LOCR + N
      LOCP = LOCZ + N
      LOCRR = LOCP + N
      LOCZZ = LOCRR + N
      LOCPP = LOCZZ + N
      LOCDZ = LOCPP + N
      LOCW = LOCDZ + N
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSDBCG', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(4) = LOCDIN
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
      CALL DSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCDIN))
!
!         Perform the Diagonally Scaled BiConjugate gradient algorithm.
      CALL DBCG(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSMTV, &
           DSDI, DSDI, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           RWORK(LOCR), RWORK(LOCZ), RWORK(LOCP), &
           RWORK(LOCRR), RWORK(LOCZZ), RWORK(LOCPP), &
           RWORK(LOCDZ), RWORK(1), IWORK(1))
      RETURN
!------------- LAST LINE OF DSDBCG FOLLOWS ----------------------------
      END
      SUBROUTINE DSLUBC(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSLUBC
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(SSLUBC-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative incomplete LU Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Incomplete LU BiConjugate Gradient Sparse Ax=b solver.
!            Routine to solve a linear system  Ax = b  using the
!            BiConjugate Gradient  method  with  Incomplete   LU
!            decomposition preconditioning.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK(NEL+NU+4*N+2), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(NEL+NU+8*N)
!
!     CALL DSLUBC(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW)
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "Description",
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!         COMMON /SOLBLK/ SOLN( )
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
!           IERR = 7 => Incomplete factorization broke down
!                       and was fudged.  Resulting preconditioning may
!                       be less than the best.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.  NEL is the
!         number of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal).  NU is the number of nonzeros in the upper
!         triangle of the matrix (including the diagonal).
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= NEL+NU+8*N.
! IWORK  :WORK     Integer IWORK(LENIW).
!         Integer array used for workspace.  NEL is the number of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal).  NU is the number of nonzeros in the upper
!         triangle of the matrix (including the diagonal).
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.
!         LENIW >= NEL+NU+4*N+12.
!
! *Description:
!       This routine is simply a  driver for the DBCGN  routine.  It
!       calls the DSILUS routine to set  up the  preconditioning and
!       then  calls DBCGN with  the appropriate   MATVEC, MTTVEC and
!       MSOLVE, MTSOLV routines.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA,  A) is modified internally to
!       be the   SLAP Column  format.  See above.
!
! *See Also:
!       DBCG, SDBCG
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DS2Y, DCHKW, DSILUS, DBCG, DSMV, DSMTV
!***END PROLOGUE  DSLUBC
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
      INTEGER IERR, IUNIT, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(LENW)
      EXTERNAL DSMV, DSMTV, DSLUI, DSLUTI
      PARAMETER (LOCRB=1, LOCIB=11)
!
!         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
!***FIRST EXECUTABLE STATEMENT  DSLUBC
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Count number of Non-Zero elements preconditioner ILU matrix.
!         Then set up the work arrays.
      NL = 0
      NU = 0
      DO 20 ICOL = 1, N
!         Don't count diagonal.
         JBGN = JA(ICOL)+1
         JEND = JA(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
!VD$ NOVECTOR
            DO J = JBGN, JEND
               IF( IA(J).GT.ICOL ) THEN
                  NL = NL + 1
                  IF( ISYM.NE.0 ) NU = NU + 1
               ELSE
                  NU = NU + 1
               end if
            end do
         end if
 20   CONTINUE
!
      LOCIL = LOCIB
      LOCJL = LOCIL + N+1
      LOCIU = LOCJL + NL
      LOCJU = LOCIU + NU
      LOCNR = LOCJU + N+1
      LOCNC = LOCNR + N
      LOCIW = LOCNC + N
!
      LOCL = LOCRB
      LOCDIN = LOCL + NL
      LOCU = LOCDIN + N
      LOCR = LOCU + NU
      LOCZ = LOCR + N
      LOCP = LOCZ + N
      LOCRR = LOCP + N
      LOCZZ = LOCRR + N
      LOCPP = LOCZZ + N
      LOCDZ = LOCPP + N
      LOCW = LOCDZ + N
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSLUBC', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(1) = LOCIL
      IWORK(2) = LOCJL
      IWORK(3) = LOCIU
      IWORK(4) = LOCJU
      IWORK(5) = LOCL
      IWORK(6) = LOCDIN
      IWORK(7) = LOCU
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
!         Compute the Incomplete LU decomposition.
      CALL DSILUS( N, NELT, IA, JA, A, ISYM, NL, IWORK(LOCIL), &
           IWORK(LOCJL), RWORK(LOCL), RWORK(LOCDIN), NU, IWORK(LOCIU), &
           IWORK(LOCJU), RWORK(LOCU), IWORK(LOCNR), IWORK(LOCNC) )
!
!         Perform the incomplete LU preconditioned
!         BiConjugate Gradient algorithm.
      CALL DBCG(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSMTV, &
           DSLUI, DSLUTI, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           RWORK(LOCR), RWORK(LOCZ), RWORK(LOCP), &
           RWORK(LOCRR), RWORK(LOCZZ), RWORK(LOCPP), &
           RWORK(LOCDZ), RWORK, IWORK )
      RETURN
!------------- LAST LINE OF DSLUBC FOLLOWS ----------------------------
      END
      FUNCTION ISDBCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, &
           TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, RR, ZZ, PP, DZ, &
           RWORK, IWORK, AK, BK, BNRM, SOLNRM)
!***BEGIN PROLOGUE  ISDBCG
!***REFER TO  DBCG, DSDBCG, DSLUBC
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(ISDBCG-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Stop Test
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned BiConjugate Gradient Stop Test.
!            This routine calculates the stop test for the BiConjugate
!            Gradient iteration scheme.  It returns a nonzero if the
!            error estimate (the type of which is determined by ITOL)
!            is less than the user specified tolerance TOL.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
!     INTEGER  IERR, IUNIT, IWORK(USER DEFINED)
!     DOUBLE PRECISION B(N), X(N), A(N), TOL, ERR, R(N), Z(N), P(N)
!     DOUBLE PRECISION RR(N), ZZ(N), PP(N), DZ(N)
!     DOUBLE PRECISION RWORK(USER DEFINED), AK, BK, BNRM, SOLNRM
!     EXTERNAL MSOLVE
!
!     IF( ISDBCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, RR, ZZ, PP, DZ,
!    $     RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 )
!    $     THEN ITERATION DONE
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See "Description", in the SLAP
!         routine DBCG for more late breaking details...
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R  for Z
!         given R with the preconditioning matrix M (M is supplied via
!         RWORK  and IWORK arrays).   The name  of  the MSOLVE routine
!         must be declared  external  in the  calling   program.   The
!         calling sequence of MSLOVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is  the right-hand side
!         vector, and Z is the solution upon return.  NELT,  IA, JA, A
!         and  ISYM define the SLAP  matrix  data structure: see
!         Description, below.  RWORK is a  double precision array that
!         can be used
!         to  pass   necessary  preconditioning     information and/or
!         workspace to MSOLVE.  IWORK is an integer work array for the
!         same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than tol, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than tol) through a common block,
!         COMMON /SOLBLK/ SOLN( )
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than tol.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Error flag.  IERR is set to 3 if ITOL is not on of the
!         acceptable values, see above.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :IN       Double Precision R(N).
!         The residual r = b - Ax.
! Z      :WORK     Double Precision Z(N).
! P      :DUMMY    Double Precision P(N).
! RR     :DUMMY    Double Precision RR(N).
! ZZ     :DUMMY    Double Precision ZZ(N).
! PP     :DUMMY    Double Precision PP(N).
! DZ     :WORK     Double Precision DZ(N).
!         If ITOL.eq.0 then DZ is used to hold M-inv * B on the first
!         call.  If ITOL.eq.11 then DZ is used to hold X-SOLN.
! RWORK  :WORK     Double Precision RWORK(USER DEFINED).
!         Double Precision array that can be used for workspace in
!         MSOLVE and MTSOLV.
! IWORK  :WORK     Integer IWORK(USER DEFINED).
!         Integer array that can be used for workspace in MSOLVE
!         and MTSOLV.
! AK     :IN       Double Precision.
!         Current iterate BiConjugate Gradient iteration parameter.
! BK     :IN       Double Precision.
!         Current iterate BiConjugate Gradient iteration parameter.
! BNRM   :INOUT    Double Precision.
!         Norm of the right hand side.  Type of norm depends on ITOL.
!         Calculated only on the first call.
! SOLNRM :INOUT    Double Precision.
!         2-Norm of the true solution, SOLN.  Only computed and used
!         if ITOL = 11.
!
! *Function Return Values:
!       0 : Error estimate (determined by ITOL) is *NOT* less than the
!           specified tolerance, TOL.  The iteration must continue.
!       1 : Error estimate (determined by ITOL) is less than the
!           specified tolerance, TOL.  The iteration can be considered
!           complete.
!
! *Precision:           Double Precision
!***REFERENCES  (NONE)
!***ROUTINES CALLED  MSOLVE, DNRM2
!***COMMON BLOCKS    SOLBLK
!***END PROLOGUE  ISDBCG
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
      INTEGER ITER, IERR, IUNIT, IWORK(1)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N), P(N)
      DOUBLE PRECISION RR(N), ZZ(N), PP(N), DZ(N), RWORK(*)
      DOUBLE PRECISION AK, BK, BNRM, SOLNRM
      COMMON /SOLBLK/ SOLN(1)
      EXTERNAL MSOLVE
!
!***FIRST EXECUTABLE STATEMENT  ISDBCG
      ISDBCG = 0
!
      IF( ITOL.EQ.1 ) THEN
!         err = ||Residual||/||RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) BNRM = DNRM2(N, B, 1)
         ERR = DNRM2(N, R, 1)/BNRM
      ELSE IF( ITOL.EQ.2 ) THEN
!                  -1              -1
!         err = ||M  Residual||/||M  RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) THEN
            CALL MSOLVE(N, B, DZ, NELT, IA, JA, A, ISYM, RWORK, IWORK)
            BNRM = DNRM2(N, DZ, 1)
         end if
         ERR = DNRM2(N, Z, 1)/BNRM
      ELSE IF( ITOL.EQ.11 ) THEN
!         err = ||x-TrueSolution||/||TrueSolution|| (2-Norms).
         IF(ITER .EQ. 0) SOLNRM = DNRM2(N, SOLN, 1)

         DZ(1:n) = X(1:n) - SOLN(1:n)

         ERR = DNRM2(N, DZ, 1)/SOLNRM
      ELSE
!
!         If we get here ITOL is not one of the acceptable values.
         ERR = 1.0E10
         IERR = 3
      end if
!
      IF(IUNIT .NE. 0) THEN
         IF( ITER.EQ.0 ) THEN
            WRITE(IUNIT,1000) N, ITOL
         end if
         WRITE(IUNIT,1010) ITER, ERR, AK, BK
      end if
      IF(ERR .LE. TOL) ISDBCG = 1
!
      RETURN
 1000 FORMAT(' Preconditioned BiConjugate Gradient for N, ITOL = ', &
           I5,I5,/' ITER','   Error Estimate','            Alpha', &
           '             Beta')
 1010 FORMAT(1X,I4,1X,E16.7,1X,E16.7,1X,E16.7)
!------------- LAST LINE OF ISDBCG FOLLOWS ----------------------------
      END
      SUBROUTINE DCG(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ, &
           RWORK, IWORK )
!***BEGIN PROLOGUE  DCG
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DCG-D),
!             Symmetric Linear system, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned Conjugate Gradient iterative Ax=b solver.
!            Routine to  solve a  symmetric positive definite linear
!            system    Ax = b    using the Preconditioned  Conjugate
!            Gradient method.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER  ITER, IERR, IUNIT, IWORK(USER DEFINABLE)
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N)
!     DOUBLE PRECISION P(N), DZ(N), RWORK(USER DEFINABLE)
!     EXTERNAL MATVEC, MSOLVE
!
!     CALL DCG(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSLOVE,
!    $     ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ,
!    $     RWORK, IWORK )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Integer A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See ``Description'', below
!         for more late breaking details...
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MATVEC :EXT      External.
!         Name of a routine which performs the matrix vector multiply
!         Y = A*X given A and X.  The name of the MATVEC routine must
!         be declared external in the calling program.  The calling
!         sequence to MATVEC is:
!
!             CALL MATVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!
!         Where N is the number of unknowns, Y is the product A*X
!         upon return X is an input vector, NELT is the number of
!         non-zeros in the SLAP IA, JA, A storage for the matrix A.
!         ISYM is a flag which, if non-zero, denotest that A is
!         symmetric and only the lower or upper triangle is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R for
!         Z given R with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays).  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSLOVE is:
!
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!
!         Where N is the number of unknowns, R is the right-hand side
!         vector, and Z is the solution upon return.  RWORK is a double
!         precision
!         array that can be used to pass necessary preconditioning
!         information and/or workspace to MSOLVE.  IWORK is an integer
!         work array for the same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :WORK     Double Precision R(N).
! Z      :WORK     Double Precision Z(N).
! P      :WORK     Double Precision P(N).
! DZ     :WORK     Double Precision DZ(N).
! RWORK  :WORK     Double Precision RWORK(USER DEFINABLE).
!         Double Precision array that can be used by  MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINABLE).
!         Integer array that can be used by  MSOLVE.
!
! *Description
!       This routine does  not care  what matrix data   structure is
!       used for  A and M.  It simply   calls  the MATVEC and MSOLVE
!       routines, with  the arguments as  described above.  The user
!       could write any type of structure and the appropriate MATVEC
!       and MSOLVE routines.  It is assumed  that A is stored in the
!       IA, JA, A  arrays in some fashion and  that M (or INV(M)) is
!       stored  in  IWORK  and  RWORK   in  some fashion.   The SLAP
!       routines DSDCG and DSICCG are examples of this procedure.
!
!       Two  examples  of  matrix  data structures  are the: 1) SLAP
!       Triad  format and 2) SLAP Column format.
!
!       =================== S L A P Triad format ===================
!
!       In  this   format only the  non-zeros are  stored.  They may
!       appear  in *ANY* order.   The user  supplies three arrays of
!       length NELT, where  NELT  is the number  of non-zeros in the
!       matrix:  (IA(NELT), JA(NELT),  A(NELT)).  For each  non-zero
!       the  user puts   the row  and  column index   of that matrix
!       element in the IA and JA arrays.  The  value of the non-zero
!       matrix  element is  placed in  the corresponding location of
!       the A  array.  This is  an extremely easy data  structure to
!       generate.  On  the other hand it  is  not too  efficient  on
!       vector  computers   for the  iterative  solution  of  linear
!       systems.  Hence, SLAP  changes this input  data structure to
!       the SLAP   Column  format for the  iteration (but   does not
!       change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix      SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *See Also:
!       DSDCG, DSICCG
!***REFERENCES  1. Louis Hageman \& David Young, ``Applied Iterative
!                 Methods'', Academic Press, New York (1981) ISBN
!                 0-12-313340-8.
!
!               2. Concus, Golub \& O'Leary, ``A Generalized Conjugate
!                 Gradient Method for the Numerical Solution of
!                 Elliptic Partial Differential Equations,'' in Sparse
!                 Matrix Computations (Bunch \& Rose, Eds.), Academic
!                 Press, New York (1979).
!***ROUTINES CALLED  MATVEC, MSOLVE, ISDCG, DCOPY, DDOT, DAXPY, D1MACH
!***END PROLOGUE  DCG
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
      INTEGER IUNIT, IERR, IWORK(*)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N), P(N)
      DOUBLE PRECISION DZ(N), RWORK(*)
      EXTERNAL MATVEC, MSOLVE
!
!         Check some of the input data.
!***FIRST EXECUTABLE STATEMENT  DCG
      ITER = 0
      IERR = 0
      IF( N.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      TOLMIN = 500.0 * epsilon ( tolmin )
      IF( TOL.LT.TOLMIN ) THEN
         TOL = TOLMIN
         IERR = 4
      end if
!
!         Calculate initial residual and pseudo-residual, and check
!         stopping criterion.
      CALL MATVEC(N, X, R, NELT, IA, JA, A, ISYM)

      R(1:n) = B(1:n) - R(1:n)

      CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!
      IF( ISDCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ, &
           RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 ) GO TO 200
      IF( IERR.NE.0 ) RETURN
!
!         ***** Iteration loop *****
!
      DO 100 K=1,ITMAX
         ITER = K
!
!         Calculate coefficient bk and direction vector p.
         BKNUM = DDOT(N, Z, 1, R, 1)
         IF( BKNUM.LE.0.0D0 ) THEN
            IERR = 5
            RETURN
         end if
         IF(ITER .EQ. 1) THEN
            CALL DCOPY(N, Z, 1, P, 1)
         ELSE
            BK = BKNUM/BKDEN
            DO 20 I = 1, N
               P(I) = Z(I) + BK*P(I)
 20         CONTINUE
         end if
         BKDEN = BKNUM
!
!         Calculate coefficient ak, new iterate x, new residual r,
!         and new pseudo-residual z.
         CALL MATVEC(N, P, Z, NELT, IA, JA, A, ISYM)
         AKDEN = DDOT(N, P, 1, Z, 1)
         IF( AKDEN.LE.0.0D0 ) THEN
            IERR = 6
            RETURN
         end if
         AK = BKNUM/AKDEN
         CALL DAXPY(N, AK, P, 1, X, 1)
         CALL DAXPY(N, -AK, Z, 1, R, 1)
         CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!
!         check stopping criterion.
         IF( ISDCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL, &
              ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ, RWORK, &
              IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 ) GO TO 200
!
 100  CONTINUE
!
!         *****   end of loop  *****
!
!         stopping criterion not satisfied.
      ITER = ITMAX + 1
      IERR = 2
!
 200  RETURN
      END
      SUBROUTINE DSDCG(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSDCG
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSDCG-D),
!             Symmetric Linear system, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Diagonally Scaled Conjugate Gradient Sparse Ax=b Solver.
!            Routine to solve a  symmetric positive definite  linear
!            system Ax  =  b  using   the  Preconditioned  Conjugate
!            Gradient   method.    The preconditioner  is   diagonal
!            scaling.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK(10), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(5*N)
!
!     CALL DSDCG(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Integer A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See ``Description'',
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.  LENW >= 5*N.
! IWORK  :WORK     Integer IWORK(LENIW).
!         Used to hold pointers into the double precision workspace,
!         RWORK.  Upon return the following locations of IWORK hold
!         information which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.  LENIW >= 10.
!
! *Description:
!       This  routine   performs preconditioned conjugate   gradient
!       method on  the  symmetric positive definite   linear  system
!       Ax=b.   The preconditioner is  M = DIAG(A), the  diagonal of
!       the matrix A.  This is the  simplest of preconditioners  and
!       vectorizes very well.   This routine is  simply a driver for
!       the DCG routine.  It  calls the DSDS  routine to  set up the
!       preconditioning  and  then  calls  DCG  with the appropriate
!       MATVEC and MSOLVE routines.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A) is modified internally  to
!       be the   SLAP   Column format.  See above.
!
! *See Also:
!       DCG, DSICCG
!***REFERENCES  1. Louis Hageman \& David Young, ``Applied Iterative
!                 Methods'', Academic Press, New York (1981) ISBN
!                 0-12-313340-8.
!               2. Concus, Golub \& O'Leary, ``A Generalized Conjugate
!                 Gradient Method for the Numerical Solution of
!                 Elliptic Partial Differential Equations,'' in Sparse
!                 Matrix Computations (Bunch \& Rose, Eds.), Academic
!                 Press, New York (1979).
!***ROUTINES CALLED  DS2Y, DCHKW, DSDS, DCG
!***END PROLOGUE  DSDCG
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL
      INTEGER ITMAX, ITER, IERR, IUNIT, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(LENW)
      EXTERNAL DSMV, DSDI
      PARAMETER (LOCRB=1, LOCIB=11)
!
!         Modify the SLAP matrix data structure to YSMP-Column.
!***FIRST EXECUTABLE STATEMENT  DSDCG
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Set up the work arrays.
!         Compute the inverse of the diagonal of the matrix.  This
!         will be used as the preconditioner.
      LOCIW = LOCIB
!
      LOCD = LOCRB
      LOCR = LOCD + N
      LOCZ = LOCR + N
      LOCP = LOCZ + N
      LOCDZ = LOCP + N
      LOCW  = LOCDZ + N
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSDCG', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(4) = LOCD
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
      CALL DSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCD))
!
!         Do the Preconditioned Conjugate Gradient.
      CALL DCG(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSDI, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK(LOCR), &
           RWORK(LOCZ), RWORK(LOCP), RWORK(LOCDZ), RWORK, IWORK)
      RETURN
!------------- LAST LINE OF DSDCG FOLLOWS -----------------------------
      END
      SUBROUTINE DSICCG(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSICCG
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSICCG-D),
!             Symmetric Linear system, Sparse,
!             Iterative Precondition, Incomplete Cholesky
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Incomplete Cholesky Conjugate Gradient Sparse Ax=b Solver.
!            Routine to  solve a symmetric  positive definite linear
!            system   Ax    =  b  using  the    incomplete  Cholesky
!            Preconditioned Conjugate Gradient method.
!
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK(NEL+2*n+1), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(NEL+5*N)
!
!     CALL DSICCG(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Integer A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See ``Description'',
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
!           IERR = 7 => Incomplete factorization broke down
!                       and was fudged.  Resulting preconditioning may
!                       be less than the best.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.  NEL is the
!         number of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal)
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= NEL+5*N.
! IWORK  :WORK     Integer IWORK(LENIW).
!         Integer array used for workspace.  NEL is the number of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal).
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.  LENIW >= NEL+N+11.
!
! *Description:
!       This routine  performs  preconditioned  conjugate   gradient
!       method on the   symmetric positive  definite  linear  system
!       Ax=b.   The preconditioner  is  the incomplete Cholesky (IC)
!       factorization of the matrix A.  See  DSICS for details about
!       the incomplete   factorization algorithm.  One   should note
!       here however, that the  IC factorization is a  slow  process
!       and  that  one should   save  factorizations  for  reuse, if
!       possible.  The   MSOLVE operation (handled  in  DSLLTI) does
!       vectorize on machines  with  hardware  gather/scatter and is
!       quite fast.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A) is modified internally to be
!       the SLAP Column format.  See above.
!
! *See Also:
!       DCG, DSLLTI
!***REFERENCES  1. Louis Hageman \& David Young, ``Applied Iterative
!                 Methods'', Academic Press, New York (1981) ISBN
!                 0-12-313340-8.
!               2. Concus, Golub \& O'Leary, ``A Generalized Conjugate
!                 Gradient Method for the Numerical Solution of
!                 Elliptic Partial Differential Equations,'' in Sparse
!                 Matrix Computations (Bunch \& Rose, Eds.), Academic
!                 Press, New York (1979).
!***ROUTINES CALLED  DS2Y, DCHKW, DSICS, XERRWV, DCG
!***END PROLOGUE  DSICCG
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL
      INTEGER ITMAX, ITER, IUNIT, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(LENW)
      EXTERNAL DSMV, DSLLTI
      PARAMETER (LOCRB=1, LOCIB=11)
!
!         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
!***FIRST EXECUTABLE STATEMENT  DSICCG
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Count number of elements in lower triangle of the matrix.
!         Then set up the work arrays.
      IF( ISYM.EQ.0 ) THEN
         NEL = (NELT + N)/2
      ELSE
         NEL = NELT
      end if
!
      LOCJEL = LOCIB
      LOCIEL = LOCJEL + NEL
      LOCIW = LOCIEL + N + 1
!
      LOCEL = LOCRB
      LOCDIN = LOCEL + NEL
      LOCR = LOCDIN + N
      LOCZ = LOCR + N
      LOCP = LOCZ + N
      LOCDZ = LOCP + N
      LOCW = LOCDZ + N
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSICCG', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(1) = NEL
      IWORK(2) = LOCJEL
      IWORK(3) = LOCIEL
      IWORK(4) = LOCEL
      IWORK(5) = LOCDIN
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
!         Compute the Incomplete Cholesky decomposition.
!
      CALL DSICS(N, NELT, IA, JA, A, ISYM, NEL, IWORK(LOCIEL), &
           IWORK(LOCJEL), RWORK(LOCEL), RWORK(LOCDIN), &
           RWORK(LOCR), IERR )
      IF( IERR.NE.0 ) THEN
         CALL XERRWV('DSICCG: Warning...IC factorization broke down '// &
              'on step i1.  Diagonal was set to unity and '// &
              'factorization proceeded.', 113, 1, 1, 1, IERR, 0, &
              0, 0.0, 0.0 )
         IERR = 7
      end if
!
!         Do the Preconditioned Conjugate Gradient.
      CALL DCG(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSLLTI, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK(LOCR), &
           RWORK(LOCZ), RWORK(LOCP), RWORK(LOCDZ), RWORK(1), &
           IWORK(1))
      RETURN
!------------- LAST LINE OF DSICCG FOLLOWS ----------------------------
      END
      FUNCTION ISDCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, &
           TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ, &
           RWORK, IWORK, AK, BK, BNRM, SOLNRM)
!***BEGIN PROLOGUE  ISDCG
!***REFER TO  DCG, DSDCG, DSICCG
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(ISDCG-D),
!             Linear system, Sparse, Stop Test
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned Conjugate Gradient Stop Test.
!            This routine calculates the stop test for the Conjugate
!            Gradient iteration scheme.  It returns a nonzero if the
!            error estimate (the type of which is determined by ITOL)
!            is less than the user specified tolerance TOL.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
!     INTEGER IERR, IUNIT, IWORK(USER DEFINED)
!     DOUBLE PRECISION B(N), X(N), A(N), TOL, ERR, R(N), Z(N)
!     DOUBLE PRECISION P(N), DZ(N), RWORK(USER DEFINED), AK, BK
!     DOUBLE PRECISION BNRM, SOLNRM
!     EXTERNAL MSOLVE
!
!     IF( ISDCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ, RWORK, IWORK,
!    $     AK, BK, BNRM, SOLNRM) .NE. 0 ) THEN ITERATION DONE
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :IN       Double Precision X(N).
!         The current approximate solution vector.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See ``Description''
!         in the DCG, DSDCG or DSICCG routines.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R for
!         Z given R with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays).  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSLOVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is the right-hand side
!         vector, and Z is the solution upon return.  RWORK is a double
!         precision
!         array that can be used to pass necessary preconditioning
!         information and/or workspace to MSOLVE.  IWORK is an integer
!         work array for the same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than tol, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the ``exact''
!         solution or a very accurate approximation (one with an error
!         much less than tol) through a common block,
!         COMMON /SOLBLK/ SOLN( )
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than tol.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :IN       Integer.
!         The iteration for which to check for convergence.
! ERR    :OUT      Double Precision.
!         Error estimate of error in the X(N) approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Error flag.  IERR is set to 3 if ITOL is not on of the
!         acceptable values, see above.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :IN       Double Precision R(N).
!         The residual R = B-AX.
! Z      :WORK     Double Precision Z(N).
!         Workspace used to hold the pseudo-residual M Z = R.
! P      :IN       Double Precision P(N).
!         The conjugate direction vector.
! DZ     :WORK     Double Precision DZ(N).
!         Workspace used to hold temporary vector(s).
! RWORK  :WORK     Double Precision RWORK(USER DEFINABLE).
!         Double Precision array that can be used by MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINABLE).
!         Integer array that can be used by MSOLVE.
! BNRM   :INOUT    Double Precision.
!         Norm of the right hand side.  Type of norm depends on ITOL.
!         Calculated only on the first call.
! SOLNRM :INOUT    Double Precision.
!         2-Norm of the true solution, SOLN.  Only computed and used
!         if ITOL = 11.
!
! *Function Return Values:
!       0 : Error estimate (determined by ITOL) is *NOT* less than the
!           specified tolerance, TOL.  The iteration must continue.
!       1 : Error estimate (determined by ITOL) is less than the
!           specified tolerance, TOL.  The iteration can be considered
!           complete.
!
! *Precision:           Double Precision
! *See Also:
!       DCG, DSDCG, DSICCG
!
! *Cautions:
!     This routine will attempt to write to the fortran logical output
!     unit IUNIT, if IUNIT .ne. 0.  Thus, the user must make sure that
!     this  logical  unit  must  be  attached  to  a  file or terminal
!     before calling this routine with a non-zero  value  for   IUNIT.
!     This routine does not check for the validity of a non-zero IUNIT
!     unit number.
!***REFERENCES  (NONE)
!***ROUTINES CALLED  MSOLVE, DNRM2
!***COMMON BLOCKS    SOLBLK
!***END PROLOGUE  ISDCG
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
      INTEGER ITER, IERR, IUNIT, IWORK(*)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N)
      DOUBLE PRECISION Z(N), P(N), DZ(N), RWORK(*)
      EXTERNAL MSOLVE
      COMMON /SOLBLK/ SOLN(1)
!
!***FIRST EXECUTABLE STATEMENT  ISDCG
      ISDCG = 0
!
      IF( ITOL.EQ.1 ) THEN
!         err = ||Residual||/||RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) BNRM = DNRM2(N, B, 1)
         ERR = DNRM2(N, R, 1)/BNRM
      ELSE IF( ITOL.EQ.2 ) THEN
!                  -1              -1
!         err = ||M  Residual||/||M  RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) THEN
            CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
            BNRM = DNRM2(N, DZ, 1)
         end if
         ERR = DNRM2(N, Z, 1)/BNRM
      ELSE IF( ITOL.EQ.11 ) THEN
!         err = ||x-TrueSolution||/||TrueSolution|| (2-Norms).
         IF(ITER .EQ. 0) SOLNRM = DNRM2(N, SOLN, 1)

         DZ(1:n) = X(1:n) - SOLN(1:n)

         ERR = DNRM2(N, DZ, 1)/SOLNRM
      ELSE
!
!         If we get here ITOL is not one of the acceptable values.
         ERR = 1.0E10
         IERR = 3
      end if
!
      IF(IUNIT .NE. 0) THEN
         IF( ITER.EQ.0 ) THEN
            WRITE(IUNIT,1000) N, ITOL
         end if
         WRITE(IUNIT,1010) ITER, ERR, AK, BK
      end if
      IF(ERR .LE. TOL) ISDCG = 1
      RETURN
 1000 FORMAT(' Preconditioned Conjugate Gradient for ', &
           'N, ITOL = ',I5, I5, &
           /' ITER','   Error Estimate','            Alpha', &
           '             Beta')
 1010 FORMAT(1X,I4,1X,E16.7,1X,E16.7,1X,E16.7)
!------------- LAST LINE OF ISDCG FOLLOWS ------------------------------
      END
      SUBROUTINE DCGN(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC, &
           MSOLVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, &
           ATP, ATZ, DZ, ATDZ, RWORK, IWORK)
!***BEGIN PROLOGUE  DCGN
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DCGN-D),
!             Non-Symmetric Linear system solve, Sparse,
!             Iterative Precondition, Normal Equations.
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned CG Sparse Ax=b Solver for Normal Equations.
!            Routine  to solve a general linear system Ax = b using the
!            Preconditioned Conjugate Gradient method  applied to   the
!            normal equations AA'y = b, x=A'y.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER  ITER, IERR, IUNIT, IWORK(USER DEFINABLE)
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N)
!     DOUBLE PRECISION P(N), ATP(N), ATZ(N), DZ(N), ATDZ(N)
!     DOUBLE PRECISION RWORK(USER DEFINABLE)
!     EXTERNAL MATVEC, MTTVEC, MSOLVE
!
!     CALL DCGN(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC,
!    $     MSOLVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R,
!    $     Z, P, ATP, ATZ, DZ, ATDZ, RWORK, IWORK)
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See "Description", below
!         for more late breaking details...
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MATVEC :EXT      External.
!         Name of a routine which performs the matrix vector multiply
!         y = A*X given A and X.  The name of the MATVEC routine must
!         be declared external in the calling program.  The calling
!         sequence to MATVEC is:
!             CALL MATVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!         Where N is the number of unknowns, Y is the product A*X
!         upon return X is an input vector, NELT is the number of
!         non-zeros in the SLAP-Column IA, JA, A storage for the matrix
!         A.  ISYM is a flag which, if non-zero, denotes that A is
!         symmetric and only the lower or upper triangle is stored.
! MTTVEC :EXT      External.
!         Name of a routine which performs the matrix transpose vector
!         multiply y = A'*X given A and X (where ' denotes transpose).
!         The name of the MTTVEC routine must be declared external in
!         the calling program.  The calling sequence to MTTVEC is the
!         same as that for MATVEC, viz.:
!             CALL MTTVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!         Where N is the number of unknowns, Y is the product A'*X
!         upon return X is an input vector, NELT is the number of
!         non-zeros in the SLAP-Column IA, JA, A storage for the matrix
!         A.  ISYM is a flag which, if non-zero, denotes that A is
!         symmetric and only the lower or upper triangle is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R for
!         Z given R with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays).  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSOLVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is the right-hand side
!         vector, and Z is the solution upon return.  RWORK is a
!         double precision
!         array that can be used to pass necessary preconditioning
!         information and/or workspace to MSOLVE.  IWORK is an integer
!         work array for the same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :WORK     Double Precision R(N).
! Z      :WORK     Double Precision Z(N).
! P      :WORK     Double Precision P(N).
! ATP    :WORK     Double Precision ATP(N).
! ATZ    :WORK     Double Precision ATZ(N).
! DZ     :WORK     Double Precision DZ(N).
! ATDZ   :WORK     Double Precision ATDZ(N).
! RWORK  :WORK     Double Precision RWORK(USER DEFINABLE).
!         Double Precision array that can be used by  MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINABLE).
!         Integer array that can be used by  MSOLVE.
!
! *Description:
!       This  routine applies the  preconditioned conjugate gradient
!       (PCG) method to a non-symmetric system of equations Ax=b. To
!       do this the normal equations are solved:
!               AA' y  = b, where  x  = A'y.
!       In PCG method the iteration count is determined by condition
!                               -1
!       number of the  matrix (M  A).   In the  situation where  the
!       normal equations are  used  to solve a  non-symmetric system
!       the condition number depends on  AA' and should therefore be
!       much worse than that of A.  This is the conventional wisdom.
!       When one has a good preconditioner for AA' this may not hold.
!       The latter is the situation when DCGN should be tried.
!
!       If one is trying to solve  a symmetric system, SCG should be
!       used instead.
!
!       This routine does  not care  what matrix data   structure is
!       used for  A and M.  It simply   calls  the MATVEC and MSOLVE
!       routines, with  the arguments as  described above.  The user
!       could write any type of structure and the appropriate MATVEC
!       and MSOLVE routines.  It is assumed  that A is stored in the
!       IA, JA, A  arrays in some fashion and  that M (or INV(M)) is
!       stored  in  IWORK  and  RWORK)  in  some fashion.   The SLAP
!       routines SSDCGN and SSLUCN are examples of this procedure.
!
!       Two  examples  of  matrix  data structures  are the: 1) SLAP
!       Triad  format and 2) SLAP Column format.
!
!       =================== S L A P Triad format ===================
!
!       In  this   format only the  non-zeros are  stored.  They may
!       appear  in *ANY* order.   The user  supplies three arrays of
!       length NELT, where  NELT  is the number  of non-zeros in the
!       matrix:  (IA(NELT), JA(NELT),  A(NELT)).  For each  non-zero
!       the  user puts   the row  and  column index   of that matrix
!       element in the IA and JA arrays.  The  value of the non-zero
!       matrix  element is  placed in  the corresponding location of
!       the A  array.  This is  an extremely easy data  structure to
!       generate.  On  the other hand it  is  not too  efficient  on
!       vector  computers   for the  iterative  solution  of  linear
!       systems.  Hence, SLAP  changes this input  data structure to
!       the SLAP   Column  format for the  iteration (but   does not
!       change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *See Also:
!         DSDCGN, DSLUCN, ISDCGN
!***REFERENCES  (NONE)
!***ROUTINES CALLED  MATVEC, MTTVEC, MSOLVE, ISDCGN,
!                    DCOPY, DDOT, DAXPY, D1MACH
!***END PROLOGUE  DCGN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
      INTEGER IUNIT, IWORK(*)
      DOUBLE PRECISION B(N), X(N), A(N), R(N), Z(N), P(N)
      DOUBLE PRECISION ATP(N), ATZ(N), DZ(N), ATDZ(N), RWORK(*)
      EXTERNAL MATVEC, MTTVEC, MSOLVE
!
!         Check user input.
!***FIRST EXECUTABLE STATEMENT  DCGN
      ITER = 0
      IERR = 0
      IF( N.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      TOLMIN = 500.0 * epsilon ( tolmin )
      IF( TOL.LT.TOLMIN ) THEN
         TOL = TOLMIN
         IERR = 4
      end if
!         Calculate initial residual and pseudo-residual, and check
!         stopping criterion.
      CALL MATVEC(N, X, R, NELT, IA, JA, A, ISYM)

      R(1:n) = B(1:n) - R(1:n)

      CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
      CALL MTTVEC(N, Z, ATZ, NELT, IA, JA, A, ISYM)
!
      IF( ISDCGN(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC, MSOLVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, ATP, ATZ, &
           DZ, ATDZ, RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 ) &
           GO TO 200
      IF( IERR.NE.0 ) RETURN
!
!         ***** iteration loop *****
!
      DO 100 K=1,ITMAX
         ITER = K
!
!         Calculate coefficient BK and direction vector P.
         BKNUM = DDOT(N, Z, 1, R, 1)
         IF( BKNUM.LE.0.0D0 ) THEN
            IERR = 6
            RETURN
         end if
         IF(ITER .EQ. 1) THEN
            CALL DCOPY(N, Z, 1, P, 1)
         ELSE
            BK = BKNUM/BKDEN
            DO 20 I = 1, N
               P(I) = Z(I) + BK*P(I)
 20         CONTINUE
         end if
         BKDEN = BKNUM
!
!         Calculate coefficient AK, new iterate X, new residual R,
!         and new pseudo-residual ATZ.
         IF(ITER .NE. 1) CALL DAXPY(N, BK, ATP, 1, ATZ, 1)
         CALL DCOPY(N, ATZ, 1, ATP, 1)
         AKDEN = DDOT(N, ATP, 1, ATP, 1)
         IF( AKDEN.LE.0.0D0 ) THEN
            IERR = 6
            RETURN
         end if
         AK = BKNUM/AKDEN
         CALL DAXPY(N, AK, ATP, 1, X, 1)
         CALL MATVEC(N, ATP, Z, NELT, IA, JA, A, ISYM)
         CALL DAXPY(N, -AK, Z, 1, R, 1)
         CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
         CALL MTTVEC(N, Z, ATZ, NELT, IA, JA, A, ISYM)
!
!         check stopping criterion.
         IF( ISDCGN(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC, &
              MSOLVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, &
              Z, P, ATP, ATZ, DZ, ATDZ, RWORK, IWORK, AK, BK, BNRM, &
              SOLNRM) .NE. 0) GOTO 200
!
 100  CONTINUE
!
!         *****   end of loop  *****
!
!         stopping criterion not satisfied.
      ITER = ITMAX + 1
!
 200  RETURN
!------------- LAST LINE OF DCGN FOLLOWS ----------------------------
      END
      SUBROUTINE DSDCGN(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSDCGN
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(SSDCGN-D),
!             Non-Symmetric Linear system solve, Sparse,
!             Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Diagonally Scaled CG Sparse Ax=b Solver for Normal Eqn's.
!            Routine to solve a general linear system Ax = b using
!            diagonal scaling with the Conjugate  Gradient  method
!            applied to the the normal equations, viz.,  AA'y = b,
!            where x = A'y.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK, LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(8*N)
!
!     CALL DSDCGN(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW)
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "Description",
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= 8*N.
! IWORK  :WORK     Integer IWORK(LENIW).
!         Used to hold pointers into the RWORK array.
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.  LENIW >= 10.
!
! *Description:
!       This  routine is simply a driver  for the  DCGN routine.  It
!       calls the   DSD2S  routine to set up the preconditioning and
!       then calls DCGN with the appropriate   MATVEC  and    MSOLVE
!       routines.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix      SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A) is modified internally to be
!       the SLAP Column format.  See above.
!
! *See Also:
!       DCGN, DSD2S, DSMV, DSMTV, DSDI
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DS2Y, DCHKW, DSD2S, DCGN, DSMV, DSMTV, DSDI
!***END PROLOGUE  DSDCGN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL
      INTEGER ITMAX, ITER, IERR, IUNIT, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(LENW)
      EXTERNAL DSMV, DSMTV, DSDI
      PARAMETER (LOCRB=1, LOCIB=11)
!
!         Modify the SLAP matrix data structure to YSMP-Column.
!***FIRST EXECUTABLE STATEMENT  DSDCGN
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Set up the work arrays.
!         Compute the inverse of the diagonal of AA'.  This will be
!         used as the preconditioner.
      LOCIW = LOCIB
!
      LOCD = LOCRB
      LOCR = LOCD + N
      LOCZ = LOCR + N
      LOCP = LOCZ + N
      LOCATP = LOCP + N
      LOCATZ = LOCATP + N
      LOCDZ = LOCATZ + N
      LOCATD = LOCDZ + N
      LOCW = LOCATD + N
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSDCGN', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(4) = LOCD
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
      CALL DSD2S(N, NELT, IA, JA, A, ISYM, RWORK(1))
!
!         Perform Conjugate Gradient algorithm on the normal equations.
      CALL DCGN( N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSMTV, DSDI, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK(LOCR), &
           RWORK(LOCZ), RWORK(LOCP), RWORK(LOCATP), RWORK(LOCATZ), &
           RWORK(LOCDZ), RWORK(LOCATD), RWORK, IWORK )
!
      IF( ITER.GT.ITMAX ) IERR = 2
      RETURN
!------------- LAST LINE OF DSDCGN FOLLOWS ----------------------------
      END
      SUBROUTINE DSLUCN(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSLUCN
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(SSLUCN-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Incomplete LU Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Incomplete LU CG Sparse Ax=b Solver for Normal Equations.
!            Routine to solve  a general linear system Ax = b using the
!            incomplete  LU decomposition  with  the Conjugate Gradient
!            method  applied to the normal equations,  viz., AA'y =  b,
!            x=A'y.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK(NEL+NU+4*N+2), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(NEL+NU+8*N)
!
!     CALL DSLUCN(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "Description",
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
!           IERR = 7 => Incomplete factorization broke down
!                       and was fudged.  Resulting preconditioning may
!                       be less than the best.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.  NEL is the number
!         of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal).  NU is the number of nonzeros in the upper
!         triangle of the matrix (including the diagonal).
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= NEL+NU+8*N.
! IWORK  :WORK     Integer IWORK(LENIW).
!         Integer array used for workspace.  NEL is the number of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal).  NU is the number of nonzeros in the upper
!         triangle of the matrix (including the diagonal).
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.  LENIW >=
!         NEL+NU+4*N+12.
!
! *Description:
!       This  routine is simply a driver  for the  DCGN  routine.    It
!       calls the DSILUS routine to set up the preconditioning and then
!       calls DCGN with the appropriate  MATVEC  and  MSOLVE  routines.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A) is modified internally to be
!       the SLAP Column format.  See above.
!
! *See Also:
!       DCGN, SDCGN, DSILUS
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DS2Y, DSILUS, DCHKW, DSMV, DSMTV, DSMMTI, DCGN
!***END PROLOGUE  DSLUCN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
      INTEGER IERR, IUNIT, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(LENW)
      PARAMETER (LOCRB=1, LOCIB=11)
!
      EXTERNAL DSMV, DSMTV, DSMMTI
!
!         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
!***FIRST EXECUTABLE STATEMENT  DSLUCN
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Count number of Non-Zero elements preconditioner ILU matrix.
!         Then set up the work arrays.
      NL = 0
      NU = 0
      DO 20 ICOL = 1, N
!         Don't count diagional.
         JBGN = JA(ICOL)+1
         JEND = JA(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
!VD$ NOVECTOR
            DO J = JBGN, JEND
               IF( IA(J).GT.ICOL ) THEN
                  NL = NL + 1
                  IF( ISYM.NE.0 ) NU = NU + 1
               ELSE
                  NU = NU + 1
               end if
            end do
         end if
 20   CONTINUE
!
      LOCIL = LOCIB
      LOCJL = LOCIL + N+1
      LOCIU = LOCJL + NL
      LOCJU = LOCIU + NU
      LOCNR = LOCJU + N+1
      LOCNC = LOCNR + N
      LOCIW = LOCNC + N
!
      LOCL = LOCRB
      LOCDIN = LOCL + NL
      LOCU = LOCDIN + N
      LOCR = LOCU + NU
      LOCZ = LOCR + N
      LOCP = LOCZ + N
      LOCATP = LOCP + N
      LOCATZ = LOCATP + N
      LOCDZ = LOCATZ + N
      LOCATD = LOCDZ + N
      LOCW = LOCATD + N
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSLUCN', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(1) = LOCIL
      IWORK(2) = LOCJL
      IWORK(3) = LOCIU
      IWORK(4) = LOCJU
      IWORK(5) = LOCL
      IWORK(6) = LOCDIN
      IWORK(7) = LOCU
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
!         Compute the Incomplete LU decomposition.
      CALL DSILUS( N, NELT, IA, JA, A, ISYM, NL, IWORK(LOCIL), &
           IWORK(LOCJL), RWORK(LOCL), RWORK(LOCDIN), NU, IWORK(LOCIU), &
           IWORK(LOCJU), RWORK(LOCU), IWORK(LOCNR), IWORK(LOCNC) )
!
!         Perform Conjugate Gradient algorithm on the normal equations.
      CALL DCGN(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSMTV, DSMMTI, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK(LOCR), &
           RWORK(LOCZ), RWORK(LOCP), RWORK(LOCATP), RWORK(LOCATZ), &
           RWORK(LOCDZ), RWORK(LOCATD), RWORK, IWORK )
!
      IF( ITER.GT.ITMAX ) IERR = 2
      RETURN
!------------- LAST LINE OF DSLUCN FOLLOWS ----------------------------
      END
      FUNCTION ISDCGN(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC, &
           MSOLVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, &
           P, ATP, ATZ, DZ, ATDZ, RWORK, IWORK, AK, BK, BNRM, SOLNRM)
!***BEGIN PROLOGUE  ISDCGN
!***REFER TO  DCGN, DSDCGN, DSLUCN
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(ISDCGN-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Normal Equations
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned CG on Normal Equations Stop Test.
!            This routine calculates the stop test for the Conjugate
!            Gradient iteration   scheme applied  to     the  normal
!            equations.  It returns a nonzero  if the error estimate
!            (the type of which is determined by  ITOL) is less than
!            the user specified tolerance TOL.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
!     INTEGER  IERR, IUNIT, IWORK(USER DEFINED)
!     DOUBLE PRECISION B(N), X(N), A(N), TOL, ERR, R(N), Z(N), P(N)
!     DOUBLE PRECISION ATP(N), ATZ(N), DZ(N), ATDZ(N)
!     DOUBLE PRECISION RWORK(USER DEFINED), AK, BK, BNRM, SOLNRM
!     EXTERNAL MATVEC, MTTVEC, MSOLVE
!
!     IF( ISTPCGN(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC,
!    $     MSOLVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P,
!    $     ATP, ATZ, DZ, ATDZ, RWORK, IWORK, AK, BK, BNRM, SOLNRM)
!    $     .NE. 0 ) THEN ITERATION DONE
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :IN       Double Precision X(N).
!         The current approximate solution vector.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See "Description" in the
!         SDCGN routine.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MATVEC :EXT      External.
!         Name of a routine which performs the matrix vector multiply
!         Y = A*X given A and X.  The name of the MATVEC routine must
!         be declared external in the calling program.  The calling
!         sequence to MATVEC is:
!             CALL MATVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!         Where N is the number of unknowns, Y is the product A*X
!         upon return X is an input vector, NELT is the number of
!         non-zeros in the SLAP-Column IA, JA, A storage for the matrix
!         A.  ISYM is a flag which, if non-zero, denotes that A is
!         symmetric and only the lower or upper triangle is stored.
! MTTVEC :EXT      External.
!         Name of a routine which performs the matrix transpose vector
!         multiply y = A'*X given A and X (where ' denotes transpose).
!         The name of the MTTVEC routine must be declared external in
!         the calling program.  The calling sequence to MTTVEC is the
!         same as that for MATVEC, viz.:
!             CALL MTTVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!         Where N is the number of unknowns, Y is the product A'*X
!         upon return X is an input vector, NELT is the number of
!         non-zeros in the SLAP-Column IA, JA, A storage for the matrix
!         A.  ISYM is a flag which, if non-zero, denotes that A is
!         symmetric and only the lower or upper triangle is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R for
!         Z given R with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays).  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSOLVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is the right-hand side
!         vector, and Z is the solution upon return.  RWORK is a
!         double precision
!         array that can be used to pass necessary preconditioning
!         information and/or workspace to MSOLVE.  IWORK is an integer
!         work array for the same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than tol, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :IN       Integer.
!         The iteration for which to check for convergence.
! ERR    :OUT      Double Precision.
!         Error estimate of error in the X(N) approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Error flag.  IERR is set to 3 if ITOL is not on of the
!         acceptable values, see above.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :IN       Double Precision R(N).
!         The residual R = B-AX.
! Z      :WORK     Double Precision Z(N).
! P      :IN       Double Precision P(N).
!         The conjugate direction vector.
! ATP    :IN       Double Precision ATP(N).
!         A-transpose times the conjugate direction vector.
! ATZ    :IN       Double Precision ATZ(N).
!         A-transpose times the pseudo-residual.
! DZ     :IN       Double Precision DZ(N).
!         Workspace used to hold temporary vector(s).
! ATDZ   :WORK     Double Precision ATDZ(N).
!         Workspace.
! RWORK  :WORK     Double Precision RWORK(USER DEFINABLE).
!         Double Precision array that can be used by MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINABLE).
!         Integer array that can be used by MSOLVE.
! BNRM   :INOUT    Double Precision.
!         Norm of the right hand side.  Type of norm depends on ITOL.
!         Calculated only on the first call.
! SOLNRM :INOUT    Double Precision.
!         2-Norm of the true solution, SOLN.  Only computed and used
!         if ITOL = 11.
!
! *Function Return Values:
!       0 : Error estimate (determined by ITOL) is *NOT* less than the
!           specified tolerance, TOL.  The iteration must continue.
!       1 : Error estimate (determined by ITOL) is less than the
!           specified tolerance, TOL.  The iteration can be considered
!           complete.
!
! *Precision:           Double Precision
! *See Also:
!       SDCGN
!
! *Cautions:
!     This routine will attempt to write to the fortran logical output
!     unit IUNIT, if IUNIT .ne. 0.  Thus, the user must make sure that
!     this  logical  unit  must  be  attached  to  a  file or terminal
!     before calling this routine with a non-zero  value  for   IUNIT.
!     This routine does not check for the validity of a non-zero IUNIT
!     unit number.
!***REFERENCES  (NONE)
!***ROUTINES CALLED  MATVEC, MTTVEC, MSOLVE and the BLAS
!***COMMON BLOCKS    SOLBLK
!***END PROLOGUE  ISDCGN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
      INTEGER IUNIT, IWORK(*)
      DOUBLE PRECISION B(N), X(N), A(N), TOL, ERR, R(N), Z(N), P(N)
      DOUBLE PRECISION ATP(N), ATZ(N), DZ(N), ATDZ(N), RWORK(*)
      DOUBLE PRECISION AK, BK, BNRM, SOLNRM
      EXTERNAL MATVEC, MTTVEC, MSOLVE
      COMMON /SOLBLK/ SOLN(1)
!
!***FIRST EXECUTABLE STATEMENT  ISDCGN
      ISDCGN = 0
!
      IF( ITOL.EQ.1 ) THEN
!         err = ||Residual||/||RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) BNRM = DNRM2(N, B, 1)
         ERR = DNRM2(N, R, 1)/BNRM
      ELSE IF( ITOL.EQ.2 ) THEN
!                  -1              -1
!         err = ||M  Residual||/||M  RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) THEN
            CALL MSOLVE(N, B, DZ, NELT, IA, JA, A, ISYM, RWORK, IWORK)
            CALL MTTVEC(N, DZ, ATDZ, NELT, IA, JA, A, ISYM)
            BNRM = DNRM2(N, ATDZ, 1)
         end if
         ERR = DNRM2(N, ATZ, 1)/BNRM
      ELSE IF( ITOL.EQ.11 ) THEN
!         err = ||x-TrueSolution||/||TrueSolution|| (2-Norms).
         IF(ITER .EQ. 0) SOLNRM = DNRM2(N, SOLN, 1)

         DZ(1:n) = X(1:n) - SOLN(1:n)

         ERR = DNRM2(N, DZ, 1)/SOLNRM
      ELSE
!
!         If we get here ITOL is not one of the acceptable values.
         ERR = 1.0E10
         IERR = 3
      end if
!
      IF( IUNIT.NE.0 ) THEN
         IF( ITER.EQ.0 ) THEN
            WRITE(IUNIT,1000) N, ITOL
         end if
         WRITE(IUNIT,1010) ITER, ERR, AK, BK
      end if
      IF( ERR.LE.TOL ) ISDCGN = 1
!
      RETURN
 1000 FORMAT(' PCG Applied to the Normal Equations for ', &
           'N, ITOL = ',I5, I5, &
           /' ITER','   Error Estimate','            Alpha', &
           '             Beta')
 1010 FORMAT(1X,I4,1X,E16.7,1X,E16.7,1X,E16.7)
!------------- LAST LINE OF ISDCGN FOLLOWS ----------------------------
      END
      SUBROUTINE DCGS(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, &
           MSOLVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           R, R0, P, Q, U, V1, V2, RWORK, IWORK)
!***BEGIN PROLOGUE  DCGS
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DCGS-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition,  BiConjugate Gradient
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned BiConjugate Gradient Sparse Ax=b solver.
!            Routine to solve a Non-Symmetric linear system Ax = b
!            using the Preconditioned BiConjugate Gradient method.
!***DESCRIPTION
! *Usage:
!      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!      INTEGER ITER, IERR, IUNIT, IWORK(USER DEFINABLE)
!      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), R0(N), P(N)
!      DOUBLE PRECISION Q(N), U(N), V1(N), V2(N), RWORK(USER DEFINABLE)
!      EXTERNAL MATVEC, MSOLVE
!
!      CALL DCGS(N, B, X, NELT, IA, JA, A, ISYM, MATVEC,
!     $     MSOLVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT,
!     $     R, R0, P, Q, U, V1, V2, RWORK, IWORK)
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See "Description", below
!         for more late breaking details...
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MATVEC :EXT      External.
!         Name of a routine which  performs the matrix vector multiply
!         operation  Y = A*X  given A and X.  The  name of  the MATVEC
!         routine must  be declared external  in the  calling program.
!         The calling sequence of MATVEC is:
!             CALL MATVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!         Where N is the number of unknowns, Y is the product A*X upon
!         return,  X is an input  vector.  NELT, IA,  JA,  A and  ISYM
!         define the SLAP matrix data structure: see Description,below.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R  for Z
!         given R with the preconditioning matrix M (M is supplied via
!         RWORK  and IWORK arrays).   The name  of  the MSOLVE routine
!         must be declared  external  in the  calling   program.   The
!         calling sequence of MSLOVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is  the right-hand side
!         vector, and Z is the solution upon return.  NELT,  IA, JA, A
!         and  ISYM define the SLAP  matrix  data structure: see
!         Description, below.  RWORK is a  double precision array that
!         can be used
!         to  pass   necessary  preconditioning     information and/or
!         workspace to MSOLVE.  IWORK is an integer work array for the
!         same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         This routine must calculate the residual from R = A*X - B.
!         This is un-natural and hence expensive for this type of iter-
!         ative method.  ITOL=2 is *STRONGLY* recommended.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than tol, where M-inv time a vector is the pre-
!         conditioning step.  This is the *NATURAL* stopping for this
!         iterative method and is *STRONGLY* recommended.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than tol) through a common block,
!         COMMON /SOLBLK/ SOLN( )
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than tol.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Breakdown of the method detected.
!                       $(r0,r) approximately 0.0$.
!           IERR = 6 => Stagnation of the method detected.
!                        $(r0,v) approximately 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :WORK     Double Precision R(N).
! R0     :WORK     Double Precision R0(N).
! P      :WORK     Double Precision P(N).
! Q      :WORK     Double Precision Q(N).
! U      :WORK     Double Precision U(N).
! V1     :WORK     Double Precision V1(N).
! V2     :WORK     Double Precision V2(N).
! RWORK  :WORK     Double Precision RWORK(USER DEFINED).
!         Double Precision array that can be used for workspace in
!         MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINED).
!         Integer array that can be used for workspace in MSOLVE.
!
! *Description
!       This routine does  not care  what matrix data   structure is
!       used for  A and M.  It simply   calls  the MATVEC and MSOLVE
!       routines, with  the arguments as  described above.  The user
!       could write any type of structure and the appropriate MATVEC
!       and MSOLVE routines.  It is assumed  that A is stored in the
!       IA, JA, A  arrays in some fashion and  that M (or INV(M)) is
!       stored  in  IWORK  and  RWORK   in  some fashion.   The SLAP
!       routines SDBCG and DSLUCS are examples of this procedure.
!
!       Two  examples  of  matrix  data structures  are the: 1) SLAP
!       Triad  format and 2) SLAP Column format.
!
!       =================== S L A P Triad format ===================
!
!       In  this   format only the  non-zeros are  stored.  They may
!       appear  in *ANY* order.   The user  supplies three arrays of
!       length NELT, where  NELT  is the number  of non-zeros in the
!       matrix:  (IA(NELT), JA(NELT),  A(NELT)).  For each  non-zero
!       the  user puts   the row  and  column index   of that matrix
!       element in the IA and JA arrays.  The  value of the non-zero
!       matrix  element is  placed in  the corresponding location of
!       the A  array.  This is  an extremely easy data  structure to
!       generate.  On  the other hand it  is  not too  efficient  on
!       vector  computers   for the  iterative  solution  of  linear
!       systems.  Hence, SLAP  changes this input  data structure to
!       the SLAP   Column  format for the  iteration (but   does not
!       change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *See Also:
!       DSDCGS, DSLUCS
!***REFERENCES  1. P. Sonneveld, ``CGS, a fast Lanczos-type solver
!                 for nonsymmetric linear systems'', Delft University
!                 of Technology Report 84-16, Department of Math-
!                 ematics and Informatics, Julianalaan 132, 2628 BL
!                 Delft, The Netherlands.
!
!               2. E.F. Kaasschieter, ``The solution of non-symmetric
!                 linear systems by bi-conjugate gradients or conjugate
!                 gradients squared,''  Delft University of Tech-
!                 nology Report 86-21, Department of Mathematics and
!                 Informatics, Julianalaan 132, 2628 BL Delft,
!                 The Netherlands.
!***ROUTINES CALLED  MATVEC, MSOLVE, ISDCGS, DDOT, D1MACH
!***END PROLOGUE  DCGS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
      INTEGER ITER, IERR, IUNIT, IWORK(*)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), R0(N), P(N)
      DOUBLE PRECISION Q(N), U(N), V1(N), V2(N), RWORK(*)
      EXTERNAL MATVEC, MSOLVE
!
!         Check some of the input data.
!***FIRST EXECUTABLE STATEMENT  DCGS
      ITER = 0
      IERR = 0
      IF( N.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      TOLMIN = 500.0 * epsilon ( tolmin )
      IF( TOL.LT.TOLMIN ) THEN
         TOL = TOLMIN
         IERR = 4
      end if
!
!         Calculate initial residual and pseudo-residual, and check
!         stopping criterion.
      CALL MATVEC(N, X, R, NELT, IA, JA, A, ISYM)

      V1(1:n)  = R(1:n) - B(1:n)

      CALL MSOLVE(N, V1, R, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!
      IF( ISDCGS(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, R0, P, Q, &
           U, V1, V2, RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 ) &
           GO TO 200
      IF( IERR.NE.0 ) RETURN
!
!         Set initial values.
!
      FUZZ = ( epsilon ( fuzz ) )**2
      R0(1:n) = R(1:n)
      RHONM1 = 1.0
!
!         ***** ITERATION LOOP *****
!
      DO 100 K=1,ITMAX

         ITER = K
!
!         Calculate coefficient BK and direction vectors U, V and P.
         RHON = DDOT(N, R0, 1, R, 1)
         IF( ABS(RHONM1).LT.FUZZ ) GOTO 998
         BK = RHON/RHONM1

         IF( ITER.EQ.1 ) THEN
            U(1:n) = R(1:n)
            P(1:n) = R(1:n)
         ELSE
            U(1:n) = R(1:n) + BK*Q(1:n)
            V1(1:n) = Q(1:n) + BK*P(1:n)
            P(1:n) = U(1:n) + BK*V1(1:n)
         end if
!
!         Calculate coefficient AK, new iterate X, Q
         CALL MATVEC(N, P, V2, NELT, IA, JA, A, ISYM)
         CALL MSOLVE(N, V2, V1, NELT, IA, JA, A, ISYM, RWORK, IWORK)
         SIGMA = DDOT(N, R0, 1, V1, 1)
         IF( ABS(SIGMA).LT.FUZZ ) GOTO 999
         AK = RHON/SIGMA
         AKM = -AK
         DO 60 I = 1, N
            Q(I) = U(I) + AKM*V1(I)
 60      CONTINUE
         DO 70 I = 1, N
            V1(I) = U(I) + Q(I)
 70      CONTINUE
!         X = X - ak*V1.
         CALL DAXPY( N, AKM, V1, 1, X, 1 )
!                     -1
!         R = R - ak*M  *A*V1
         CALL MATVEC(N, V1, V2, NELT, IA, JA, A, ISYM)
         CALL MSOLVE(N, V2, V1, NELT, IA, JA, A, ISYM, RWORK, IWORK)
         CALL DAXPY( N, AKM, V1, 1, R, 1 )
!
!         check stopping criterion.
         IF( ISDCGS(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE, &
              ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, R0, P, Q, &
              U, V1, V2, RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 ) &
              GO TO 200
!
!         Update RHO.
         RHONM1 = RHON
 100  CONTINUE
!
!         *****   end of loop  *****
!         Stopping criterion not satisfied.
      ITER = ITMAX + 1
      IERR = 2
 200  RETURN
!
!         Breakdown of method detected.
 998  IERR = 5
      RETURN
!
!         Stagnation of method detected.
 999  IERR = 6
      RETURN
!------------- LAST LINE OF DCGS FOLLOWS ----------------------------
      END
      SUBROUTINE DSDCGS(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSDCGS
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(SSDCGS-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Diagonally Scaled CGS Sparse Ax=b Solver.
!            Routine to solve a linear system  Ax = b  using the
!            BiConjugate Gradient Squared method with diagonal scaling.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK(10), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(8*N)
!
!     CALL DSDCGS(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "Description",
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         This routine must calculate the residual from R = A*X - B.
!         This is un-natural and hence expensive for this type of iter-
!         ative method.  ITOL=2 is *STRONGLY* recommended.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than tol, where M-inv time a vector is the pre-
!         conditioning step.  This is the *NATURAL* stopping for this
!         iterative method and is *STRONGLY* recommended.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Breakdown of the method detected.
!                       $(r0,r) approximately 0.0$.
!           IERR = 6 => Stagnation of the method detected.
!                        $(r0,v) approximately 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.  LENW >= 8*N.
! IWORK  :WORK     Integer IWORK(LENIW).
!         Used to hold pointers into the RWORK array.
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.  LENIW >= 10.
!
! *Description:
!       This  routine performs  preconditioned  BiConjugate gradient
!       method on the Non-Symmetric positive definite  linear system
!       Ax=b. The preconditioner is M = DIAG(A), the diagonal of the
!       matrix   A.   This is the  simplest   of preconditioners and
!       vectorizes very well.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A) is  modified internally to
!       be   the SLAP  Column format.   See above.
!
! *See Also:
!       DCGS, DLUBCG
!***REFERENCES  1. P. Sonneveld, ``CGS, a fast Lanczos-type solver
!                 for nonsymmetric linear systems'', Delft University
!                 of Technology Report 84-16, Department of Math-
!                 ematics and Informatics, Julianalaan 132, 2628 BL
!                 Delft, The Netherlands.
!
!               2. E.F. Kaasschieter, ``The solution of non-symmetric
!                 linear systems by bi-conjugate gradients or conjugate
!                 gradients squared,''  Delft University of Tech-
!                 nology Report 86-21, Department of Mathematics and
!                 Informatics, Julianalaan 132, 2628 BL Delft,
!                 The Netherlands.
!***ROUTINES CALLED  DS2Y, DCHKW, DSDS, DCGS
!***END PROLOGUE  DSDCGS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
      INTEGER IERR, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(N), TOL, ERR, RWORK(LENW)
      EXTERNAL DSMV, DSDI
      PARAMETER (LOCRB=1, LOCIB=11)
!
!         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
!***FIRST EXECUTABLE STATEMENT  DSDCGS
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Set up the workspace.  Compute the inverse of the
!         diagonal of the matrix.
      LOCIW = LOCIB
!
      LOCDIN = LOCRB
      LOCR  = LOCDIN + N
      LOCR0 = LOCR + N
      LOCP  = LOCR0 + N
      LOCQ  = LOCP + N
      LOCU  = LOCQ + N
      LOCV1 = LOCU + N
      LOCV2 = LOCV1 + N
      LOCW  = LOCV2 + N
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSDCGS', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(4) = LOCDIN
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
      CALL DSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCDIN))
!
!         Perform the Diagonally Scaled
!         BiConjugate Gradient Squared algorithm.
      CALL DCGS(N, B, X, NELT, IA, JA, A, ISYM, DSMV, &
           DSDI, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           RWORK(LOCR), RWORK(LOCR0), RWORK(LOCP), &
           RWORK(LOCQ), RWORK(LOCU), RWORK(LOCV1), &
           RWORK(LOCV2), RWORK(1), IWORK(1))
      RETURN
!------------- LAST LINE OF DSDCGS FOLLOWS ----------------------------
      END
      SUBROUTINE DSLUCS(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSLUCS
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(SSLUCS-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative incomplete LU Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Incomplete LU BiConjugate Gradient Sparse Ax=b solver.
!            Routine to solve a linear system  Ax = b  using the
!            BiConjugate Gradient  method  with  Incomplete   LU
!            decomposition preconditioning.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK(NEL+NU+4*N+2), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(NEL+NU+8*N)
!
!     CALL DSLUCS(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW)
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "Description",
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         This routine must calculate the residual from R = A*X - B.
!         This is un-natural and hence expensive for this type of iter-
!         ative method.  ITOL=2 is *STRONGLY* recommended.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than tol, where M-inv time a vector is the pre-
!         conditioning step.  This is the *NATURAL* stopping for this
!         iterative method and is *STRONGLY* recommended.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Breakdown of the method detected.
!                       $(r0,r) approximately 0.0$.
!           IERR = 6 => Stagnation of the method detected.
!                        $(r0,v) approximately 0.0$.
!           IERR = 7 => Incomplete factorization broke down
!                       and was fudged.  Resulting preconditioning may
!                       be less than the best.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.  NEL is the
!         number of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal).  NU is the number of nonzeros in the upper
!         triangle of the matrix (including the diagonal).
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= NEL+NU+8*N.
! IWORK  :WORK     Integer IWORK(LENIW).
!         Integer array used for workspace.  NEL is the number of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal).  NU is the number of nonzeros in the upper
!         triangle of the matrix (including the diagonal).
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.
!         LENIW >= NEL+NU+4*N+12.
!
! *Description:
!       This routine is simply a  driver for the DCGSN  routine.  It
!       calls the DSILUS routine to set  up the  preconditioning and
!       then  calls DCGSN with  the appropriate   MATVEC, MTTVEC and
!       MSOLVE, MTSOLV routines.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA,  A) is modified internally to
!       be the   SLAP Column  format.  See above.
!
! *See Also:
!       DCGS, DSDCGS
!***REFERENCES  1. P. Sonneveld, ``CGS, a fast Lanczos-type solver
!                 for nonsymmetric linear systems'', Delft University
!                 of Technology Report 84-16, Department of Math-
!                 ematics and Informatics, Julianalaan 132, 2628 BL
!                 Delft, The Netherlands.
!
!               2. E.F. Kaasschieter, ``The solution of non-symmetric
!                 linear systems by bi-conjugate gradients or conjugate
!                 gradients squared,''  Delft University of Tech-
!                 nology Report 86-21, Department of Mathematics and
!                 Informatics, Julianalaan 132, 2628 BL Delft,
!                 The Netherlands.
!***ROUTINES CALLED  DS2Y, DCHKW, DSILUS, DCGS, DSMV, DSLUI
!***END PROLOGUE  DSLUCS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
      INTEGER IERR, IUNIT, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(LENW)
      EXTERNAL DSMV, DSLUI
      PARAMETER (LOCRB=1, LOCIB=11)
!
!         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
!***FIRST EXECUTABLE STATEMENT  DSLUCS
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Count number of Non-Zero elements preconditioner ILU matrix.
!         Then set up the work arrays.
      NL = 0
      NU = 0
      DO 20 ICOL = 1, N
!         Don't count diagonal.
         JBGN = JA(ICOL)+1
         JEND = JA(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
!VD$ NOVECTOR
            DO J = JBGN, JEND
               IF( IA(J).GT.ICOL ) THEN
                  NL = NL + 1
                  IF( ISYM.NE.0 ) NU = NU + 1
               ELSE
                  NU = NU + 1
               end if
            end do
         end if
 20   CONTINUE
!
      LOCIL = LOCIB
      LOCJL = LOCIL + N+1
      LOCIU = LOCJL + NL
      LOCJU = LOCIU + NU
      LOCNR = LOCJU + N+1
      LOCNC = LOCNR + N
      LOCIW = LOCNC + N
!
      LOCL   = LOCRB
      LOCDIN = LOCL + NL
      LOCUU  = LOCDIN + N
      LOCR   = LOCUU + NU
      LOCR0  = LOCR + N
      LOCP   = LOCR0 + N
      LOCQ   = LOCP + N
      LOCU   = LOCQ + N
      LOCV1  = LOCU + N
      LOCV2  = LOCV1 + N
      LOCW   = LOCV2 + N
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSLUCS', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(1) = LOCIL
      IWORK(2) = LOCJL
      IWORK(3) = LOCIU
      IWORK(4) = LOCJU
      IWORK(5) = LOCL
      IWORK(6) = LOCDIN
      IWORK(7) = LOCUU
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
!         Compute the Incomplete LU decomposition.
      CALL DSILUS( N, NELT, IA, JA, A, ISYM, NL, IWORK(LOCIL), &
           IWORK(LOCJL), RWORK(LOCL), RWORK(LOCDIN), NU, IWORK(LOCIU), &
           IWORK(LOCJU), RWORK(LOCUU), IWORK(LOCNR), IWORK(LOCNC) )
!
!         Perform the incomplete LU preconditioned
!         BiConjugate Gradient Squared algorithm.
      CALL DCGS(N, B, X, NELT, IA, JA, A, ISYM, DSMV, &
           DSLUI, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           RWORK(LOCR), RWORK(LOCR0), RWORK(LOCP), &
           RWORK(LOCQ), RWORK(LOCU), RWORK(LOCV1), &
           RWORK(LOCV2), RWORK, IWORK )
      RETURN
!------------- LAST LINE OF DSLUCS FOLLOWS ----------------------------
      END
      FUNCTION ISDCGS(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, R0, P, Q, U, &
           V1, V2, RWORK, IWORK, AK, BK, BNRM, SOLNRM)
!***BEGIN PROLOGUE  ISDCGS
!***REFER TO  DCGS, DSDCGS, DSLUCS
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(ISDCGS-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Stop Test
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned BiConjugate Gradient Stop Test.
!            This routine calculates the stop test for the BiConjugate
!            Gradient iteration scheme.  It returns a nonzero if the
!            error estimate (the type of which is determined by ITOL)
!            is less than the user specified tolerance TOL.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
!     INTEGER  IERR, IUNIT, IWORK(USER DEFINED)
!     DOUBLE PRECISION B(N), X(N), A(N), TOL, ERR, R(N), R0(N), P(N)
!     DOUBLE PRECISION Q(N), U(N), V1(N), V2(N)
!     DOUBLE PRECISION RWORK(USER DEFINED), AK, BK, BNRM, SOLNRM
!     EXTERNAL MATVEC, MSOLVE
!
!     IF( ISDCGS(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE, ITOL,
!    $     TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, R0, P, Q, U, V1,
!    $     V2, RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 )
!    $     THEN ITERATION DONE
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See "LONG DESCRIPTION", in
!         the SLAP routine DCGS for more late breaking details...
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MATVEC :EXT      External.
!         Name of a routine which  performs the matrix vector multiply
!         operation  Y = A*X  given A and X.  The  name of  the MATVEC
!         routine must  be declared external  in the  calling program.
!         The calling sequence of MATVEC is:
!             CALL MATVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!         Where N is the number of unknowns, Y is the product A*X upon
!         return,  X is an input  vector.  NELT, IA,  JA,  A and  ISYM
!         define the SLAP matrix data structure: see LONG DESCRIPTION,
!         below.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R  for Z
!         given R with the preconditioning matrix M (M is supplied via
!         RWORK  and IWORK arrays).   The name  of  the MSOLVE routine
!         must be declared  external  in the  calling   program.   The
!         calling sequence of MSLOVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is  the right-hand side
!         vector, and Z is the solution upon return.  NELT,  IA, JA, A
!         and  ISYM define the SLAP  matrix  data structure: see  LONG
!         DESCRIPTION, below.  RWORK is a  double precision array that
!         can be used
!         to  pass   necessary  preconditioning     information and/or
!         workspace to MSOLVE.  IWORK is an integer work array for the
!         same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         This routine must calculate the residual from R = A*X - B.
!         This is un-natural and hence expensive for this type of iter-
!         ative method.  ITOL=2 is *STRONGLY* recommended.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than tol, where M-inv time a vector is the pre-
!         conditioning step.  This is the *NATURAL* stopping for this
!         iterative method and is *STRONGLY* recommended.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Error flag.  IERR is set to 3 if ITOL is not on of the
!         acceptable values, see above.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :IN       Double Precision R(N).
!         The residual r = b - Ax.
! R0     :WORK     Double Precision R0(N).
! P      :DUMMY    Double Precision P(N).
! Q      :DUMMY    Double Precision Q(N).
! U      :DUMMY    Double Precision U(N).
! V1     :DUMMY    Double Precision V1(N).
! V2     :WORK     Double Precision V2(N).
!         If ITOL.eq.1 then V2 is used to hold A * X - B on every call.
!         If ITOL.eq.2 then V2 is used to hold M-inv * B on the first
!         call.
!         If ITOL.eq.11 then V2 is used to X - SOLN.
! RWORK  :WORK     Double Precision RWORK(USER DEFINED).
!         Double Precision array that can be used for workspace in
!         MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINED).
!         Integer array that can be used for workspace in MSOLVE.
! AK     :IN       Double Precision.
!         Current iterate BiConjugate Gradient iteration parameter.
! BK     :IN       Double Precision.
!         Current iterate BiConjugate Gradient iteration parameter.
! BNRM   :INOUT    Double Precision.
!         Norm of the right hand side.  Type of norm depends on ITOL.
!         Calculated only on the first call.
! SOLNRM :INOUT    Double Precision.
!         2-Norm of the true solution, SOLN.  Only computed and used
!         if ITOL = 11.
!
! *Function Return Values:
!       0 : Error estimate (determined by ITOL) is *NOT* less than the
!           specified tolerance, TOL.  The iteration must continue.
!       1 : Error estimate (determined by ITOL) is less than the
!           specified tolerance, TOL.  The iteration can be considered
!           complete.
!
! *Precision:           Double Precision
!***REFERENCES  (NONE)
!***ROUTINES CALLED  MATVEC, MSOLVE, DNRM2
!***COMMON BLOCKS    SOLBLK
!***END PROLOGUE  ISDCGS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
      INTEGER ITER, IERR, IUNIT, IWORK(1)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), R0(N), P(N)
      DOUBLE PRECISION Q(N), U(N), V1(N), V2(N), RWORK(1)
      DOUBLE PRECISION AK, BK, BNRM, SOLNRM
      COMMON /SOLBLK/ SOLN(1)
      EXTERNAL MATVEC, MSOLVE
!
!***FIRST EXECUTABLE STATEMENT  ISDCGS
      ISDCGS = 0
!
      IF( ITOL.EQ.1 ) THEN
!         err = ||Residual||/||RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) BNRM = DNRM2(N, B, 1)
         CALL MATVEC(N, X, V2, NELT, IA, JA, A, ISYM )
         DO 5 I = 1, N
            V2(I) = V2(I) - B(I)
 5       CONTINUE
         ERR = DNRM2(N, V2, 1)/BNRM
      ELSE IF( ITOL.EQ.2 ) THEN
!                  -1              -1
!         err = ||M  Residual||/||M  RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) THEN
            CALL MSOLVE(N, B, V2, NELT, IA, JA, A, ISYM, RWORK, IWORK)
            BNRM = DNRM2(N, V2, 1)
         end if
         ERR = DNRM2(N, R, 1)/BNRM
      ELSE IF( ITOL.EQ.11 ) THEN
!         err = ||x-TrueSolution||/||TrueSolution|| (2-Norms).
         IF(ITER .EQ. 0) SOLNRM = DNRM2(N, SOLN, 1)

         V2(1:n) = X(1:n) - SOLN(1:n)

         ERR = DNRM2(N, V2, 1)/SOLNRM
      ELSE
!
!         If we get here ITOL is not one of the acceptable values.
         ERR = 1.0E10
         IERR = 3
      end if
!
!         Print the error and Coeficients AK, BK on each step,
!         if desired.
      IF(IUNIT .NE. 0) THEN
         IF( ITER.EQ.0 ) THEN
            WRITE(IUNIT,1000) N, ITOL
         end if
         WRITE(IUNIT,1010) ITER, ERR, AK, BK
      end if
      IF(ERR .LE. TOL) ISDCGS = 1
!
      RETURN
 1000 FORMAT(' Preconditioned BiConjugate Gradient Squared for ', &
           'N, ITOL = ',I5, I5, &
           /' ITER','   Error Estimate','            Alpha', &
           '             Beta')
 1010 FORMAT(1X,I4,1X,E16.7,1X,E16.7,1X,E16.7)
!------------- LAST LINE OF ISDCGS FOLLOWS ----------------------------
      END
      SUBROUTINE DGMRES(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, SB, SX, &
           RGWK, LRGW, IGWK, LIGW, RWORK, IWORK )
!***BEGIN PROLOGUE  DGMRES
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DGMRES-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Generalized Minimum Residual
!***AUTHOR  Brown, Peter,    (LLNL), brown@lll-crg.llnl.gov
!           Hindmarsh, Alan, (LLNL), alanh@lll-crg.llnl.gov
!           Seager, Mark K., (LLNL), seager@lll-crg.llnl.gov
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!***PURPOSE  Preconditioned GMRES iterative sparse Ax=b solver.
!            This routine uses the generalized minimum residual
!            (GMRES) method with preconditioning to solve
!            non-symmetric linear systems of the form: A*x = b.
!***DESCRIPTION
! *Usage:
!      INTEGER   N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!      INTEGER   IERR, IUNIT, LRGW, LIGW, IGWK(LIGW)
!      INTEGER   IWORK(USER DEFINED)
!      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, SB(N), SX(N)
!      DOUBLE PRECISION RGWK(LRGW), RWORK(USER DEFINED)
!      EXTERNAL  MATVEC, MSOLVE
!
!      CALL DGMRES(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE,
!     $     ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, SB, SX,
!     $     RGWK, LRGW, IGWK, LIGW, RWORK, IWORK)
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for the solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See "Description", below
!         for more late breaking details...
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MATVEC :EXT      External.
!         Name of a routine which performs the matrix vector multiply
!         Y = A*X given A and X.  The name of the MATVEC routine must
!         be declared external in the calling program.  The calling
!         sequence to MATVEC is:
!             CALL MATVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!         where N is the number of unknowns, Y is the product A*X
!         upon return, X is an input vector, and NELT is the number of
!         non-zeros in the SLAP IA, JA, A storage for the matrix A.
!         ISYM is a flag which, if non-zero, denotes that A is
!         symmetric and only the lower or upper triangle is stored.
! MSOLVE :EXT      External.
!         Name of the routine which solves a linear system Mz = r for
!         z given r with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays.  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSLOVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is the right-hand side
!         vector, and z is the solution upon return.  RWORK is a
!         double precision
!         array that can be used to pass necessary preconditioning
!         information and/or workspace to MSOLVE.  IWORK is an integer
!         work array for the same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate the type of convergence criterion used.
!         ITOL=0  Means the  iteration stops when the test described
!                 below on  the  residual RL  is satisfied.  This is
!                 the  "Natural Stopping Criteria" for this routine.
!                 Other values  of   ITOL  cause  extra,   otherwise
!                 unnecessary, computation per iteration and     are
!                 therefore  much less  efficient.  See  ISDGMR (the
!                 stop test routine) for more information.
!         ITOL=1  Means   the  iteration stops   when the first test
!                 described below on  the residual RL  is satisfied,
!                 and there  is either right  or  no preconditioning
!                 being used.
!         ITOL=2  Implies     that   the  user    is   using    left
!                 preconditioning, and the second stopping criterion
!                 below is used.
!         ITOL=3  Means the  iteration stops   when  the  third test
!                 described below on Minv*Residual is satisfied, and
!                 there is either left  or no  preconditioning begin
!                 used.
!         ITOL=11 is    often  useful  for   checking  and comparing
!                 different routines.  For this case, the  user must
!                 supply  the  "exact" solution or  a  very accurate
!                 approximation (one with  an  error much less  than
!                 TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!                 if ITOL=11, iteration stops when the 2-norm of the
!                 difference between the iterative approximation and
!                 the user-supplied solution  divided by the  2-norm
!                 of the  user-supplied solution  is  less than TOL.
!                 Note that this requires  the  user to  set up  the
!                 "COMMON     /SOLBLK/ SOLN(LENGTH)"  in the calling
!                 routine.  The routine with this declaration should
!                 be loaded before the stop test so that the correct
!                 length is used by  the loader.  This procedure  is
!                 not standard Fortran and may not work correctly on
!                 your   system (although  it  has  worked  on every
!                 system the authors have tried).  If ITOL is not 11
!                 then this common block is indeed standard Fortran.
! TOL    :INOUT    Double Precision.
!         Convergence criterion, as described below.  If TOL is set
!         to zero on input, then a default value of 500*(the smallest
!         positive magnitude, machine epsilon) is used.
! ITMAX  :DUMMY    Integer.
!         Maximum number of iterations in most SLAP routines.  In
!         this routine this does not make sense.  The maximum number
!         of iterations here is given by ITMAX = MAXL*(NRMAX+1).
!         See IGWK for definitions of MAXL and NRMAX.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.  Letting norm() denote the Euclidean
!         norm, ERR is defined as follows..
!
!         If ITOL=0, then ERR = norm(SB*(B-A*X(L)))/norm(SB*B),
!                               for right or no preconditioning, and
!                         ERR = norm(SB*(M-inverse)*(B-A*X(L)))/
!                                norm(SB*(M-inverse)*B),
!                               for left preconditioning.
!         If ITOL=1, then ERR = norm(SB*(B-A*X(L)))/norm(SB*B),
!                               since right or no preconditioning
!                               being used.
!         If ITOL=2, then ERR = norm(SB*(M-inverse)*(B-A*X(L)))/
!                                norm(SB*(M-inverse)*B),
!                               since left preconditioning is being
!                               used.
!         If ITOL=3, then ERR =  Max  |(Minv*(B-A*X(L)))(i)/x(i)|
!                               i=1,n
!         If ITOL=11, then ERR = norm(SB*(X(L)-SOLN))/norm(SB*SOLN).
! IERR   :OUT      Integer.
!         Return error flag.
!               IERR = 0 => All went well.
!               IERR = 1 => Insufficient storage allocated for
!                           RGWK or IGWK.
!               IERR = 2 => Routine Dgmres failed to reduce the norm
!                           of the current residual on its last call,
!                           and so the iteration has stalled.  In
!                           this case, X equals the last computed
!                           approximation.  The user must either
!                           increase MAXL, or choose a different
!                           initial guess.
!               IERR =-1 => Insufficient length for RGWK array.
!                           IGWK(6) contains the required minimum
!                           length of the RGWK array.
!               IERR =-2 => Inconsistent ITOL and JPRE values.
!         For IERR <= 2, RGWK(1) = RHOL, which is the norm on the
!         left-hand-side of the relevant stopping test defined
!         below associated with the residual for the current
!         approximation X(L).
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! SB     :IN       Double Precision SB(N).
!         Array of length N containing scale factors for the right
!         hand side vector B.  If JSCAL.eq.0 (see below), SB need
!         not be supplied.
! SX     :IN       Double Precision SX(N).
!         Array of length N containing scale factors for the solution
!         vector X.  If JSCAL.eq.0 (see below), SX need not be
!         supplied.  SB and SX can be the same array in the calling
!         program if desired.
! RGWK   :INOUT    Double Precision RGWK(LRGW).
!         Double Precision array of size at least
!         1 + N*(MAXL+6) + MAXL*(MAXL+3)
!         used for work space by DGMRES.  See below for definition of
!         MAXL.
!         On return, RGWK(1) = RHOL.  See IERR for definition of RHOL.
! LRGW   :IN       Integer.
!         Length of the double precision workspace, RGWK.
!         LRGW > 1 + N*(MAXL+6) + MAXL*(MAXL+3).
!         For the default values, RGWK has size at least 131 + 16*N.
! IGWK   :INOUT    Integer IGWK(LIGW).
!         The following IGWK parameters should be set by the user
!         before calling this routine.
!         IGWK(1) = MAXL.  Maximum dimension of Krylov subspace in
!            which X - X0 is to be found (where, X0 is the initial
!            guess).  The default value of MAXL is 10.
!         IGWK(2) = KMP.  Maximum number of previous Krylov basis
!            vectors to which each new basis vector is made orthogonal.
!            The default value of KMP is MAXL.
!         IGWK(3) = JSCAL.  Flag indicating whether the scaling
!            arrays SB and SX are to be used.
!            JSCAL = 0 => SB and SX are not used and the algorithm
!               will perform as if all SB(I) = 1 and SX(I) = 1.
!            JSCAL = 1 =>  Only SX is used, and the algorithm
!               performs as if all SB(I) = 1.
!            JSCAL = 2 =>  Only SB is used, and the algorithm
!               performs as if all SX(I) = 1.
!            JSCAL = 3 =>  Both SB and SX are used.
!         IGWK(4) = JPRE.  Flag indicating whether preconditioning
!            is being used.
!            JPRE = 0  =>  There is no preconditioning.
!            JPRE > 0  =>  There is preconditioning on the right
!               only, and the solver will call routine MSOLVE.
!            JPRE < 0  =>  There is preconditioning on the left
!               only, and the solver will call routine MSOLVE.
!         IGWK(5) = NRMAX.  Maximum number of restarts of the
!            Krylov iteration.  The default value of NRMAX = 10.
!            if IWORK(5) = -1,  then no restarts are performed (in
!            this case, NRMAX is set to zero internally).
!         The following IWORK parameters are diagnostic information
!         made available to the user after this routine completes.
!         IGWK(6) = MLWK.  Required minimum length of RGWK array.
!         IGWK(7) = NMS.  The total number of calls to MSOLVE.
! LIGW   :IN       Integer.
!         Length of the integer workspace, IGWK.  LIGW >= 20.
!
! *Description:
!       DGMRES solves a linear system A*X = B rewritten in the form:
!
!        (SB*A*(M-inverse)*(SX-inverse))*(SX*M*X) = SB*B,
!
!       with right preconditioning, or
!
!        (SB*(M-inverse)*A*(SX-inverse))*(SX*X) = SB*(M-inverse)*B,
!
!       with left preconditioning, where A is an N-by-N double
!       precision matrix,
!       X  and  B are N-vectors,   SB and SX   are  diagonal scaling
!       matrices,   and M is  a preconditioning    matrix.   It uses
!       preconditioned  Krylov   subpace  methods  based     on  the
!       generalized minimum residual  method (GMRES).   This routine
!       optionally performs  either  the  full     orthogonalization
!       version of the  GMRES  algorithm or an incomplete variant of
!       it.  Both versions use restarting of the linear iteration by
!       default, although the user can disable this feature.
!
!       The GMRES  algorithm generates a sequence  of approximations
!       X(L) to the  true solution of the above  linear system.  The
!       convergence criteria for stopping the  iteration is based on
!       the size  of the  scaled norm of  the residual  R(L)  =  B -
!       A*X(L).  The actual stopping test is either:
!
!               norm(SB*(B-A*X(L))) .le. TOL*norm(SB*B),
!
!       for right preconditioning, or
!
!               norm(SB*(M-inverse)*(B-A*X(L))) .le.
!                       TOL*norm(SB*(M-inverse)*B),
!
!       for left preconditioning, where norm() denotes the euclidean
!       norm, and TOL is  a positive scalar less  than one  input by
!       the user.  If TOL equals zero  when DGMRES is called, then a
!       default  value  of 500*(the   smallest  positive  magnitude,
!       machine epsilon) is used.  If the  scaling arrays SB  and SX
!       are used, then  ideally they  should be chosen  so  that the
!       vectors SX*X(or SX*M*X) and  SB*B have all their  components
!       approximately equal  to  one in  magnitude.  If one wants to
!       use the same scaling in X  and B, then  SB and SX can be the
!       same array in the calling program.
!
!       The following is a list of the other routines and their
!       functions used by DGMRES:
!       DPIGMR  Contains the main iteration loop for GMRES.
!       DORTH   Orthogonalizes a new vector against older basis vects.
!       DHEQR   Computes a QR decomposition of a Hessenberg matrix.
!       DHELS   Solves a Hessenberg least-squares system, using QR
!               factors.
!       DRLCAL  Computes the scaled residual RL.
!       DXLCAL  Computes the solution XL.
!       ISDGMR  User-replaceable stopping routine.
!
!       This routine does  not care  what matrix data   structure is
!       used for  A and M.  It simply   calls  the MATVEC and MSOLVE
!       routines, with  the arguments as  described above.  The user
!       could write any type of structure and the appropriate MATVEC
!       and MSOLVE routines.  It is assumed  that A is stored in the
!       IA, JA, A  arrays in some fashion and  that M (or INV(M)) is
!       stored  in  IWORK  and  RWORK   in  some fashion.   The SLAP
!       routines DSDCG and DSICCG are examples of this procedure.
!
!       Two  examples  of  matrix  data structures  are the: 1) SLAP
!       Triad  format and 2) SLAP Column format.
!
!       =================== S L A P Triad format ===================
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
!***REFERENCES  1. Peter N. Brown and A. C. Hindmarsh,
!                 "Reduced Storage Matrix Methods In Stiff ODE
!                 Systems," LLNL report UCRL-95088, Rev. 1,
!                 June 1987.
!***ROUTINES CALLED  DPIGMR, DORTH, DHEQR, DHELS, DRCAL, DXLCAL,
!                    ISDGMR, DNRM2, DDOT, DAXPY, DSCAL, IDAMAX, D1MACH.
!***END PROLOGUE  DGMRES
!         The following is for optimized compilation on LLNL/LTSS Crays.
!LLL. OPTIMIZE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
      INTEGER  IERR, IUNIT, LRGW, LIGW, IGWK(LIGW)
      INTEGER  IWORK(*)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, SB(N), SX(N)
      DOUBLE PRECISION RGWK(LRGW), RWORK(*)
      EXTERNAL MATVEC, MSOLVE, D1MACH
      INTEGER JPRE, KMP, MAXL, NMS, MAXLP1, NMSL, NRSTS, NRMAX
      INTEGER I, IFLAG, LR, LDL, LHES, LGMR, LQ, LV, LW
      DOUBLE PRECISION BNRM, RHOL, SUM
!
!***FIRST EXECUTABLE STATEMENT  DGMRES
      IERR = 0
!   ------------------------------------------------------------------
!         Load method parameters with user values or defaults.
!   ------------------------------------------------------------------
      MAXL = IGWK(1)
      IF (MAXL .EQ. 0) MAXL = 10
      IF (MAXL .GT. N) MAXL = N
      KMP = IGWK(2)
      IF (KMP .EQ. 0) KMP = MAXL
      IF (KMP .GT. MAXL) KMP = MAXL
      JSCAL = IGWK(3)
      JPRE = IGWK(4)
!         Check for consistent values of ITOL and JPRE.
      IF( ITOL.EQ.1 .AND. JPRE.LT.0 ) GOTO 650
      IF( ITOL.EQ.2 .AND. JPRE.GE.0 ) GOTO 650
      NRMAX = IGWK(5)
      IF( NRMAX.EQ.0 ) NRMAX = 10
!         If NRMAX .eq. -1, then set NRMAX = 0 to turn off restarting.
      IF( NRMAX.EQ.-1 ) NRMAX = 0
!         If input value of TOL is zero, set it to its default value.
      IF( TOL.EQ.0.0D0 ) TOL = 500.0 * epsilon ( tol )
!
!         Initialize counters.
      ITER = 0
      NMS = 0
      NRSTS = 0
!   ------------------------------------------------------------------
!         Form work array segment pointers.
!   ------------------------------------------------------------------
      MAXLP1 = MAXL + 1
      LV = 1
      LR = LV + N*MAXLP1
      LHES = LR + N + 1
      LQ = LHES + MAXL*MAXLP1
      LDL = LQ + 2*MAXL
      LW = LDL + N
      LXL = LW + N
      LZ = LXL + N
!
!         Load igwk(6) with required minimum length of the rgwk array.
      IGWK(6) = LZ + N - 1
      IF( LZ+N-1.GT.LRGW ) GOTO 640
!   ------------------------------------------------------------------
!         Calculate scaled-preconditioned norm of RHS vector b.
!   ------------------------------------------------------------------
      IF (JPRE .LT. 0) THEN
         CALL MSOLVE(N, B, RGWK(LR), NELT, IA, JA, A, ISYM, &
              RWORK, IWORK)
         NMS = NMS + 1
      ELSE
         CALL DCOPY(N, B, 1, RGWK(LR), 1)
      end if
      IF( JSCAL.EQ.2 .OR. JSCAL.EQ.3 ) THEN
         SUM = 0.D0
         DO I = 1,N
            SUM = SUM + (RGWK(LR-1+I)*SB(I))**2
         end do
         BNRM = DSQRT(SUM)
      ELSE
         BNRM = DNRM2(N,RGWK(LR),1)
      end if
!   ------------------------------------------------------------------
!         Calculate initial residual.
!   ------------------------------------------------------------------
      CALL MATVEC(N, X, RGWK(LR), NELT, IA, JA, A, ISYM)
      DO 50 I = 1,N
         RGWK(LR-1+I) = B(I) - RGWK(LR-1+I)
 50   CONTINUE
!   ------------------------------------------------------------------
!         If performing restarting, then load the residual into the
!         correct location in the Rgwk array.
!   ------------------------------------------------------------------
 100  CONTINUE
      IF( NRSTS.GT.NRMAX ) GOTO 610
      IF( NRSTS.GT.0 ) THEN
!         Copy the curr residual to different loc in the Rgwk array.
         CALL DCOPY(N, RGWK(LDL), 1, RGWK(LR), 1)
      end if
!   ------------------------------------------------------------------
!         Use the DPIGMR algorithm to solve the linear system A*Z = R.
!   ------------------------------------------------------------------
      CALL DPIGMR(N, RGWK(LR), SB, SX, JSCAL, MAXL, MAXLP1, KMP, &
             NRSTS, JPRE, MATVEC, MSOLVE, NMSL, RGWK(LZ), RGWK(LV), &
             RGWK(LHES), RGWK(LQ), LGMR, RWORK, IWORK, RGWK(LW), &
             RGWK(LDL), RHOL, NRMAX, B, BNRM, X, RGWK(LXL), ITOL, &
             TOL, NELT, IA, JA, A, ISYM, IUNIT, IFLAG, ERR)
      ITER = ITER + LGMR
      NMS = NMS + NMSL
!
!         Increment X by the current approximate solution Z of A*Z = R.
!
      LZM1 = LZ - 1
      DO 110 I = 1,N
         X(I) = X(I) + RGWK(LZM1+I)
 110  CONTINUE
      IF( IFLAG.EQ.0 ) GOTO 600
      IF( IFLAG.EQ.1 ) THEN
         NRSTS = NRSTS + 1
         GOTO 100
      end if
      IF( IFLAG.EQ.2 ) GOTO 620
!   ------------------------------------------------------------------
!         All returns are made through this section.
!   ------------------------------------------------------------------
!         The iteration has converged.
!
 600  CONTINUE
      IGWK(7) = NMS
      RGWK(1) = RHOL
      IERR = 0
      RETURN
!
!         Max number((NRMAX+1)*MAXL) of linear iterations performed.
 610  CONTINUE
      IGWK(7) = NMS
      RGWK(1) = RHOL
      IERR = 1
      RETURN
!
!         GMRES failed to reduce last residual in MAXL iterations.
!         The iteration has stalled.
 620  CONTINUE
      IGWK(7) = NMS
      RGWK(1) = RHOL
      IERR = 2
      RETURN
!         Error return.  Insufficient length for Rgwk array.
 640  CONTINUE
      ERR = TOL
      IERR = -1
      RETURN
!         Error return.  Inconsistent ITOL and JPRE values.
 650  CONTINUE
      ERR = TOL
      IERR = -2
      RETURN
!------------- LAST LINE OF DGMRES FOLLOWS ----------------------------
      END
      SUBROUTINE DSDGMR(N, B, X, NELT, IA, JA, A, ISYM, NSAVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, &
           IWORK, LENIW )
!***BEGIN PROLOGUE  DSDGMR
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSDGMR-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Generalized Minimum Residual
!***AUTHOR  Brown, Peter,    (LLNL), brown@lll-crg.llnl.gov
!           Hindmarsh, Alan, (LLNL), alanh@lll-crg.llnl.gov
!           Seager, Mark K., (LLNL), seager@lll-crg.llnl.gov
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!***PURPOSE  Diagonally scaled GMRES iterative sparse Ax=b solver.
!            This routine uses the generalized minimum residual
!            (GMRES) method with diagonal scaling to solve possibly
!            non-symmetric linear systems of the form: A*x = b.
!***DESCRIPTION
! *Usage:
!      INTEGER   N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE
!      INTEGER   ITOL, ITMAX, IERR, IUNIT, LENW, IWORK(LENIW), LENIW
!      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR
!      DOUBLE PRECISION RWORK(LENW)
!      EXTERNAL  MATVEC, MSOLVE
!
!      CALL DSDGMR(N, B, X, NELT, IA, JA, A, ISYM, NSAVE,
!     $     ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT,
!     $     RWORK, LENW, IWORK, LENIW)
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "Description",
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! NSAVE  :IN       Integer.
!         Number of direction vectors to save and orthogonalize against.
!         Must be greater than 1.
! ITOL   :IN       Integer.
!         Flag to indicate the type of convergence criterion used.
!         ITOL=0  Means the  iteration stops when the test described
!                 below on  the  residual RL  is satisfied.  This is
!                 the  "Natural Stopping Criteria" for this routine.
!                 Other values  of   ITOL  cause  extra,   otherwise
!                 unnecessary, computation per iteration and     are
!                 therefore  much less  efficient.  See  ISDGMR (the
!                 stop test routine) for more information.
!         ITOL=1  Means   the  iteration stops   when the first test
!                 described below on  the residual RL  is satisfied,
!                 and there  is either right  or  no preconditioning
!                 being used.
!         ITOL=2  Implies     that   the  user    is   using    left
!                 preconditioning, and the second stopping criterion
!                 below is used.
!         ITOL=3  Means the  iteration stops   when  the  third test
!                 described below on Minv*Residual is satisfied, and
!                 there is either left  or no  preconditioning begin
!                 used.
!         ITOL=11 is    often  useful  for   checking  and comparing
!                 different routines.  For this case, the  user must
!                 supply  the  "exact" solution or  a  very accurate
!                 approximation (one with  an  error much less  than
!                 TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!                 if ITOL=11, iteration stops when the 2-norm of the
!                 difference between the iterative approximation and
!                 the user-supplied solution  divided by the  2-norm
!                 of the  user-supplied solution  is  less than TOL.
!                 Note that this requires  the  user to  set up  the
!                 "COMMON     /SOLBLK/ SOLN(LENGTH)"  in the calling
!                 routine.  The routine with this declaration should
!                 be loaded before the stop test so that the correct
!                 length is used by  the loader.  This procedure  is
!                 not standard Fortran and may not work correctly on
!                 your   system (although  it  has  worked  on every
!                 system the authors have tried).  If ITOL is not 11
!                 then this common block is indeed standard Fortran.
! TOL    :INOUT    Double Precision.
!         Convergence criterion, as described below.  If TOL is set
!         to zero on input, then a default value of 500*(the smallest
!         positive magnitude, machine epsilon) is used.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.  This routine uses the default
!         of NRMAX = ITMAX/NSAVE to determine the when each restart
!         oshould ccur.  See the description of NRMAX and MAXL in
!         DGMRES for a full and frightfully interesting discussion of
!         this topic.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.  Letting norm() denote the Euclidean
!         norm, ERR is defined as follows...
!         If ITOL=0, then ERR = norm(SB*(B-A*X(L)))/norm(SB*B),
!                               for right or no preconditioning, and
!                         ERR = norm(SB*(M-inverse)*(B-A*X(L)))/
!                                norm(SB*(M-inverse)*B),
!                               for left preconditioning.
!         If ITOL=1, then ERR = norm(SB*(B-A*X(L)))/norm(SB*B),
!                               since right or no preconditioning
!                               being used.
!         If ITOL=2, then ERR = norm(SB*(M-inverse)*(B-A*X(L)))/
!                                norm(SB*(M-inverse)*B),
!                               since left preconditioning is being
!                               used.
!         If ITOL=3, then ERR =  Max  |(Minv*(B-A*X(L)))(i)/x(i)|
!                               i=1,n
!         If ITOL=11, then ERR = norm(SB*(X(L)-SOLN))/norm(SB*SOLN).
! IERR   :OUT      Integer.
!         Return error flag.
!               IERR = 0 => All went well.
!               IERR = 1 => Insufficient storage allocated for
!                           RGWK or IGWK.
!               IERR = 2 => Routine DPIGMR failed to reduce the norm
!                           of the current residual on its last call,
!                           and so the iteration has stalled.  In
!                           this case, X equals the last computed
!                           approximation.  The user must either
!                           increase MAXL, or choose a different
!                           initial guess.
!               IERR =-1 => Insufficient length for RGWK array.
!                           IGWK(6) contains the required minimum
!                           length of the RGWK array.
!               IERR =-2 => Inconsistent ITOL and JPRE values.
!         For IERR <= 2, RGWK(1) = RHOL, which is the norm on the
!         left-hand-side of the relevant stopping test defined
!         below associated with the residual for the current
!         approximation X(L).
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK    Double Precision RWORK(LENW).
!         Double Precision array of size LENW.
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= 1 + N*(NSAVE+7) + NSAVE*(NSAVE+3).
!         For the recommended values of NSAVE (10), RWORK has size at
!         least 131 + 17*N.
! IWORK  :INOUT    Integer IWORK(USER DEFINED >= 30).
!         Used to hold pointers into the RWORK array.
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace IWORK.  LENIW >= 30.
!
! *Description:
!       DSDGMR solves a linear system A*X = B rewritten in the form:
!
!        (SB*A*(M-inverse)*(SX-inverse))*(SX*M*X) = SB*B,
!
!       with right preconditioning, or
!
!        (SB*(M-inverse)*A*(SX-inverse))*(SX*X) = SB*(M-inverse)*B,
!
!       with left preconditioning, where a is an n-by-n double
!       precision matrix,
!       X and  B  are N-vectors,  SB and  SX  are  diagonal  scaling
!       matrices, and  M   is   the  diagonal  of   A.     It   uses
!       preconditioned   Krylov  subpace   methods  based    on  the
!       generalized  minimum residual method (GMRES).   This routine
!       is  a  driver routine  which   assumes a  SLAP matrix   data
!       structure  and   sets  up the  necessary information   to do
!       diagonal preconditioning and  calls  the main GMRES  routine
!       DGMRES   for  the  solution  of the   linear system.  DGMRES
!       optionally   performs   either the   full  orthogonalization
!       version of the GMRES algorithm or an  incomplete  variant of
!       it.  Both versions use restarting of the linear iteration by
!       default, although the user can disable this feature.
!
!       The GMRES  algorithm generates a sequence  of approximations
!       X(L) to the  true solution of the above  linear system.  The
!       convergence criteria for stopping the  iteration is based on
!       the size  of the  scaled norm of  the residual  R(L)  =  B -
!       A*X(L).  The actual stopping test is either:
!
!               norm(SB*(B-A*X(L))) .le. TOL*norm(SB*B),
!
!       for right preconditioning, or
!
!               norm(SB*(M-inverse)*(B-A*X(L))) .le.
!                       TOL*norm(SB*(M-inverse)*B),
!
!       for left preconditioning, where norm() denotes the euclidean
!       norm, and TOL is  a positive scalar less  than one  input by
!       the user.  If TOL equals zero  when DSDGMR is called, then a
!       default  value  of 500*(the   smallest  positive  magnitude,
!       machine epsilon) is used.  If the  scaling arrays SB  and SX
!       are used, then  ideally they  should be chosen  so  that the
!       vectors SX*X(or SX*M*X) and  SB*B have all their  components
!       approximately equal  to  one in  magnitude.  If one wants to
!       use the same scaling in X  and B, then  SB and SX can be the
!       same array in the calling program.
!
!       The following is a list of the other routines and their
!       functions used by GMRES:
!       DGMRES  Contains the matrix structure independent driver
!               routine for GMRES.
!       DPIGMR  Contains the main iteration loop for GMRES.
!       DORTH   Orthogonalizes a new vector against older basis vects.
!       DHEQR   Computes a QR decomposition of a Hessenberg matrix.
!       DHELS   Solves a Hessenberg least-squares system, using QR
!               factors.
!       RLCALC  Computes the scaled residual RL.
!       XLCALC  Computes the solution XL.
!       ISDGMR  User-replaceable stopping routine.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A) is modified internally to be
!       the SLAP Column format.  See above.
!***REFERENCES  1. Peter N. Brown and A. C. Hindmarsh,
!                 "Reduced Storage Matrix Methods In Stiff ODE
!                 Systems," LLNL report UCRL-95088, Rev. 1,
!                 June 1987.
!***ROUTINES CALLED  DS2Y, DCHKW, DSDS, DGMRES
!***END PROLOGUE  DSDGMR
!         The following is for optimized compilation on LLNL/LTSS Crays.
!LLL. OPTIMIZE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE, ITOL
      INTEGER  ITMAX, ITER, IERR, IUNIT, LENW, LENIW, IWORK(LENIW)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(LENW)
      EXTERNAL DSMV, DSDI
      PARAMETER (LOCRB=1, LOCIB=11)
!
!         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
!***FIRST EXECUTABLE STATEMENT  DSDGMR
      IERR = 0
      ERR  = 0.0
      IF( NSAVE.LE.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Set up the workspace.  We assume MAXL=KMP=NSAVE.
!         Compute the inverse of the diagonal of the matrix.
      LOCIGW = LOCIB
      LOCIW = LOCIGW + 20
!
      LOCDIN = LOCRB
      LOCRGW = LOCDIN + N
      LOCW = LOCRGW + 1+N*(NSAVE+6)+NSAVE*(NSAVE+3)
!
      IWORK(4) = LOCDIN
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSDGMR', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      CALL DSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCDIN))
!
!         Perform the Diagonaly Scaled Generalized Minimum
!         Residual iteration algorithm.  The following DGMRES
!         defaults are used MAXL = KMP = NSAVE, JSCAL = 0,
!         JPRE = -1, NRMAX = ITMAX/NSAVE
      IWORK(LOCIGW  ) = NSAVE
      IWORK(LOCIGW+1) = NSAVE
      IWORK(LOCIGW+2) = 0
      IWORK(LOCIGW+3) = -1
      IWORK(LOCIGW+4) = ITMAX/NSAVE
      MYITOL = 0
!
      CALL DGMRES( N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSDI, &
           MYITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK, RWORK, &
           RWORK(LOCRGW), LENW-LOCRGW, IWORK(LOCIGW), 20, &
           RWORK, IWORK )
!
      IF( ITER.GT.ITMAX ) IERR = 2
      RETURN
!------------- LAST LINE OF DSDGMR FOLLOWS ----------------------------
      END
      SUBROUTINE DSLUGM(N, B, X, NELT, IA, JA, A, ISYM, NSAVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, &
           IWORK, LENIW )
!***BEGIN PROLOGUE  DSLUGM
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSLUGM-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Generalized Minimum Residual
!***AUTHOR  Brown, Peter,    (LLNL), brown@lll-crg.llnl.gov
!           Hindmarsh, Alan, (LLNL), alanh@lll-crg.llnl.gov
!           Seager, Mark K., (LLNL), seager@lll-crg.llnl.gov
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!***PURPOSE  Incomplete LU GMRES iterative sparse Ax=b solver.
!            This routine uses the generalized minimum residual
!            (GMRES) method with incomplete LU factorization for
!            preconditioning to solve possibly non-symmetric linear
!            systems of the form: Ax = b.
!***DESCRIPTION
! *Usage:
!      INTEGER   N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE
!      INTEGER   ITOL, ITMAX, IERR, IUNIT, LENW, IWORK(LENIW), LENIW
!      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, SB(N), SX(N)
!      DOUBLE PRECISION RWORK(LENW)
!      EXTERNAL  MATVEC, MSOLVE
!
!      CALL DSLUGM(N, B, X, NELT, IA, JA, A, ISYM, NSAVE,
!     $     ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT,
!     $     RWORK, LENW, IWORK, LENIW)
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "Description",
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! NSAVE  :IN       Integer.
!         Number of direction vectors to save and orthogonalize against.
!         Must be greater than 1.
! ITOL   :IN       Integer.
!         Flag to indicate the type of convergence criterion used.
!         ITOL=0  Means the  iteration stops when the test described
!                 below on  the  residual RL  is satisfied.  This is
!                 the  "Natural Stopping Criteria" for this routine.
!                 Other values  of   ITOL  cause  extra,   otherwise
!                 unnecessary, computation per iteration and     are
!                 therefore  much less  efficient.  See  ISDGMR (the
!                 stop test routine) for more information.
!         ITOL=1  Means   the  iteration stops   when the first test
!                 described below on  the residual RL  is satisfied,
!                 and there  is either right  or  no preconditioning
!                 being used.
!         ITOL=2  Implies     that   the  user    is   using    left
!                 preconditioning, and the second stopping criterion
!                 below is used.
!         ITOL=3  Means the  iteration stops   when  the  third test
!                 described below on Minv*Residual is satisfied, and
!                 there is either left  or no  preconditioning begin
!                 used.
!         ITOL=11 is    often  useful  for   checking  and comparing
!                 different routines.  For this case, the  user must
!                 supply  the  "exact" solution or  a  very accurate
!                 approximation (one with  an  error much less  than
!                 TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!                 if ITOL=11, iteration stops when the 2-norm of the
!                 difference between the iterative approximation and
!                 the user-supplied solution  divided by the  2-norm
!                 of the  user-supplied solution  is  less than TOL.
!                 Note that this requires  the  user to  set up  the
!                 "COMMON     /SOLBLK/ SOLN(LENGTH)"  in the calling
!                 routine.  The routine with this declaration should
!                 be loaded before the stop test so that the correct
!                 length is used by  the loader.  This procedure  is
!                 not standard Fortran and may not work correctly on
!                 your   system (although  it  has  worked  on every
!                 system the authors have tried).  If ITOL is not 11
!                 then this common block is indeed standard Fortran.
! TOL    :INOUT    Double Precision.
!         Convergence criterion, as described below.  If TOL is set
!         to zero on input, then a default value of 500*(the smallest
!         positive magnitude, machine epsilon) is used.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.  This routine uses the default
!         of NRMAX = ITMAX/NSAVE to determine the when each restart
!         should occur.  See the description of NRMAX and MAXL in
!         DGMRES for a full and frightfully interesting discussion of
!         this topic.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.  Letting norm() denote the Euclidean
!         norm, ERR is defined as follows...
!         If ITOL=0, then ERR = norm(SB*(B-A*X(L)))/norm(SB*B),
!                               for right or no preconditioning, and
!                         ERR = norm(SB*(M-inverse)*(B-A*X(L)))/
!                                norm(SB*(M-inverse)*B),
!                               for left preconditioning.
!         If ITOL=1, then ERR = norm(SB*(B-A*X(L)))/norm(SB*B),
!                               since right or no preconditioning
!                               being used.
!         If ITOL=2, then ERR = norm(SB*(M-inverse)*(B-A*X(L)))/
!                                norm(SB*(M-inverse)*B),
!                               since left preconditioning is being
!                               used.
!         If ITOL=3, then ERR =  Max  |(Minv*(B-A*X(L)))(i)/x(i)|
!                               i=1,n
!         If ITOL=11, then ERR = norm(SB*(X(L)-SOLN))/norm(SB*SOLN).
! IERR   :OUT      Integer.
!         Return error flag.
!               IERR = 0 => All went well.
!               IERR = 1 => Insufficient storage allocated for
!                           RGWK or IGWK.
!               IERR = 2 => Routine DPIGMR failed to reduce the norm
!                           of the current residual on its last call,
!                           and so the iteration has stalled.  In
!                           this case, X equals the last computed
!                           approximation.  The user must either
!                           increase MAXL, or choose a different
!                           initial guess.
!               IERR =-1 => Insufficient length for RGWK array.
!                           IGWK(6) contains the required minimum
!                           length of the RGWK array.
!               IERR =-2 => Inconsistent ITOL and JPRE values.
!         For IERR <= 2, RGWK(1) = RHOL, which is the norm on the
!         left-hand-side of the relevant stopping test defined
!         below associated with the residual for the current
!         approximation X(L).
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK    Double Precision RWORK(LENW).
!         Double Precision array of size LENW.
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= 1 + N*(NSAVE+7) +  NSAVE*(NSAVE+3)+NEL+NU.
!         For the recommended values,  RWORK
!         has size at least 131 + 17*N + NEL + NU.  Where  NEL is  the
!         number of non- zeros  in  the  lower triangle of  the matrix
!         (including the diagonal).  NU is the  number  of nonzeros in
!         the upper triangle of the matrix (including the diagonal).
! IWORK  :INOUT    Integer IWORK(LENIW).
!         Used to hold pointers into the RWORK array.
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.
!         LENIW >= NEL+NU+4*N+32.
!
! *Description:
!       DSLUGM solves a linear system A*X = B rewritten in the form:
!
!        (SB*A*(M-inverse)*(SX-inverse))*(SX*M*X) = SB*B,
!
!       with right preconditioning, or
!
!        (SB*(M-inverse)*A*(SX-inverse))*(SX*X) = SB*(M-inverse)*B,
!
!       with left preconditioning, where a is an n-by-n double
!       precision matrix,
!       X and  B are  N-vectors,  SB and  SX  are   diagonal scaling
!       matrices, and M is the Incomplete LU factorization of A.  It
!       uses preconditioned  Krylov subpace   methods  based on  the
!       generalized minimum residual  method (GMRES).   This routine
!       is a  driver  routine  which  assumes a SLAP   matrix   data
!       structure   and  sets  up  the  necessary  information to do
!       diagonal  preconditioning  and calls the main GMRES  routine
!       DGMRES for the   solution   of the linear   system.   DGMRES
!       optionally   performs  either  the full    orthogonalization
!       version of the  GMRES algorithm or  an incomplete variant of
!       it.  Both versions use restarting of the linear iteration by
!       default, although the user can disable this feature.
!
!       The GMRES  algorithm generates a sequence  of approximations
!       X(L) to the  true solution of the above  linear system.  The
!       convergence criteria for stopping the  iteration is based on
!       the size  of the  scaled norm of  the residual  R(L)  =  B -
!       A*X(L).  The actual stopping test is either:
!
!               norm(SB*(B-A*X(L))) .le. TOL*norm(SB*B),
!
!       for right preconditioning, or
!
!               norm(SB*(M-inverse)*(B-A*X(L))) .le.
!                       TOL*norm(SB*(M-inverse)*B),
!
!       for left preconditioning, where norm() denotes the euclidean
!       norm, and TOL is  a positive scalar less  than one  input by
!       the user.  If TOL equals zero  when DSLUGM is called, then a
!       default  value  of 500*(the   smallest  positive  magnitude,
!       machine epsilon) is used.  If the  scaling arrays SB  and SX
!       are used, then  ideally they  should be chosen  so  that the
!       vectors SX*X(or SX*M*X) and  SB*B have all their  components
!       approximately equal  to  one in  magnitude.  If one wants to
!       use the same scaling in X  and B, then  SB and SX can be the
!       same array in the calling program.
!
!       The following is a list of the other routines and their
!       functions used by GMRES:
!       DGMRES  Contains the matrix structure independent driver
!               routine for GMRES.
!       DPIGMR  Contains the main iteration loop for GMRES.
!       DORTH   Orthogonalizes a new vector against older basis vects.
!       DHEQR   Computes a QR decomposition of a Hessenberg matrix.
!       DHELS   Solves a Hessenberg least-squares system, using QR
!               factors.
!       RLCALC  Computes the scaled residual RL.
!       XLCALC  Computes the solution XL.
!       ISDGMR  User-replaceable stopping routine.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A) is modified internally to be
!       the SLAP Column format.  See above.
!***REFERENCES  1. Peter N. Brown and A. C. Hindmarsh,
!                 "Reduced Storage Matrix Methods In Stiff ODE
!                 Systems," LLNL report UCRL-95088, Rev. 1,
!                 June 1987.
!***ROUTINES CALLED  DS2Y, DCHKW, DSILUS, DGMRES, DSMV, DSLUI
!***END PROLOGUE  DSLUGM
!         The following is for optimized compilation on LLNL/LTSS Crays.
!LLL. OPTIMIZE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE, ITOL
      INTEGER  ITMAX, ITER, IERR, IUNIT, LENW, LENIW, IWORK(LENIW)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(LENW)
      EXTERNAL DSMV, DSLUI
      PARAMETER (LOCRB=1, LOCIB=11)
!
!         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
!***FIRST EXECUTABLE STATEMENT  DSLUGM
      IERR = 0
      ERR  = 0.0
      IF( NSAVE.LE.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Count number of Non-Zero elements preconditioner ILU matrix.
!         Then set up the work arrays.  We assume MAXL=KMP=NSAVE.
      NL = 0
      NU = 0
      DO 20 ICOL = 1, N
!         Don't count diagonal.
         JBGN = JA(ICOL)+1
         JEND = JA(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
!VD$ NOVECTOR
            DO J = JBGN, JEND
               IF( IA(J).GT.ICOL ) THEN
                  NL = NL + 1
                  IF( ISYM.NE.0 ) NU = NU + 1
               ELSE
                  NU = NU + 1
               end if
            end do
         end if
 20   CONTINUE
!
      LOCIGW = LOCIB
      LOCIL = LOCIGW + 20
      LOCJL = LOCIL + N+1
      LOCIU = LOCJL + NL
      LOCJU = LOCIU + NU
      LOCNR = LOCJU + N+1
      LOCNC = LOCNR + N
      LOCIW = LOCNC + N
!
      LOCL = LOCRB
      LOCDIN = LOCL + NL
      LOCU = LOCDIN + N
      LOCRGW = LOCU + NU
      LOCW = LOCRGW + 1+N*(NSAVE+6)+NSAVE*(NSAVE+3)
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSLUGM', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(1) = LOCIL
      IWORK(2) = LOCJL
      IWORK(3) = LOCIU
      IWORK(4) = LOCJU
      IWORK(5) = LOCL
      IWORK(6) = LOCDIN
      IWORK(7) = LOCU
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
!         Compute the Incomplete LU decomposition.
      CALL DSILUS( N, NELT, IA, JA, A, ISYM, NL, IWORK(LOCIL), &
           IWORK(LOCJL), RWORK(LOCL), RWORK(LOCDIN), NU, IWORK(LOCIU), &
           IWORK(LOCJU), RWORK(LOCU), IWORK(LOCNR), IWORK(LOCNC) )
!
!         Perform the Incomplet LU Preconditioned Generalized Minimum
!         Residual iteration algorithm.  The following DGMRES
!         defaults are used MAXL = KMP = NSAVE, JSCAL = 0,
!         JPRE = -1, NRMAX = ITMAX/NSAVE
      IWORK(LOCIGW  ) = NSAVE
      IWORK(LOCIGW+1) = NSAVE
      IWORK(LOCIGW+2) = 0
      IWORK(LOCIGW+3) = -1
      IWORK(LOCIGW+4) = ITMAX/NSAVE
      MYITOL = 0
!
      CALL DGMRES( N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSLUI, &
           MYITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK, RWORK, &
           RWORK(LOCRGW), LENW-LOCRGW, IWORK(LOCIGW), 20, &
           RWORK, IWORK )
!
      IF( ITER.GT.ITMAX ) IERR = 2
      RETURN
!------------- LAST LINE OF DSLUGM FOLLOWS ----------------------------
      END
      SUBROUTINE DHELS(A, LDA, N, Q, B)
!***BEGIN PROLOGUE  DHEQR
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DHEQR-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Generalized Minimum Residual
!***AUTHOR  Brown, Peter,    (LLNL), brown@lll-crg.llnl.gov
!           Hindmarsh, Alan, (LLNL), alanh@lll-crg.llnl.gov
!           Seager, Mark K., (LLNL), seager@lll-crg.llnl.gov
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!***PURPOSE  Internal routine for DGMRES.
!***DESCRIPTION
!        This routine is extraced from the LINPACK routine SGESL with
!        changes  due to  the fact  that  A is an  upper   Hessenberg
!        matrix.
!
!        DHELS solves the least squares problem:
!
!                   MIN(B-A*X,B-A*X)
!
!        using the factors computed by DHEQR.
!
! *Usage:
!      INTEGER LDA, N
!      DOUBLE PRECISION A(LDA,1), B(1), Q(1)
!
!      CALL DHELS(A, LDA, N, Q, B)
!
! *Arguments:
! A       :IN       Double Precision A(LDA,N)
!          The output from DHEQR which contains the upper
!          triangular factor R in the QR decomposition of A.
! LDA     :IN       Integer
!          The leading dimension of the array A.
! N       :IN       Integer
!          A is originally an (N+1) by N matrix.
! Q       :IN       Double Precision Q(2*N)
!          The coefficients of the N givens rotations
!          used in the QR factorization of A.
! B       :INOUT    Double Precision B(N+1)
!          On input, B is the right hand side vector.
!          On output, B is the solution vector X.
! *See Also:
!         DGMRES
!
!***ROUTINES CALLED  DAXPY
!***END PROLOGUE  DHEQR
!         The following is for optimized compilation on LLNL/LTSS Crays.
!LLL. OPTIMIZE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER LDA, N
      DOUBLE PRECISION A(LDA,1), B(1), Q(1)
!
!         Local Variables.
!
      INTEGER IQ, K, KB, KP1
      DOUBLE PRECISION C, S, T, T1, T2
!
!         minimize(B-A*X,B-A*X).  First form Q*B.
!
      DO 20 K = 1, N
         KP1 = K + 1
         IQ = 2*(K-1) + 1
         C = Q(IQ)
         S = Q(IQ+1)
         T1 = B(K)
         T2 = B(KP1)
         B(K) = C*T1 - S*T2
         B(KP1) = S*T1 + C*T2
 20   CONTINUE
!
!         Now solve  R*X = Q*B.
!
      DO 40 KB = 1, N
         K = N + 1 - KB
         B(K) = B(K)/A(K,K)
         T = -B(K)
         CALL DAXPY(K-1, T, A(1,K), 1, B(1), 1)
 40   CONTINUE
      RETURN
!------------- LAST LINE OF DHELS FOLLOWS ----------------------------
      END
      SUBROUTINE DHEQR(A, LDA, N, Q, INFO, IJOB)
!***BEGIN PROLOGUE  DHEQR
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DHEQR-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Generalized Minimum Residual
!***AUTHOR  Brown, Peter,    (LLNL), brown@lll-crg.llnl.gov
!           Hindmarsh, Alan, (LLNL), alanh@lll-crg.llnl.gov
!           Seager, Mark K., (LLNL), seager@lll-crg.llnl.gov
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!***PURPOSE  Internal routine for DGMRES.
!***DESCRIPTION
!        This   routine  performs  a QR   decomposition  of an  upper
!        Hessenberg matrix A using Givens  rotations.  There  are two
!        options  available: 1)  Performing  a fresh decomposition 2)
!        updating the QR factors by adding a row and  a column to the
!        matrix A.
!
! *Usage:
!      INTEGER LDA, N, INFO, IJOB
!      DOUBLE PRECISION A(LDA,1), Q(1)
!
!      CALL DHEQR(A, LDA, N, Q, INFO, IJOB)
!
! *Arguments:
! A      :INOUT    Double Precision A(LDA,N)
!         On input, the matrix to be decomposed.
!         On output, the upper triangular matrix R.
!         The factorization can be written Q*A = R, where
!         Q is a product of Givens rotations and R is upper
!         triangular.
! LDA    :IN       Integer
!         The leading dimension of the array A.
! N      :IN       Integer
!         A is an (N+1) by N Hessenberg matrix.
! IJOB   :IN       Integer
!         = 1     means that a fresh decomposition of the
!                 matrix A is desired.
!         .ge. 2  means that the current decomposition of A
!                 will be updated by the addition of a row
!                 and a column.
! Q      :OUT      Double Precision Q(2*N)
!         The factors c and s of each Givens rotation used
!         in decomposing A.
! INFO   :OUT      Integer
!         = 0  normal value.
!         = K  if  A(K,K) .eq. 0.0 .  This is not an error
!           condition for this subroutine, but it does
!           indicate that DHELS will divide by zero
!           if called.
!
! *See Also:
!         DGMRES
!
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DHEQR
!         The following is for optimized compilation on LLNL/LTSS Crays.
!LLL. OPTIMIZE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER LDA, N, INFO, IJOB
      DOUBLE PRECISION A(LDA,*), Q(*)
!
!         Local Variables.
!
      INTEGER I, IQ, J, K, KM1, KP1, NM1
      DOUBLE PRECISION C, S, T, T1, T2
!
!***FIRST EXECUTABLE STATEMENT  DHEQR
      IF (IJOB .GT. 1) GO TO 70
!   -------------------------------------------------------------------
!         A new facorization is desired.
!   -------------------------------------------------------------------
!         QR decomposition without pivoting.
!
      INFO = 0
      DO 60 K = 1, N
         KM1 = K - 1
         KP1 = K + 1
!
!           Compute K-th column of R.
!           First, multiply the K-th column of a by the previous
!           K-1 Givens rotations.
!
         IF (KM1 .LT. 1) GO TO 20
         DO J = 1, KM1
            I = 2*(J-1) + 1
            T1 = A(J,K)
            T2 = A(J+1,K)
            C = Q(I)
            S = Q(I+1)
            A(J,K) = C*T1 - S*T2
            A(J+1,K) = S*T1 + C*T2
         end do
!
!         Compute Givens components C and S.
!
 20      CONTINUE
         IQ = 2*KM1 + 1
         T1 = A(K,K)
         T2 = A(KP1,K)
         IF( T2.EQ.0.0D0 ) THEN
            C = 1.0D0
            S = 0.0D0
         ELSEIF( ABS(T2).GE.ABS(T1) ) THEN
            T = T1/T2
            S = -1.0D0/DSQRT(1.0D0+T*T)
            C = -S*T
         ELSE
            T = T2/T1
            C = 1.0D0/DSQRT(1.0D0+T*T)
            S = -C*T
         end if
         Q(IQ) = C
         Q(IQ+1) = S
         A(K,K) = C*T1 - S*T2
         IF( A(K,K).EQ.0.0D0 ) INFO = K
 60   CONTINUE
      RETURN
!   -------------------------------------------------------------------
!         The old factorization of a will be updated.  A row and a
!         column has been added to the matrix A.  N by N-1 is now
!         the old size of the matrix.
!   -------------------------------------------------------------------
 70   CONTINUE
      NM1 = N - 1
!   -------------------------------------------------------------------
!         Multiply the new column by the N previous Givens rotations.
!   -------------------------------------------------------------------
      DO K = 1,NM1
         I = 2*(K-1) + 1
         T1 = A(K,N)
         T2 = A(K+1,N)
         C = Q(I)
         S = Q(I+1)
         A(K,N) = C*T1 - S*T2
         A(K+1,N) = S*T1 + C*T2
      end do
!   -------------------------------------------------------------------
!         Complete update of decomposition by forming last Givens
!         rotation, and multiplying it times the column
!         vector(A(N,N),A(NP1,N)).
!   -------------------------------------------------------------------
      INFO = 0
      T1 = A(N,N)
      T2 = A(N+1,N)
      IF ( T2.EQ.0.0D0 ) THEN
         C = 1.0D0
         S = 0.0D0
      ELSEIF( ABS(T2).GE.ABS(T1) ) THEN
         T = T1/T2
         S = -1.0D0/DSQRT(1.0D0+T*T)
         C = -S*T
      ELSE
         T = T2/T1
         C = 1.0D0/DSQRT(1.0D0+T*T)
         S = -C*T
      end if
      IQ = 2*N - 1
      Q(IQ) = C
      Q(IQ+1) = S
      A(N,N) = C*T1 - S*T2
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
!------------- LAST LINE OF DHEQR FOLLOWS ----------------------------
      END
      SUBROUTINE DORTH(VNEW, V, HES, N, LL, LDHES, KMP, SNORMW)
!***BEGIN PROLOGUE  DORTH
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DORTH-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Generalized Minimum Residual
!***AUTHOR  Brown, Peter,    (LLNL), brown@lll-crg.llnl.gov
!           Hindmarsh, Alan, (LLNL), alanh@lll-crg.llnl.gov
!           Seager, Mark K., (LLNL), seager@lll-crg.llnl.gov
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!***PURPOSE  Internal routine for DGMRES.
!***DESCRIPTION
!        This routine  orthogonalizes  the  vector  VNEW  against the
!        previous KMP  vectors in the   V array.  It uses  a modified
!        gram-schmidt   orthogonalization procedure with  conditional
!        reorthogonalization.
!
! *Usage:
!      INTEGER N, LL, LDHES, KMP
!      DOUBLE PRECISION VNEW, V, HES, SNORMW
!      DIMENSION VNEW(1), V(N,1), HES(LDHES,1)
!
!      CALL DORTH(VNEW, V, HES, N, LL, LDHES, KMP, SNORMW)
!
! *Arguments:
! VNEW   :INOUT    Double Precision VNEW(N)
!         On input, the vector of length n containing a scaled
!         product of the jacobian and the vector v(*,ll).
!         On output, the new vector orthogonal to v(*,i0) to v(*,ll),
!         where i0 = max(1, ll-kmp+1).
! V      :IN       Double Precision V(N,1)
!         The n x ll array containing the previous ll
!         orthogonal vectors v(*,1) to v(*,ll).
! HES    :INOUT    Double Precision HES(LDHES,1)
!         On input, an LL x LL upper hessenberg matrix containing,
!         in HES(I,K), K.lt.LL, the scaled inner products of
!         A*V(*,K) and V(*,i).
!         On return, column LL of HES is filled in with
!         the scaled inner products of A*V(*,LL) and V(*,i).
! LDHES  :IN       Integer
!         The leading dimension of the HES array.
! N      :IN       Integer
!         The order of the matrix A, and the length of VNEW.
! LL     :IN       Integer
!         The current order of the matrix HES.
! KMP    :IN       Integer
!         The number of previous vectors the new vector VNEW
!         must be made orthogonal to (KMP .le. MAXL).
! SNORMW :OUT      DOUBLE PRECISION
!         Scalar containing the l-2 norm of VNEW.
!
! *See Also:
!         DGMRES
!
!***ROUTINES CALLED  DAXPY
!***END PROLOGUE  DORTH
!         The following is for optimized compilation on LLNL/LTSS Crays.
!LLL. OPTIMIZE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, LL, LDHES, KMP
      DOUBLE PRECISION VNEW, V, HES, SNORMW
      DIMENSION VNEW(1), V(N,1), HES(LDHES,1)
!
!         Internal variables.
!
      INTEGER I, I0
      DOUBLE PRECISION ARG, SUMDSQ, TEM, VNRM
!
!         Get norm of unaltered VNEW for later use.
!***FIRST EXECUTABLE STATEMENT  DORTH
      VNRM = DNRM2(N, VNEW, 1)
!   -------------------------------------------------------------------
!         Perform the modified gram-schmidt procedure on VNEW =A*V(LL).
!         Scaled inner products give new column of HES.
!         Projections of earlier vectors are subtracted from VNEW.
!   -------------------------------------------------------------------
      I0 = MAX0(1,LL-KMP+1)
      DO I = I0,LL
         HES(I,LL) = DDOT(N, V(1,I), 1, VNEW, 1)
         TEM = -HES(I,LL)
         CALL DAXPY(N, TEM, V(1,I), 1, VNEW, 1)
      end do
!   -------------------------------------------------------------------
!         Compute SNORMW = norm of VNEW.  If VNEW is small compared
!         to its input value (in norm), then reorthogonalize VNEW to
!         V(*,1) through V(*,LL).  Correct if relative correction
!         exceeds 1000*(unit roundoff).  Finally, correct SNORMW using
!         the dot products involved.
!   -------------------------------------------------------------------
      SNORMW = DNRM2(N, VNEW, 1)
      IF (VNRM + 0.001D0*SNORMW .NE. VNRM) RETURN
      SUMDSQ = 0.0D0
      DO 30 I = I0,LL
         TEM = -DDOT(N, V(1,I), 1, VNEW, 1)
         IF (HES(I,LL) + 0.001D0*TEM .EQ. HES(I,LL)) GO TO 30
         HES(I,LL) = HES(I,LL) - TEM
         CALL DAXPY(N, TEM, V(1,I), 1, VNEW, 1)
         SUMDSQ = SUMDSQ + TEM**2
 30   CONTINUE
      IF (SUMDSQ .EQ. 0.0D0) RETURN
      ARG = MAX(0.0D0,SNORMW**2 - SUMDSQ)
      SNORMW = DSQRT(ARG)
!
      RETURN
!------------- LAST LINE OF DORTH FOLLOWS ----------------------------
      END
      SUBROUTINE DPIGMR(N, R0, SR, SZ, JSCAL, MAXL, MAXLP1, KMP, &
           NRSTS, JPRE, MATVEC, MSOLVE, NMSL, Z, V, HES, Q, LGMR, &
           RPAR, IPAR, WK, DL, RHOL, NRMAX, B, BNRM, X, XL, &
           ITOL, TOL, NELT, IA, JA, A, ISYM, IUNIT, IFLAG, ERR)
!***BEGIN PROLOGUE  DPIGMR
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DPIGMR-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Generalized Minimum Residual
!***AUTHOR  Brown, Peter,    (LLNL), brown@lll-crg.llnl.gov
!           Hindmarsh, Alan, (LLNL), alanh@lll-crg.llnl.gov
!           Seager, Mark K., (LLNL), seager@lll-crg.llnl.gov
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!***PURPOSE  Internal routine for DGMRES.
!***DESCRIPTION
!         This routine solves the linear system A * Z = R0 using a
!         scaled preconditioned version of the generalized minimum
!         residual method.  An initial guess of Z = 0 is assumed.
!
! *Usage:
!      EXTERNAL MATVEC, MSOLVE
!      INTEGER N,MAXL,MAXLP1,KMP,JPRE,NMSL,LGMR,IPAR,IFLAG,JSCAL,NRSTS
!      INTEGER NRMAX,ITOL,NELT,ISYM
!      DOUBLE PRECISION R0,SR,SZ,Z,V,HES,Q,RPAR,WK,DL,RHOL,BNRM,TOL,
!     $     A,B,X, R0(1), SR(1), SZ(1), Z(1), V(N,1),
!     $     HES(MAXLP1,1), Q(1), RPAR(1), IPAR(1), WK(1), DL(1),
!     $     IA(NELT), JA(NELT), A(NELT), B(1), X(1), XL(1)
!
!      CALL DPIGMR(N, R0, SR, SZ, JSCAL, MAXL, MAXLP1, KMP,
!     $     NRSTS, JPRE, MATVEC, MSOLVE, NMSL, Z, V, HES, Q, LGMR,
!     $     RPAR, IPAR, WK, DL, RHOL, NRMAX, B, BNRM, X, XL,
!     $     ITOL, TOL, NELT, IA, JA, A, ISYM, IUNIT, IFLAG, ERR)
!
! *Arguments:
! R0     :IN       Double Precision R0(N)
!         R0 = the right hand side of the system A*Z = R0.
!         R0 is also used as work space when computing
!         the final approximation.
!         (R0 is the same as V(*,MAXL+1) in the call to DPIGMR.)
! SR     :IN       Double Precision SR(N)
!         SR is a vector of length N containing the nonzero
!         elements of the diagonal scaling matrix for R0.
! SZ     :IN       Double Precision SZ(N)
!         SZ is a vector of length N containing the nonzero
!         elements of the diagonal scaling matrix for Z.
! JSCAL  :IN       Integer
!         A flag indicating whether arrays SR and SZ are used.
!         JSCAL=0 means SR and SZ are not used and the
!                 algorithm will perform as if all
!                 SR(i) = 1 and SZ(i) = 1.
!         JSCAL=1 means only SZ is used, and the algorithm
!                 performs as if all SR(i) = 1.
!         JSCAL=2 means only SR is used, and the algorithm
!                 performs as if all SZ(i) = 1.
!         JSCAL=3 means both SR and SZ are used.
! N      :IN       Integer
!         The order of the matrix A, and the lengths
!         of the vectors SR, SZ, R0 and Z.
! MAXL   :IN       Integer
!         The maximum allowable order of the matrix H.
! MAXLP1 :IN       Integer
!         MAXPL1 = MAXL + 1, used for dynamic dimensioning of HES.
! KMP    :IN       Integer
!         The number of previous vectors the new vector VNEW
!         must be made orthogonal to.  (KMP .le. MAXL)
! NRSTS  :IN       Integer
!         Counter for the number of restarts on the current
!         call to DGMRES.  If NRSTS .gt. 0, then the residual
!         R0 is already scaled, and so scaling of it is
!         not necessary.
! JPRE   :IN       Integer
!         Preconditioner type flag.
! WK     :IN       Double Precision WK(N)
!         A double precision work array of length N used by routine
!         MATVEC
!         and MSOLVE.
! DL     :INOUT    Double Precision DL(N)
!         On input, a double precision work array of length N used for
!         calculation of the residual norm RHO when the method is
!         incomplete (KMP.lt.MAXL), and/or when using restarting.
!         On output, the scaled residual vector RL.  It is only loaded
!         when performing restarts of the Krylov iteration.
! NRMAX  :IN       Integer
!         The maximum number of restarts of the Krylov iteration.
!         NRMAX .gt. 0 means restarting is active, while
!         NRMAX = 0 means restarting is not being used.
! B      :IN       Double Precision B(N)
!         The right hand side of the linear system A*X = B.
! BNRM   :IN       Double Precision
!         The scaled norm of b.
! X      :IN       Double Precision X(N)
!         The current approximate solution as of the last
!         restart.
! XL     :IN       Double Precision XL(N)
!         An array of length N used to hold the approximate
!         solution X(L) when ITOL=11.
! ITOL   :IN       Integer
!         A flag to indicate the type of convergence criterion
!         used.  see the driver for its description.
! TOL    :IN       Double Precision
!         The tolerance on residuals R0-A*Z in scaled norm.
! NELT   :IN       Integer
!         The length of arrays IA, JA and A.
! IA     :IN       Integer IA(NELT)
!         An integer array of length NELT containing matrix data.
!         It is passed directly to the MATVEC and MSOLVE routines.
! JA     :IN       Integer JA(NELT)
!         An integer array of length NELT containing matrix data.
!         It is passed directly to the MATVEC and MSOLVE routines.
! A      :IN       Double Precision A(NELT)
!         A double precision array of length NELT containing matrix
!         data. It is passed directly to the MATVEC and MSOLVE routines.
! ISYM   :IN       Integer
!         A flag to indicate symmetric matrix storage.
!         If ISYM=0, all nonzero entries of the matrix are
!         stored.  If ISYM=1, the matrix is symmetric and
!         only the upper or lower triangular part is stored.
! IUNIT  :IN       Integer
!         The i/o unit number for writing intermediate residual
!         norm values.
! Z      :OUT      Double Precision Z(N)
!         The final computed approximation to the solution
!         of the system A*Z = R0.
! LGMR   :OUT      Integer
!         The number of iterations performed and
!         the current order of the upper hessenberg
!         matrix HES.
! RPAR   :IN       Double Precision RPAR(*)
!         Double Precision work space passed directly to the MSOLVE
!         routine.
! IPAR   :IN       Integer IPAR(*)
!         Integer work space passed directly to the MSOLVE
!         routine.
! NMSL   :OUT      Integer
!         The number of calls to MSOLVE.
! V      :OUT      Double Precision V(N,MAXLP1)
!         The N by (LGMR+1) array containing the LGMR
!         orthogonal vectors V(*,1) to V(*,LGMR).
! HES    :OUT      Double Precision HES(MAXLP1,MAXL)
!         The upper triangular factor of the QR decomposition
!         of the (LGMR+1) by LGMR upper Hessenberg matrix whose
!         entries are the scaled inner-products of A*V(*,I)
!         and V(*,K).
! Q      :OUT      Double Precision Q(2*MAXL)
!         A double precision array of length 2*MAXL containing the
!         components of the Givens rotations used in the QR
!         decomposition of HES.  It is loaded in DHEQR and used in
!         DHELS.
! RHOL   :OUT      Double Precision
!         A double precision scalar containing the norm of the final
!         residual.
! IFLAG  :OUT      Integer
!         An integer error flag..
!         0 means convergence in LGMR iterations, LGMR.le.MAXL.
!         1 means the convergence test did not pass in MAXL
!           iterations, but the residual norm is .lt. norm(R0),
!           and so Z is computed.
!         2 means the convergence test did not pass in MAXL
!           iterations, residual .ge. norm(R0), and Z = 0.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
!
! *See Also:
!         DGMRES
!
!***ROUTINES CALLED  ISDGMR, MATVEC, MSOLVE, DORTH, DRLCAL, DHELS,
!                    DHEQR, DXLCAL, DAXPY, DCOPY, DSCAL,
!***END PROLOGUE  DPIGMR
!         The following is for optimized compilation on LLNL/LTSS Crays.
!LLL. OPTIMIZE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      EXTERNAL MATVEC, MSOLVE
      INTEGER N,MAXL,MAXLP1,KMP,JPRE,NMSL,LGMR,IFLAG,JSCAL,NRSTS
      INTEGER NRMAX,ITOL,NELT,ISYM
      DOUBLE PRECISION RHOL, BNRM, TOL
      DOUBLE PRECISION R0(*), SR(*), SZ(*), Z(*), V(N,*)
      DOUBLE PRECISION HES(MAXLP1,*), Q(*), RPAR(*), WK(*), DL(*)
      DOUBLE PRECISION A(NELT), B(*), X(*), XL(*)
      INTEGER IPAR(*), IA(NELT), JA(NELT)
!
!         Local variables.
!
      INTEGER I, INFO, IP1, I2, J, K, LL, LLP1
      DOUBLE PRECISION R0NRM,C,DLNRM,PROD,RHO,S,SNORMW,TEM
!
!         Zero out the z array.
!***FIRST EXECUTABLE STATEMENT  DPIGMR

      Z(1:n) = 0.0D0

      IFLAG = 0
      LGMR = 0
      NMSL = 0
!         Load ITMAX, the maximum number of iterations.
      ITMAX =(NRMAX+1)*MAXL
!   -------------------------------------------------------------------
!         The initial residual is the vector R0.
!         Apply left precon. if JPRE < 0 and this is not a restart.
!         Apply scaling to R0 if JSCAL = 2 or 3.
!   -------------------------------------------------------------------
      IF ((JPRE .LT. 0) .AND.(NRSTS .EQ. 0)) THEN
         CALL DCOPY(N, R0, 1, WK, 1)
         CALL MSOLVE(N, WK, R0, NELT, IA, JA, A, ISYM, RPAR, IPAR)
         NMSL = NMSL + 1
      end if
      IF (((JSCAL.EQ.2) .OR.(JSCAL.EQ.3)) .AND.(NRSTS.EQ.0)) THEN

         V(1:n,1) = R0(1:n) * SR(1:n)

      ELSE

         V(1:n,1) = R0(1:n)

      end if

      R0NRM = DNRM2(N, V, 1)
      ITER = NRSTS*MAXL
!
!         Call stopping routine ISDGMR.
!
      IF (ISDGMR(N, B, X, XL, NELT, IA, JA, A, ISYM, MSOLVE, &
          NMSL, ITOL, TOL, ITMAX, ITER, ERR, IUNIT, V(1,1), Z, WK, &
          RPAR, IPAR, R0NRM, BNRM, SR, SZ, JSCAL, &
          KMP, LGMR, MAXL, MAXLP1, V, Q, SNORMW, PROD, R0NRM, &
          HES, JPRE) .NE. 0) RETURN
      TEM = 1.0D0/R0NRM
      CALL DSCAL(N, TEM, V(1,1), 1)
!
!         Zero out the HES array.
!
      HES(1:maxlp1,1:maxl) = 0.0D0

!   -------------------------------------------------------------------
!         main loop to compute the vectors V(*,2) to V(*,MAXL).
!         The running product PROD is needed for the convergence test.
!   -------------------------------------------------------------------
      PROD = 1.0D0
      DO 90 LL = 1,MAXL
         LGMR = LL
!   -------------------------------------------------------------------
!        Unscale  the  current V(LL)  and store  in WK.  Call routine
!        msolve    to   compute(M-inverse)*WK,   where    M   is  the
!        preconditioner matrix.  Save the answer in Z.   Call routine
!        MATVEC to compute  VNEW  = A*Z,  where  A is  the the system
!        matrix.  save the answer in  V(LL+1).  Scale V(LL+1).   Call
!        routine DORTH  to  orthogonalize the    new vector VNEW   =
!        V(*,LL+1).  Call routine DHEQR to update the factors of HES.
!   -------------------------------------------------------------------
        IF ((JSCAL .EQ. 1) .OR.(JSCAL .EQ. 3)) THEN
           WK(1:n) = V(1:n,LL)/SZ(1:n)
        ELSE
           CALL DCOPY(N, V(1,LL), 1, WK, 1)
        end if

        IF (JPRE .GT. 0) THEN
           CALL MSOLVE(N, WK, Z, NELT, IA, JA, A, ISYM, RPAR, IPAR)
           NMSL = NMSL + 1
           CALL MATVEC(N, Z, V(1,LL+1), NELT, IA, JA, A, ISYM)
        ELSE
           CALL MATVEC(N, WK, V(1,LL+1), NELT, IA, JA, A, ISYM)
        end if
        IF (JPRE .LT. 0) THEN
           CALL DCOPY(N, V(1,LL+1), 1, WK, 1)
           CALL MSOLVE(N,WK,V(1,LL+1),NELT,IA,JA,A,ISYM,RPAR,IPAR)
           NMSL = NMSL + 1
        end if

        IF ((JSCAL .EQ. 2) .OR.(JSCAL .EQ. 3)) THEN
           V(1:n,LL+1) = V(1:n,LL+1)*SR(1:n)
        end if

        CALL DORTH(V(1,LL+1), V, HES, N, LL, MAXLP1, KMP, SNORMW)
        HES(LL+1,LL) = SNORMW
        CALL DHEQR(HES, MAXLP1, LL, Q, INFO, LL)
        IF (INFO .EQ. LL) GO TO 120
!   -------------------------------------------------------------------
!         Update RHO, the estimate of the norm of the residual R0-A*ZL.
!         If KMP <  MAXL, then the vectors V(*,1),...,V(*,LL+1) are not
!         necessarily orthogonal for LL > KMP.  The vector DL must then
!         be computed, and its norm used in the calculation of RHO.
!   -------------------------------------------------------------------
        PROD = PROD*Q(2*LL)
        RHO = ABS(PROD*R0NRM)
        IF ((LL.GT.KMP) .AND.(KMP.LT.MAXL)) THEN
           IF (LL .EQ. KMP+1) THEN
              CALL DCOPY(N, V(1,1), 1, DL, 1)
              DO 75 I = 1,KMP
                 IP1 = I + 1
                 I2 = I*2
                 S = Q(I2)
                 C = Q(I2-1)
                 DO 70 K = 1,N
                    DL(K) = S*DL(K) + C*V(K,IP1)
 70              CONTINUE
 75           CONTINUE
           end if
           S = Q(2*LL)
           C = Q(2*LL-1)/SNORMW
           LLP1 = LL + 1
           DL(1:n) = S*DL(1:n) + C*V(1:n,LLP1)
           DLNRM = DNRM2(N, DL, 1)
           RHO = RHO*DLNRM
        end if
        RHOL = RHO
!   -------------------------------------------------------------------
!         Test for convergence.  If passed, compute approximation ZL.
!         If failed and LL < MAXL, then continue iterating.
!   -------------------------------------------------------------------
        ITER = NRSTS*MAXL + LGMR
        IF (ISDGMR(N, B, X, XL, NELT, IA, JA, A, ISYM, MSOLVE, &
            NMSL, ITOL, TOL, ITMAX, ITER, ERR, IUNIT, DL, Z, WK, &
            RPAR, IPAR, RHOL, BNRM, SR, SZ, JSCAL, &
            KMP, LGMR, MAXL, MAXLP1, V, Q, SNORMW, PROD, R0NRM, &
            HES, JPRE) .NE. 0) GO TO 200
        IF (LL .EQ. MAXL) GO TO 100
!   -------------------------------------------------------------------
!         Rescale so that the norm of V(1,LL+1) is one.
!   -------------------------------------------------------------------
        TEM = 1.0D0/SNORMW
        CALL DSCAL(N, TEM, V(1,LL+1), 1)
 90   CONTINUE
 100  CONTINUE
      IF (RHO .LT. R0NRM) GO TO 150
 120  CONTINUE
      IFLAG = 2
!
!         Load approximate solution with zero.
!
      Z(1:n) = 0.D0

      RETURN
 150  IFLAG = 1
!
!         Tolerance not met, but residual norm reduced.
!
      IF (NRMAX .GT. 0) THEN
!
!        If performing restarting (NRMAX > 0)  calculate the residual
!        vector RL and  store it in the DL  array.  If the incomplete
!        version is being used (KMP < MAXL) then DL has  already been
!        calculated up to a scaling factor.   Use DRLCAL to calculate
!        the scaled residual vector.
!
         CALL DRLCAL(N, KMP, MAXL, MAXL, V, Q, DL, SNORMW, PROD, &
              R0NRM)
      end if
!   -------------------------------------------------------------------
!         Compute the approximation ZL to the solution.  Since the
!         vector Z was used as work space, and the initial guess
!         of the linear iteration is zero, Z must be reset to zero.
!   -------------------------------------------------------------------
 200  CONTINUE
      LL = LGMR
      LLP1 = LL + 1
      R0(1:llp1) = 0.0D0
      R0(1) = R0NRM
      CALL DHELS(HES, MAXLP1, LL, Q, R0)

      Z(1:n) = 0.0D0

      DO 230 I = 1,LL
         CALL DAXPY(N, R0(I), V(1,I), 1, Z, 1)
 230  CONTINUE
      IF ((JSCAL .EQ. 1) .OR.(JSCAL .EQ. 3)) THEN
         Z(1:n) = Z(1:n) / SZ(1:n)
      end if
      IF (JPRE .GT. 0) THEN
         CALL DCOPY(N, Z, 1, WK, 1)
         CALL MSOLVE(N, WK, Z, NELT, IA, JA, A, ISYM, RPAR, IPAR)
         NMSL = NMSL + 1
      end if
      RETURN
!------------- LAST LINE OF DPIGMR FOLLOWS ----------------------------
      END
      SUBROUTINE DRLCAL(N, KMP, LL, MAXL, V, Q, RL, SNORMW, PROD, &
           R0NRM)
!***BEGIN PROLOGUE  DRLCAL
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DRLCAL-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Generalized Minimum Residual
!***AUTHOR  Brown, Peter,    (LLNL), brown@lll-crg.llnl.gov
!           Hindmarsh, Alan, (LLNL), alanh@lll-crg.llnl.gov
!           Seager, Mark K., (LLNL), seager@lll-crg.llnl.gov
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!***PURPOSE  Internal routine for DGMRES.
!***DESCRIPTION
!         This routine calculates the scaled residual RL from the
!         V(I)'s.
! *Usage:
!      INTEGER N, KMP, LL, MAXL
!      DOUBLE PRECISION SNORMW
!      DOUBLE PRECISION V(N,1), Q(1), RL(N)
!
!      CALL DRLCAL(N, KMP, LL, MAXL, V, Q, RL, SNORMW, PROD,
!     $     R0NRM)
!
! *Arguments:
! N      :IN       Integer
!         The order of the matrix A, and the lengths
!         of the vectors SR, SZ, R0 and Z.
! KMP    :IN       Integer
!         The number of previous V vectors the new vector VNEW
!         must be made orthogonal to. (KMP .le. MAXL)
! LL     :IN       Integer
!         The current dimension of the Krylov subspace.
! MAXL   :IN       Integer
!         The maximum dimension of the Krylov subspace.
! Q      :IN       Double Precision Q(2*MAXL)
!         A double precision array of length 2*MAXL containing the
!         components of the Givens rotations used in the QR
!         decomposition of HES.  It is loaded in DHEQR and used in
!         DHELS.
! PROD   :IN       Double Precision
!        The product s1*s2*...*sl = the product of the sines of the
!        givens rotations used in the QR factorization of
!        the hessenberg matrix HES.
! R0NRM  :IN       Double Precision
!         The scaled norm of initial residual R0.
! RL     :OUT      Double Precision RL(N)
!         The residual vector RL.  This is either SB*(B-A*XL) if
!         not preconditioning or preconditioning on the right,
!         or SB*(M-inverse)*(B-A*XL) if preconditioning on the
!         left.
!
! *See Also:
!         DGMRES
!
!***ROUTINES CALLED  DCOPY, DSCAL
!***END PROLOGUE  DRLCAL
!         The following is for optimized compilation on LLNL/LTSS Crays.
!LLL. OPTIMIZE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, KMP, LL, MAXL
      DOUBLE PRECISION SNORMW
      DOUBLE PRECISION V(N,*), Q(*), RL(N)
!
!         Internal Variables.
!
      INTEGER I, IP1, I2, K
!
!***FIRST EXECUTABLE STATEMENT  DRLCAL
      IF (KMP .EQ. MAXL) THEN
!
!         calculate RL.  Start by copying V(*,1) into RL.
!
         CALL DCOPY(N, V(1,1), 1, RL, 1)
         LLM1 = LL - 1
         DO 20 I = 1,LLM1
            IP1 = I + 1
            I2 = I*2
            S = Q(I2)
            C = Q(I2-1)

            RL(1:n) = S*RL(1:n) + C*V(1:n,IP1)

 20      CONTINUE
         S = Q(2*LL)
         C = Q(2*LL-1)/SNORMW
         LLP1 = LL + 1
         RL(1:n) = S*RL(1:n) + C*V(1:n,LLP1)
      end if
!
!         When KMP < MAXL, RL vector already partially calculated.
!         Scale RL by R0NRM*PROD to obtain the residual RL.
!
      TEM = R0NRM*PROD
      CALL DSCAL(N, TEM, RL, 1)
      RETURN
!------------- LAST LINE OF DRLCAL FOLLOWS ----------------------------
      END
      SUBROUTINE DXLCAL(N, LGMR, X, XL, ZL, HES, MAXLP1, Q, V, R0NRM, &
           WK, SZ, JSCAL, JPRE, MSOLVE, NMSL, RPAR, IPAR, &
           NELT, IA, JA, A, ISYM)
!***BEGIN PROLOGUE  DXLCAL
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DXLCAL-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Generalized Minimum Residual
!***AUTHOR  Brown, Peter,    (LLNL), brown@lll-crg.llnl.gov
!           Hindmarsh, Alan, (LLNL), alanh@lll-crg.llnl.gov
!           Seager, Mark K., (LLNL), seager@lll-crg.llnl.gov
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!***PURPOSE  Internal routine for DGMRES.
!***DESCRIPTION
!        This  routine computes the solution  XL,  the current DGMRES
!        iterate, given the  V(I)'s and  the  QR factorization of the
!        Hessenberg  matrix HES.   This routine  is  only called when
!        ITOL=11.
!
! *Usage:
!      EXTERNAL MSOLVE
!      DOUBLE PRECISION R0NRM
!      DOUBLE PRECISION X(N), XL(N), ZL(N), HES(MAXLP1,1), Q(1)
!      DOUBLE PRECISION V(N,1), WK(N), SZ(1), RPAR(1)
!      DOUBLE PRECISION A(NELT)
!      INTEGER N, LGMR, MAXLP1, JSCAL, JPRE, IPAR, NMSL, NELT, ISYM
!      INTEGER IPAR(1), IA(NELT), JA(NELT)
!
!      CALL DXLCAL(N, LGMR, X, XL, ZL, HES, MAXLP1, Q, V, R0NRM,
!     $     WK, SZ, JSCAL, JPRE, MSOLVE, NMSL, RPAR, IPAR,
!     $     NELT, IA, JA, A, ISYM)
!
! *Arguments:
! N      :IN       Integer
!         The order of the matrix A, and the lengths
!         of the vectors SR, SZ, R0 and Z.
! LGMR   :IN       Integer
!         The number of iterations performed and
!         the current order of the upper Hessenberg
!         matrix HES.
! X      :IN       Double Precision X(N)
!         The current approximate solution as of the last restart.
! ZL     :IN       Double Precision ZL(N)
!         An array of length N used to hold the approximate
!         solution Z(L).
! SZ     :IN       Double Precision SZ(N)
!         A vector of length N containing the nonzero
!         elements of the diagonal scaling matrix for Z.
! JSCAL  :IN       Integer
!         A flag indicating whether arrays SR and SZ are used.
!         JSCAL=0 means SR and SZ are not used and the
!                 algorithm will perform as if all
!                 SR(i) = 1 and SZ(i) = 1.
!         JSCAL=1 means only SZ is used, and the algorithm
!                 performs as if all SR(i) = 1.
!         JSCAL=2 means only SR is used, and the algorithm
!                 performs as if all SZ(i) = 1.
!         JSCAL=3 means both SR and SZ are used.
! MAXLP1 :IN       Integer
!         MAXLP1 = MAXL + 1, used for dynamic dimensioning of HES.
!         MAXL is the maximum allowable order of the matrix HES.
! JPRE   :IN       Integer
!         The preconditioner type flag.
! WK     :IN       Double Precision WK(N)
!         A double precision work array of length N.
! NMSL   :IN       Integer
!         The number of calls to MSOLVE.
! V      :IN       Double Precision V(N,MAXLP1)
!         The N by(LGMR+1) array containing the LGMR
!         orthogonal vectors V(*,1) to V(*,LGMR).
! HES    :IN       Double Precision HES(MAXLP1,MAXL)
!         The upper triangular factor of the QR decomposition
!         of the (LGMR+1) by LGMR upper Hessenberg matrix whose
!         entries are the scaled inner-products of A*V(*,i) and V(*,k).
! Q      :IN       Double Precision Q(2*MAXL)
!         A double precision array of length 2*MAXL containing the
!         components of the givens rotations used in the QR
!         decomposition of HES.  It is loaded in DHEQR.
! R0NRM  :IN       Double Precision
!         The scaled norm of the initial residual for the
!         current call to DPIGMR.
! RPAR   :IN       Double Precision RPAR(*)
!         Double Precision work space passed directly to the MSOLVE
!         routine.
! IPAR   :IN       Integer IPAR(*)
!         Integer work space passed directly to the MSOLVE
!         routine.
! NELT   :IN       Integer
!         The length of arrays IA, JA and A.
! IA     :IN       Integer IA(NELT)
!         An integer array of length NELT containing matrix data.
!         It is passed directly to the MATVEC and MSOLVE routines.
! JA     :IN       Integer JA(NELT)
!         An integer array of length NELT containing matrix data.
!         It is passed directly to the MATVEC and MSOLVE routines.
! A      :IN       Double Precision A(NELT)
!         A double precision array of length NELT containing matrix
!         data.
!         It is passed directly to the MATVEC and MSOLVE routines.
! ISYM   :IN       Integer
!         A flag to indicate symmetric matrix storage.
!         If ISYM=0, all nonzero entries of the matrix are
!         stored.  If ISYM=1, the matrix is symmetric and
!         only the upper or lower triangular part is stored.
! XL     :OUT      Double Precision XL(N)
!         An array of length N used to hold the approximate
!         solution X(L).
!         Warning: XL and ZL are the same array in the calling routine.
!
! *See Also:
!         DGMRES
!
!***ROUTINES CALLED  MSOLVE, DHELS, DAXPY, DCOPY, DSCAL
!***END PROLOGUE  DXLCAL
!         The following is for optimized compilation on LLNL/LTSS Crays.
!LLL. OPTIMIZE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      EXTERNAL MSOLVE
      INTEGER N, LGMR, MAXLP1, JSCAL, JPRE, IPAR(*), NMSL, NELT
      INTEGER IA(NELT), JA(NELT), ISYM
      DOUBLE PRECISION R0NRM, X(N), XL(N), ZL(N), HES(MAXLP1,*)
      DOUBLE PRECISION Q(*), V(N,*), WK(N), SZ(*), RPAR(*), A(NELT)
!
!         Internal variables.
!
      INTEGER I, K, LL, LLP1
!
!***FIRST EXECUTABLE STATEMENT  DXLCAL

      LL = LGMR
      LLP1 = LL + 1

      WK(1:llp1) = 0.0D0

      WK(1) = R0NRM
      CALL DHELS(HES, MAXLP1, LL, Q, WK)
      ZL(1:n) = 0.0D0

      DO I = 1,LL
        CALL DAXPY(N, WK(I), V(1,I), 1, ZL, 1)
      end do

      IF ((JSCAL .EQ. 1) .OR.(JSCAL .EQ. 3)) THEN
        ZL(1:n) = ZL(1:n) / SZ(1:n)
      end if

      IF (JPRE .GT. 0) THEN
         CALL DCOPY(N, ZL, 1, WK, 1)
         CALL MSOLVE(N, WK, ZL, NELT, IA, JA, A, ISYM, RPAR, IPAR)
         NMSL = NMSL + 1
      end if
!
!  Calculate XL from X and ZL.
!
      XL(1:n) = X(1:n) + ZL(1:n)

      RETURN
      END
      FUNCTION ISDGMR(N, B, X, XL, NELT, IA, JA, A, ISYM, MSOLVE, &
           NMSL, ITOL, TOL, ITMAX, ITER, ERR, IUNIT, R, Z, DZ, &
           RWORK, IWORK, RNRM, BNRM, SB, SX, JSCAL, &
           KMP, LGMR, MAXL, MAXLP1, V, Q, SNORMW, PROD, R0NRM, &
           HES, JPRE)
!***BEGIN PROLOGUE ISDGMR
!***DATE WRITTEN   890404  (YYMMDD)
!***REVISION DATE  890404  (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=INTEGER(ISDGMR-I)
!             Linear system, Sparse, Stop Test, GMRES
!***AUTHOR  Brown, Peter,    (LLNL), brown@lll-crg.llnl.gov
!           Hindmarsh, Alan, (LLNL), alanh@lll-crg.llnl.gov
!           Seager, Mark K., (LLNL), seager@lll-crg.llnl.gov
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!***PURPOSE Generalized Minimum Residual Stop Test.
!         This routine calculates the stop test for the Generalized
!         Minimum RESidual (GMRES) iteration scheme.   It returns a
!         nonzero if  the  error  estimate (the  type  of  which is
!         determined  by   ITOL)  is  less  than the user specified
!         tolerence TOL.
!***DESCRIPTION
! *Usage:
!      INTEGER KMP, LGMR, MAXL, MAXLP1, JPRE, NMSL
!      DOUBLE PRECISION DXNRM, RNRM, R0NRM, SNORMW, SOLNRM, PROD
!      DOUBLE PRECISION B(1), X(1), IA(1), JA(1), A(1), R(1), Z(1)
!      DOUBLE PRECISION DZ(1), RWORK(1), IWORK(1), SB(1), SX(1)
!      DOUBLE PRECISION Q(1), V(N,1), HES(MAXLP1,MAXL), XL(1)
!      EXTERNAL MSOLVE
!
!      IF (ISDGMR(N, B, X, XL, NELT, IA, JA, A, ISYM, MSOLVE,
!     $     NMSL, ITOL, TOL, ITMAX, ITER, ERR, IUNIT, R, Z, DZ,
!     $     RWORK, IWORK, RNRM, BNRM, SB, SX, JSCAL,
!     $     KMP, LGMR, MAXL, MAXLP1, V, Q, SNORMW, PROD, R0NRM,
!     $     HES, JPRE) .NE. 0) THEN ITERATION DONE
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand-side vector.
! X      :IN       Double Precision X(N).
!         Approximate solution vector as of the last restart.
! XL     :OUT      Double Precision XL(N)
!         An array of length N used to hold the approximate
!         solution as of the current iteration.  Only computed by
!         this routine when ITOL=11.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See "Description", in the DGMRES,
!         DSLUGM and DSDGMR routines for more late breaking details...
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system Mz = r for  z
!         given r with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays.  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSLOVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is the right-hand side
!         vector, and z is the solution upon return.  RWORK is a
!         double precision
!         array that can be used to pass necessary preconditioning
!         information and/or workspace to MSOLVE.  IWORK is an integer
!         work array for the same purpose as RWORK.
! NMSL   :INOUT    Integer.
!         A counter for the number of calls to MSOLVE.
! ITOL   :IN       Integer.
!         Flag to indicate the type of convergence criterion used.
!         ITOL=0  Means the  iteration stops when the test described
!                 below on  the  residual RL  is satisfied.  This is
!                 the  "Natural Stopping Criteria" for this routine.
!                 Other values  of   ITOL  cause  extra,   otherwise
!                 unnecessary, computation per iteration and     are
!                 therefore  much less  efficient.  See  ISDGMR (the
!                 stop test routine) for more information.
!         ITOL=1  Means   the  iteration stops   when the first test
!                 described below on  the residual RL  is satisfied,
!                 and there  is either right  or  no preconditioning
!                 being used.
!         ITOL=2  Implies     that   the  user    is   using    left
!                 preconditioning, and the second stopping criterion
!                 below is used.
!         ITOL=3  Means the  iteration stops   when  the  third test
!                 described below on Minv*Residual is satisfied, and
!                 there is either left  or no  preconditioning begin
!                 used.
!         ITOL=11 is    often  useful  for   checking  and comparing
!                 different routines.  For this case, the  user must
!                 supply  the  "exact" solution or  a  very accurate
!                 approximation (one with  an  error much less  than
!                 TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!                 if ITOL=11, iteration stops when the 2-norm of the
!                 difference between the iterative approximation and
!                 the user-supplied solution  divided by the  2-norm
!                 of the  user-supplied solution  is  less than TOL.
!                 Note that this requires  the  user to  set up  the
!                 "COMMON     /SOLBLK/ SOLN(LENGTH)"  in the calling
!                 routine.  The routine with this declaration should
!                 be loaded before the stop test so that the correct
!                 length is used by  the loader.  This procedure  is
!                 not standard Fortran and may not work correctly on
!                 your   system (although  it  has  worked  on every
!                 system the authors have tried).  If ITOL is not 11
!                 then this common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :IN       Integer.
!         The iteration for which to check for convergence.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.  Letting norm() denote the Euclidean
!         norm, ERR is defined as follows..
!
!         If ITOL=0, then ERR = norm(SB*(B-A*X(L)))/norm(SB*B),
!                               for right or no preconditioning, and
!                         ERR = norm(SB*(M-inverse)*(B-A*X(L)))/
!                                norm(SB*(M-inverse)*B),
!                               for left preconditioning.
!         If ITOL=1, then ERR = norm(SB*(B-A*X(L)))/norm(SB*B),
!                               since right or no preconditioning
!                               being used.
!         If ITOL=2, then ERR = norm(SB*(M-inverse)*(B-A*X(L)))/
!                                norm(SB*(M-inverse)*B),
!                               since left preconditioning is being
!                               used.
!         If ITOL=3, then ERR =  Max  |(Minv*(B-A*X(L)))(i)/x(i)|
!                               i=1,n
!         If ITOL=11, then ERR = norm(SB*(X(L)-SOLN))/norm(SB*SOLN).
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :INOUT    Double Precision R(N).
!         Work array used in calling routine.  It contains
!         information necessary to compute the residual RL = B-A*XL.
! Z      :WORK     Double Precision Z(N).
!         Workspace used to hold the pseudo-residule M z = r.
! DZ     :WORK     Double Precision DZ(N).
!         Workspace used to hold temporary vector(s).
! RWORK  :WORK     Double Precision RWORK(USER DEFINABLE).
!         Double Precision array that can be used by MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINABLE).
!         Integer array that can be used by MSOLVE.
! RNRM   :IN       Double Precision.
!         Norm of the current residual.  Type of norm depends on ITOL.
! BNRM   :IN       Double Precision.
!         Norm of the right hand side.  Type of norm depends on ITOL.
! SB     :IN       Double Precision SB(N).
!         Scaling vector for B.
! SX     :IN       Double Precision SX(N).
!         Scaling vector for X.
! JSCAL  :IN       Integer.
!         Flag indicating if scaling arrays SB and SX are being
!         used in the calling routine DPIGMR.
!         JSCAL=0 means SB and SX are not used and the
!                 algorithm will perform as if all
!                 SB(i) = 1 and SX(i) = 1.
!         JSCAL=1 means only SX is used, and the algorithm
!                 performs as if all SB(i) = 1.
!         JSCAL=2 means only SB is used, and the algorithm
!                 performs as if all SX(i) = 1.
!         JSCAL=3 means both SB and SX are used.
! KMP    :IN       Integer
!         The number of previous vectors the new vector VNEW
!         must be made orthogonal to.  (KMP .le. MAXL)
! LGMR   :IN       Integer
!         The number of GMRES iterations performed on the current call
!         to DPIGMR (i.e., # iterations since the last restart) and
!         the current order of the upper hessenberg
!         matrix HES.
! MAXL   :IN       Integer
!         The maximum allowable order of the matrix H.
! MAXLP1 :IN       Integer
!         MAXPL1 = MAXL + 1, used for dynamic dimensioning of HES.
! V      :IN       Double Precision V(N,MAXLP1)
!         The N by (LGMR+1) array containing the LGMR
!         orthogonal vectors V(*,1) to V(*,LGMR).
! Q      :IN       Double Precision Q(2*MAXL)
!         A double precision array of length 2*MAXL containing the
!         components of the Givens rotations used in the QR
!         decomposition
!         of HES.
! SNORMW :IN       Double Precision
!         A scalar containing the scaled norm of VNEW before it
!         is renormalized in DPIGMR.
! PROD   :IN       Double Precision
!        The product s1*s2*...*sl = the product of the sines of the
!        givens rotations used in the QR factorization of
!        the hessenberg matrix HES.
! R0NRM  :IN       Double Precision
!         The scaled norm of initial residual R0.
! HES    :IN       Double Precision HES(MAXLP1,MAXL)
!         The upper triangular factor of the QR decomposition
!         of the (LGMR+1) by LGMR upper Hessenberg matrix whose
!         entries are the scaled inner-products of A*V(*,I)
!         and V(*,K).
! JPRE   :IN       Integer
!         Preconditioner type flag.
!
! *Description
!       When using the GMRES solver,  the preferred value  for ITOL
!       is 0.  This is due to the fact that when ITOL=0 the norm of
!       the residual required in the stopping test is  obtained for
!       free, since this value is already  calculated  in the GMRES
!       algorithm.   The  variable  RNRM contains the   appropriate
!       norm, which is equal to norm(SB*(RL - A*XL))  when right or
!       no   preconditioning is  being  performed,   and equal   to
!       norm(SB*Minv*(RL - A*XL))  when using left preconditioning.
!       Here, norm() is the Euclidean norm.  Nonzero values of ITOL
!       require  additional work  to  calculate the  actual  scaled
!       residual  or its scaled/preconditioned  form,  and/or   the
!       approximate solution XL.  Hence, these values of  ITOL will
!       not be as efficient as ITOL=0.
!
!***ROUTINES CALLED     MSOLVE, DNRM2, DCOPY,
!***END PROLOG ISDGMR
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER KMP, LGMR, MAXL, MAXLP1, JPRE, NMSL
      DOUBLE PRECISION DXNRM, RNRM, R0NRM, SNORMW, SOLNRM, PROD
      DOUBLE PRECISION B(*), X(*)
      integer IA(*), JA(*)
      double precision A(*), R(*), Z(*), DZ(*)
      DOUBLE PRECISION RWORK(*)
      integer IWORK(*)
      double precision SB(*), SX(*), Q(*), V(N,*)
      DOUBLE PRECISION HES(MAXLP1,MAXL), XL(*)
      EXTERNAL MSOLVE
      COMMON /SOLBLK/ SOLN(1)
      SAVE SOLNRM
!
!***FIRST EXECUTABLE STATEMENT ISDGMR
      ISDGMR = 0
      IF ( ITOL.EQ.0 ) THEN
!
!       Use input from DPIGMR to determine if stop conditions are met.
!
         ERR = RNRM/BNRM
      end if
      IF ( (ITOL.GT.0) .AND. (ITOL.LE.3) ) THEN
!
!       Use DRLCAL to calculate the scaled residual vector.
!       Store answer in R.
!
         IF ( LGMR.NE.0 ) CALL DRLCAL(N, KMP, LGMR, MAXL, V, Q, R, &
                                      SNORMW, PROD, R0NRM)
         IF ( ITOL.LE.2 ) THEN
!         err = ||Residual||/||RightHandSide||(2-Norms).
            ERR = DNRM2(N, R, 1)/BNRM
!
!         Unscale R by R0NRM*PROD when KMP < MAXL.
!
            IF ( (KMP.LT.MAXL) .AND. (LGMR.NE.0) ) THEN
               TEM = 1.0D0/(R0NRM*PROD)
               CALL DSCAL(N, TEM, R, 1)
            end if
         ELSEIF ( ITOL.EQ.3 ) THEN
!         err = Max |(Minv*Residual)(i)/x(i)|
!         When jpre .lt. 0, r already contains Minv*Residual.
            IF ( JPRE.GT.0 ) THEN
               CALL MSOLVE(N, R, DZ, NELT, IA, JA, A, ISYM, RWORK, &
                    IWORK)
               NMSL = NMSL + 1
            end if
!
!         Unscale R by R0NRM*PROD when KMP < MAXL.
!
            IF ( (KMP.LT.MAXL) .AND. (LGMR.NE.0) ) THEN
               TEM = 1.0D0/(R0NRM*PROD)
               CALL DSCAL(N, TEM, R, 1)
            end if

            FUZZ = tiny ( fuzz )
            IELMAX = 1
            RATMAX = ABS(DZ(1))/MAX(ABS(X(1)),FUZZ)
            DO 25 I = 2, N
               RAT = ABS(DZ(I))/MAX(ABS(X(I)),FUZZ)
               IF( RAT.GT.RATMAX ) THEN
                  IELMAX = I
                  RATMAX = RAT
               end if
 25         CONTINUE
            ERR = RATMAX
            IF( RATMAX.LE.TOL ) ISDGMR = 1
            IF( IUNIT.GT.0 ) WRITE(IUNIT,1020) ITER, IELMAX, RATMAX
            RETURN
         end if
      end if
      IF ( ITOL.EQ.11 ) THEN
!
!       Use DXLCAL to calculate the approximate solution XL.
!
         IF ( (LGMR.NE.0) .AND. (ITER.GT.0) ) THEN
            CALL DXLCAL(N, LGMR, X, XL, XL, HES, MAXLP1, Q, V, R0NRM, &
                 DZ, SX, JSCAL, JPRE, MSOLVE, NMSL, RWORK, IWORK, &
                 NELT, IA, JA, A, ISYM)
         ELSEIF ( ITER.EQ.0 ) THEN
!         Copy X to XL to check if initial guess is good enough.
            CALL DCOPY(N, X, 1, XL, 1)
         ELSE
!         Return since this is the first call to DPIGMR on a restart.
            RETURN
         end if
!
         IF ((JSCAL .EQ. 0) .OR.(JSCAL .EQ. 2)) THEN
!         err = ||x-TrueSolution||/||TrueSolution||(2-Norms).
            IF ( ITER.EQ.0 ) SOLNRM = DNRM2(N, SOLN, 1)
            DO 30 I = 1, N
               DZ(I) = XL(I) - SOLN(I)
 30         CONTINUE
            ERR = DNRM2(N, DZ, 1)/SOLNRM
         ELSE
            IF (ITER .EQ. 0) THEN
               SOLNRM = 0.D0
               DO 40 I = 1,N
                  SOLNRM = SOLNRM + (SX(I)*SOLN(I))**2
 40            CONTINUE
               SOLNRM = DSQRT(SOLNRM)
            end if
            DXNRM = 0.D0
            DO 50 I = 1,N
               DXNRM = DXNRM + (SX(I)*(XL(I)-SOLN(I)))**2
 50         CONTINUE
            DXNRM = DSQRT(DXNRM)
!         err = ||SX*(x-TrueSolution)||/||SX*TrueSolution|| (2-Norms).
            ERR = DXNRM/SOLNRM
         end if
      end if
!
      IF( IUNIT.NE.0 ) THEN
         IF( ITER.EQ.0 ) THEN
            WRITE(IUNIT,1000) N, ITOL, MAXL, KMP
         end if
         WRITE(IUNIT,1010) ITER, RNRM/BNRM, ERR
      end if
      IF ( ERR.LE.TOL ) ISDGMR = 1
!
      RETURN
 1000 FORMAT(' Generalized Minimum Residual(',I3,I3,') for ', &
           'N, ITOL = ',I5, I5, &
           /' ITER','   Natral Err Est','   Error Estimate')
 1010 FORMAT(1X,I4,1X,E16.7,1X,E16.7)
 1020 FORMAT(1X,' ITER = ',I5, ' IELMAX = ',I5, &
           ' |R(IELMAX)/X(IELMAX)| = ',E12.5)
!------------- LAST LINE OF ISDGMR FOLLOWS ----------------------------
      END
      SUBROUTINE DIR(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, DZ, &
           RWORK, IWORK)
!***BEGIN PROLOGUE  DIR
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DIR-D),
!             Linear system, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned Iterative Refinement sparse Ax = b solver.
!            Routine to solve a general linear system  Ax = b  using
!            iterative refinement with a matrix splitting.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER  ITER, IERR, IUNIT, IWORK(USER DEFINABLE)
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N), DZ(N)
!     DOUBLE PRECISION RWORK(USER DEFINABLE)
!     EXTERNAL MATVEC, MSOLVE
!
!     CALL DIR(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSLOVE, ITOL,
!    $     TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, DZ, RWORK, IWORK)
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Integer A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See "Description", below
!         for more late breaking details...
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MATVEC :EXT      External.
!         Name of a routine which performs the matrix vector multiply
!         Y = A*X given A and X.  The name of the MATVEC routine must
!         be declared external in the calling program.  The calling
!         sequence to MATVEC is:
!             CALL MATVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!         Where N is the number of unknowns, Y is the product A*X
!         upon return, X is an input vector, NELT is the number of
!         non-zeros in the SLAP IA, JA, A storage for the matrix A.
!         ISYM is a flag which, if non-zero, denotes that A is
!         symmetric and only the lower or upper triangle is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R for
!         Z given R with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays).  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSOLVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is the right-hand side
!         vector, and Z is the solution upon return.  IA, JA, A and
!         ISYM are defined as above.  RWORK is a double precision array
!         that can be used to pass necessary preconditioning information
!         and/or workspace to MSOLVE.  IWORK is an integer work array
!         for the same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :WORK     Double Precision R(N).
! Z      :WORK     Double Precision Z(N).
! DZ     :WORK     Double Precision DZ(N).
! RWORK  :WORK     Double Precision RWORK(USER DEFINABLE).
!         Double Precision array that can be used by  MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINABLE).
!         Integer array that can be used by  MSOLVE.
!
! *Description:
!       The basic algorithm for iterative refinement (also known as
!       iterative improvement) is:
!
!            n+1    n    -1       n
!           X    = X  + M  (B - AX  ).
!
!           -1   -1
!       If M =  A then   this is  the standard iterative  refinement
!       algorithm and the "subtraction" in  the residual calculation
!       should be done in double precision (which it is  not in this
!       routine).  If M = DIAG(A), the diagonal of A, then iterative
!       refinement is known  as Jacobi's  method.   The SLAP routine
!       DSJAC  implements this iterative strategy.   If  M = L,  the
!       lower  triangle of A,  then iterative refinement is known as
!       Gauss-Seidel.   The    SLAP  routine  DSGS  implements  this
!       iterative strategy.
!
!       This routine does  not care  what matrix data   structure is
!       used for  A and M.  It simply   calls  the MATVEC and MSOLVE
!       routines, with  the arguments as  described above.  The user
!       could write any type of structure and the appropriate MATVEC
!       and MSOLVE routines.  It is assumed  that A is stored in the
!       IA, JA, A  arrays in some fashion and  that M (or INV(M)) is
!       stored  in  IWORK  and  RWORK)  in  some fashion.   The SLAP
!       routines DSJAC and DSGS are examples of this procedure.
!
!       Two  examples  of  matrix  data structures  are the: 1) SLAP
!       Triad  format and 2) SLAP Column format.
!
!       =================== S L A P Triad format ===================
!
!       In  this   format only the  non-zeros are  stored.  They may
!       appear  in *ANY* order.   The user  supplies three arrays of
!       length NELT, where  NELT  is the number  of non-zeros in the
!       matrix:  (IA(NELT), JA(NELT),  A(NELT)).  For each  non-zero
!       the  user puts   the row  and  column index   of that matrix
!       element in the IA and JA arrays.  The  value of the non-zero
!       matrix  element is  placed in  the corresponding location of
!       the A  array.  This is  an extremely easy data  structure to
!       generate.  On  the other hand it  is  not too  efficient  on
!       vector  computers   for the  iterative  solution  of  linear
!       systems.  Hence, SLAP  changes this input  data structure to
!       the SLAP   Column  format for the  iteration (but   does not
!       change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Examples:
!       See the SLAP routines DSJAC, DSGS
!
! *Precision:           Double Precision
! *See Also:
!       DSJAC, DSGS
!***REFERENCES  1. Gene Golub \& Charles Van Loan, "Matrix
!                 Computations", John Hopkins University Press; 3
!                 (1983) IBSN 0-8018-3010-9.
!***ROUTINES CALLED  MATVEC, MSOLVE, ISDIR.
!***END PROLOGUE  DIR
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
      INTEGER ITOL, ITMAX, ITER, IERR, IUNIT, IWORK(*)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N)
      DOUBLE PRECISION DZ(N), RWORK(*)
      EXTERNAL MSOLVE, MATVEC, ISDIR
!
!         Check some of the input data.
!***FIRST EXECUTABLE STATEMENT  DIR
      ITER = 0
      IERR = 0
      IF( N.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      TOLMIN = 500.0 * epsilon ( tolmin )
      IF( TOL.LT.TOLMIN ) THEN
         TOL = TOLMIN
         IERR = 4
      end if
!
!         Calculate initial residual and pseudo-residual, and check
!         stopping criterion.
      CALL MATVEC(N, X, R, NELT, IA, JA, A, ISYM)

      R(1:n) = B(1:n) - R(1:n)

      CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!
      IF( ISDIR(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, R, Z, DZ, RWORK, &
           IWORK, BNRM, SOLNRM) .NE. 0 ) GO TO 200
      IF( IERR.NE.0 ) RETURN
!
!         ***** iteration loop *****
!
      DO 100 K=1,ITMAX

         ITER = K
!
!         Calculate new iterate x, new residual r, and new
!         pseudo-resid z.
         X(1:n) = X(1:n) + Z(1:n)

         CALL MATVEC(N, X, R, NELT, IA, JA, A, ISYM)

         R(1:n) = B(1:n) - R(1:n)

         CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!
!         check stopping criterion.
         IF( ISDIR(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL, &
              ITMAX, ITER, ERR, IERR, IUNIT, R, Z, DZ, RWORK, &
              IWORK, BNRM, SOLNRM) .NE. 0 ) GO TO 200
!
 100  CONTINUE
!
!         *****   end of loop  *****
!         Stopping criterion not satisfied.
      ITER = ITMAX + 1
      IERR = 2
!
 200  RETURN
!------------- LAST LINE OF DIR FOLLOWS -------------------------------
      END
      SUBROUTINE DSJAC(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSJAC
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSJAC-D),
!             Linear system, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Jacobi's method iterative sparse Ax = b solver.
!            Routine to solve a general linear system  Ax = b  using
!            Jacobi iteration.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER  ITER, IERR, IUNIT, LENW, IWORK(LENIW), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(LENW)
!
!     CALL DSJAC(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Integer A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "Description",
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.  LENW >= 4*N.
! IWORK  :WORK     Integer IWORK(LENIW).
!         Used to hold pointers into the double precision workspace,
!         RWORK. Upon return the following locations of IWORK hold
!         information which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.  LENIW >= 10.
!
! *Description:
!       Jacobi's method solves the linear system Ax=b with the
!       basic iterative method (where A = L + D + U):
!
!            n+1    -1       n    n
!           X    = D  (B - LX - UX )
!
!                   n    -1       n
!                = X  + D  (B - AX )
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out which one
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A) is modified internally to be
!       the SLAP Column format.  See above.
!
! *See Also:
!       DSGS, DIR
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DS2Y, DDCHKW, DSDS, DIR, DSMV, DSDI
!***END PROLOGUE  DSJAC
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
      INTEGER ITER, IUNIT, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(NELT), RWORK(LENW)
      EXTERNAL DSMV, DSDI
      PARAMETER(LOCRB=1,LOCIB=11)
!
!         Compute the inverse of the diagonal of the matrix.  This
!         will be used as the precontioner.
!***FIRST EXECUTABLE STATEMENT  DSJAC
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      LOCIW = LOCIB
      LOCD = LOCRB
      LOCR = LOCD + N
      LOCZ = LOCR + N
      LOCDZ = LOCZ + N
      LOCW = LOCDZ + N
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSJAC', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(4) = LOCD
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
      CALL DS2Y(N, NELT, IA, JA, A, ISYM )
      CALL DSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCD))
!
!         Set up the work array and perform the iterative refinement.
      CALL DIR(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSDI, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK(LOCR), RWORK(LOCZ), &
           RWORK(LOCDZ), RWORK, IWORK )
      RETURN
!------------- LAST LINE OF DSJAC FOLLOWS -----------------------------
      END
      SUBROUTINE DSGS(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSGS
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSGS-S),
!             Linear system, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Gauss-Seidel method iterative sparse Ax = b solver.
!            Routine to solve a general linear system  Ax = b  using
!            Gauss-Seidel iteration.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK(NEL+2*N+1), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(NEL+3*N)
!
!     CALL DSGS(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Integer A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "Description",
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the lower
!         lower triangle of the matrix is stored.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.  NEL is the number
!         of non-zeros in the lower triangle of the matrix (including
!         the diagonal).
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= NEL+3*N.
! IWORK  :WORK     Integer IWORK(LENIW).
!         Integer array used for workspace.  NEL is the number of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal).
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.  LENIW >=
!         NEL+N+11.
!
! *Description
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A) is modified internally to be
!       the SLAP Column format.  See above.
!
! *See Also:
!       DSJAC, DIR
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DS2Y, DCHKW, DS2LT, SDIR, DSMV, DSLI
!***END PROLOGUE  DSGS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
      INTEGER ITER, IUNIT, IWORK(10)
      DOUBLE PRECISION B(N), X(N), A(N), TOL, ERR, RWORK(1)
      EXTERNAL DSMV, DSLI
      PARAMETER(LOCRB=1,LOCIB=11)
!
!         Modify the SLAP matrix data structure to YSMP-Column.
!***FIRST EXECUTABLE STATEMENT  DSGS
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Count number of elements in lower triangle of the matrix.
      IF( ISYM.EQ.0 ) THEN
         NEL = 0
         DO 20 ICOL = 1, N
            JBGN = JA(ICOL)
            JEND = JA(ICOL+1)-1
            DO J = JBGN, JEND
               IF( IA(J).GE.ICOL ) NEL = NEL + 1
            end do
 20      CONTINUE
      ELSE
         NEL = JA(N+1)-1
      end if
!
!         Set up the work arrays.  Then store the lower triangle of
!         the matrix.
!
      LOCJEL = LOCIB
      LOCIEL = LOCJEL + N+1
      LOCIW = LOCIEL + NEL
!
      LOCEL = LOCRB
      LOCR = LOCEL + NEL
      LOCZ = LOCR + N
      LOCDZ = LOCZ + N
      LOCW = LOCDZ + N
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSGS', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(1) = NEL
      IWORK(2) = LOCIEL
      IWORK(3) = LOCJEL
      IWORK(4) = LOCEL
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
      CALL DS2LT( N, NELT, IA, JA, A, ISYM, NEL, IWORK(LOCIEL), &
           IWORK(LOCJEL), RWORK(LOCEL) )
!
!         Call iterative refinement routine.
      CALL DIR(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSLI, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK(LOCR), &
           RWORK(LOCZ), RWORK(LOCDZ), RWORK, IWORK )
!
!         Set the amount of Integer and Double Precision Workspace used.
      IWORK(9) = LOCIW+N+NELT
      IWORK(10) = LOCW+NELT
      RETURN
!------------- LAST LINE OF DSGS FOLLOWS ------------------------------
      END
      SUBROUTINE DSILUR(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSILUR
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSILUR-S),
!             Linear system, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Incomplete LU Iterative Refinement sparse Ax = b solver.
!            Routine to solve a general linear system  Ax = b  using
!            the incomplete LU decomposition with iterative refinement.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK(NEL+NU+4*N+2), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(NEL+NU+4*N)
!
!     CALL DSILUR(N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW)
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Integer A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "Description",
!         below.  If the SLAP Triad format is chosen it is changed
!         internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Matrix A is not Positive Definite.
!                       $(p,Ap) < 0.0$.
!           IERR = 7 => Incomplete factorization broke down
!                       and was fudged.  Resulting preconditioning may
!                       be less than the best.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.  NEL is the number
!         of non-zeros in the lower triangle of the matrix (including
!         the diagonal).  NU is the number of nonzeros in the upper
!         triangle of the matrix (including the diagonal).
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= NEL+NU+4*N.
! IWORK  :WORK     Integer IWORK(LENIW).
!         Integer array used for workspace.  NEL is the number of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal).  NU is the number of nonzeros in the upper
!         triangle of the matrix (including the diagonal).
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the integer workspace, IWORK.  LENIW >=
!         NEL+NU+4*N+10.
!
! *Description
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A) is modified internally to be
!       the SLAP Column format.  See above.
!
! *Portability:
!       DSJAC, DSGS, DIR
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DS2Y, DCHKW, DSILUS, DIR, DSMV, DSLUI
!***END PROLOGUE  DSILUR
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
      INTEGER IERR, IUNIT, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, RWORK(LENW)
      PARAMETER (LOCRB=1, LOCIB=11)
!
      EXTERNAL DSMV, DSLUI
!
!         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
!***FIRST EXECUTABLE STATEMENT  DSILUR
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Count number of Non-Zero elements in preconditioner ILU
!         matrix.  Then set up the work arrays.
      NL = 0
      NU = 0
      DO 20 ICOL = 1, N
!         Don't count diagonal.
         JBGN = JA(ICOL)+1
         JEND = JA(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
!VD$ NOVECTOR
            DO J = JBGN, JEND
               IF( IA(J).GT.ICOL ) THEN
                  NL = NL + 1
                  IF( ISYM.NE.0 ) NU = NU + 1
               ELSE
                  NU = NU + 1
               end if
            end do
         end if
 20   CONTINUE
!
      LOCIL = LOCIB
      LOCJL = LOCIL + N+1
      LOCIU = LOCJL + NL
      LOCJU = LOCIU + NU
      LOCNR = LOCJU + N+1
      LOCNC = LOCNR + N
      LOCIW = LOCNC + N
!
      LOCL = LOCRB
      LOCDIN = LOCL + NL
      LOCU = LOCDIN + N
      LOCR = LOCU + NU
      LOCZ = LOCR + N
      LOCDZ = LOCZ + N
      LOCW = LOCDZ + N
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSILUR', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(1) = LOCIL
      IWORK(2) = LOCJL
      IWORK(3) = LOCIU
      IWORK(4) = LOCJU
      IWORK(5) = LOCL
      IWORK(6) = LOCDIN
      IWORK(7) = LOCU
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
!         Compute the Incomplete LU decomposition.
      CALL DSILUS( N, NELT, IA, JA, A, ISYM, NL, IWORK(LOCIL), &
           IWORK(LOCJL), RWORK(LOCL), RWORK(LOCDIN), NU, IWORK(LOCIU), &
           IWORK(LOCJU), RWORK(LOCU), IWORK(LOCNR), IWORK(LOCNC) )
!
!         Do the Preconditioned Iterative Refinement iteration.
      CALL DIR(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSLUI, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK(LOCR), &
           RWORK(LOCZ), RWORK(LOCDZ), RWORK, IWORK)
      RETURN
!------------- LAST LINE OF DSILUR FOLLOWS ----------------------------
      END
      FUNCTION ISDIR(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL, &
           ITMAX, ITER, ERR, IERR, IUNIT, R, Z, DZ, RWORK, IWORK, &
           BNRM, SOLNRM)
!***BEGIN PROLOGUE  ISDIR
!***REFER TO  DIR, DSJAC, DSGS
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  880320   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(ISDIR-S),
!             Linear system, Sparse, Stop Test
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned Iterative Refinement Stop Test.
!            This routine calculates the stop test for the iterative
!            refinement iteration scheme.  It returns a nonzero if the
!            error estimate (the type of which is determined by ITOL)
!            is less than the user specified tolerance TOL.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
!     INTEGER IERR, IUNIT, IWORK(USER DEFINED)
!     DOUBLE PRECISION B(N), X(N), A(N), TOL, ERR, R(N), Z(N), DZ(N)
!     DOUBLE PRECISION RWORK(USER DEFINED), BNRM, SOLNRM
!     EXTERNAL MSOLVE
!
!     IF( ISDIR(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, R, Z, DZ, RWORK, IWORK,
!    $     BNRM, SOLNRM) .NE. 0 ) THEN ITERATION DONE
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :IN       Double Precision X(N).
!         The current approximate solution vector.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See "C *Description" in the
!         DIR routine.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system Mz = r for
!         z given r with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays.  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSOLVE is:
!             CALL MSOLVE(N, R, Z, RWORK, IWORK)
!         Where N is the number of unknowns, R is the right-hand side
!         vector, and z is the solution upon return.  RWORK is a double
!         precision array that can be used to pass necessary
!         preconditioning information and/or workspace to MSOLVE.
!         IWORK is an integer work array for the same purpose as RWORK.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITER   :IN       Integer.
!         Current iteration count.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in the X(N) approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Error flag.  IERR is set to 3 if ITOL is not on of the
!         acceptable values, see above.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :IN       Double Precision R(N).
!         The residual R = B-AX.
! Z      :WORK     Double Precision Z(N).
!         Workspace used to hold the pseudo-residual M z = r.
! DZ     :WORK     Double Precision DZ(N).
!         Workspace used to hold temporary vector(s).
! RWORK  :WORK     Double Precision RWORK(USER DEFINABLE).
!         Double Precision array that can be used by  MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINABLE).
!         Integer array that can be used by MSOLVE.
! BNRM   :INOUT    Double Precision.
!         Norm of the right hand side.  Type of norm depends on ITOL.
!         Calculated only on the first call.
! SOLNRM :INOUT    Double Precision.
!         2-Norm of the true solution, SOLN.  Only computed and used
!         if ITOL = 11.
!
! *Function Return Values:
!       0 : Error estimate (determined by ITOL) is *NOT* less than the
!           specified tolerance, TOL.  The iteration must continue.
!       1 : Error estimate (determined by ITOL) is less than the
!           specified tolerance, TOL.  The iteration can be considered
!           complete.
!
! *Precision:           Double Precision
! *See Also:
!       DIR, DSJAC, DSGS
!
! *Cautions:
!     This routine will attempt to write to the fortran logical output
!     unit IUNIT, if IUNIT .ne. 0.  Thus, the user must make sure that
!     this  logical  unit  must  be  attached  to  a  file or terminal
!     before calling this routine with a non-zero  value  for   IUNIT.
!     This routine does not check for the validity of a non-zero IUNIT
!     unit number.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  MSOLVE, DNRM2
!***COMMON BLOCKS    SOLBLK
!***END PROLOGUE  ISDIR
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, ITOL, ITMAX, ITER
      INTEGER IUNIT, IWORK(*)
      DOUBLE PRECISION B(N), X(N), A(NELT), R(N), Z(N), DZ(N), RWORK(*)
      EXTERNAL MSOLVE
      COMMON /SOLBLK/ SOLN(1)
!
!***FIRST EXECUTABLE STATEMENT  ISDIR
      ISDIR = 0
      IF( ITOL.EQ.1 ) THEN
!         err = ||Residual||/||RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) BNRM = DNRM2(N, B, 1)
         ERR = DNRM2(N, R, 1)/BNRM
      ELSE IF( ITOL.EQ.2 ) THEN
!                  -1              -1
!         err = ||M  Residual||/||M  RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) THEN
            CALL MSOLVE(N, B, DZ, NELT, IA, JA, A, ISYM, RWORK, IWORK)
            BNRM = DNRM2(N, DZ, 1)
         end if
         ERR = DNRM2(N, Z, 1)/BNRM
      ELSE IF( ITOL.EQ.11 ) THEN
!         err = ||x-TrueSolution||/||TrueSolution|| (2-Norms).
         IF( ITER.EQ.0 ) SOLNRM = DNRM2(N, SOLN, 1)

         DZ(1:n) = X(1:n) - SOLN(1:n)

         ERR = DNRM2(N, DZ, 1)/SOLNRM
      ELSE
!
!         If we get here ITOL is not one of the acceptable values.
         ERR = 1.0E10
         IERR = 3
      end if
!
      IF( IUNIT.NE.0 ) THEN
         WRITE(IUNIT,1000) ITER,ERR
      end if
!
      IF( ERR.LE.TOL ) ISDIR = 1
!
      RETURN
 1000 FORMAT(5X,'ITER = ',I4,' Error Estimate = ',E16.7)
!------------- LAST LINE OF ISDIR FOLLOWS -----------------------------
      END
      SUBROUTINE DBHIN( N, NELT, IA, JA, A, ISYM, SOLN, RHS, &
           IUNIT, JOB )
!***BEGIN PROLOGUE  DBHIN
!***DATE WRITTEN   881107   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DBHIN-D),
!             Linear system, SLAP Sparse, Diagnostics
!***AUTHOR  Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Read a Sparse Linear System in the Boeing/Harwell Format.
!            The matrix is read in and if the right hand side is also
!            present in the input file then it too is read in.
!            The matrix is then modified to be in the SLAP Column
!            format.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, IUNIT, JOB
!     DOUBLE PRECISION A(NELT), SOLN(N), RHS(N)
!
!     CALL DBHIN( N, NELT, IA, JA, A, ISYM, SOLN, RHS, IUNIT, JOB )
!
! *Arguments:
! N      :OUT      Integer
!         Order of the Matrix.
! NELT   :INOUT    Integer.
!         On input NELT is the maximum number of non-zeros that
!         can be stored in the IA, JA, A arrays.
!         On output NELT is the number of non-zeros stored in A.
! IA     :OUT      Integer IA(NELT).
! JA     :OUT      Integer JA(NELT).
! A      :OUT      Double Precision A(NELT).
!         On output these arrays hold the matrix A in the SLAP
!         Triad format.  See "LONG DESCRIPTION", below.
! ISYM   :OUT      Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the lower
!         triangle of the matrix is stored.
! SOLN   :OUT      Double Precision SOLN(N).
!         The solution to the linear system, if present.  This array
!         is accessed if and only if JOB to read it in, see below.
!         If the user requests that SOLN be read in, but it is not in
!         the file, then it is simply zeroed out.
! RHS    :OUT      Double Precision RHS(N).
!         The right hand side vector.  This array is accessed if and
!         only if JOB is set to read it in, see below.
!         If the user requests that RHS be read in, but it is not in
!         the file, then it is simply zeroed out.
! IUNIT  :IN       Integer.
!         Fortran logical I/O device unit number to write the matrix
!         to.  This unit must be connected in a system dependent fashion
!         to a file or the console or you will get a nasty message
!         from the Fortran I/O libraries.
! JOB    :INOUT    Integer.
!         Flag indicating what I/O operations to perform.
!         On input JOB indicates what Input operations to try to
!         perform.
!         JOB = 0 => Read only the matrix.
!             = 1 => Read matrix and RHS (if present).
!             = 2 => Read matrix and SOLN (if present).
!             = 3 => Read matrix, RHS and SOLN (if present).
!         On output JOB indicates what operations were actually
!         performed.
!               -3 => Unable to parse matrix "CODE" from input file
!                     to determine if only the lower triangle of matrix
!                     is stored.
!               -2 => Number of non-zeros (NELT) too large.
!               -1 => System size (N) too large.
!         JOB =  0 => Read in only the matrix.
!             =  1 => Read in the matrix and RHS.
!             =  2 => Read in the matrix and SOLN.
!             =  3 => Read in the matrix, RHS and SOLN.
!             = 10 => Read in only the matrix *STRUCTURE*, but no
!                     non-zero entries.  Hence, A(*) is not referenced
!                     and has the return values the same as the input.
!             = 11 => Read in the matrix *STRUCTURE* and RHS.
!             = 12 => Read in the matrix *STRUCTURE* and SOLN.
!             = 13 => Read in the matrix *STRUCTURE*, RHS and SOLN.
!
! *Precision:           Double Precision
! *Portability:
!         You must make sure that IUNIT is a valid Fortran logical
!         I/O device unit number and that the unit number has been
!         associated with a file or the console.  This is a system
!         dependent function.
!
!***LONG DESCRIPTION
!       The format for the output is as follows.  On  the first line
!       are counters and flags: N, NELT, ISYM, IRHS, ISOLN.  N, NELT
!       and ISYM are described above.  IRHS is  a flag indicating if
!       the RHS was  written out (1 is  yes, 0 is  no).  ISOLN  is a
!       flag indicating if the SOLN was written out  (1 is yes, 0 is
!       no).  The format for the fist line is: 5i10.  Then comes the
!       NELT Triad's IA(I), JA(I) and A(I), I = 1, NELT.  The format
!       for  these lines is   :  1X,I5,1X,I5,1X,E16.7.   Then  comes
!       RHS(I), I = 1, N, if IRHS = 1.  Then  comes SOLN(I), I  = 1,
!       N, if ISOLN = 1.  The format for these lines is: 1X,E16.7.
!
!       =================== S L A P Triad format ===================
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero  matrix  element is  placed   in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DBHIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, JOB
      DOUBLE PRECISION A(NELT), RHS(N), SOLN(N)
!
!         Local Variables
!
      CHARACTER*80  TITLE
      CHARACTER*3   CODE
      CHARACTER*16  PNTFMT, RINFMT
      CHARACTER*20  NVLFMT, RHSFMT
!
      INTEGER NLINE, NPLS, NRILS, NNVLS, NRHSLS, NROW, NCOL, NIND, NELE
!
!         Read Matrices In BOEING-HARWELL format.
!
! NLINE  Number of Data (after the header) lines in the file.
! NPLS   Number of lines for the Column Pointer data in the file.
! NRILS  Number of lines for the Row indicies in the data file.
! NNVLS  Number of lines for the Matrix elements in the data file.
! NRHSLS Number of lines for the RHS in the data file.
!
!***FIRST EXECUTABLE STATEMENT  DBHIN
      READ(IUNIT,9000) TITLE
      READ(IUNIT,9010) NLINE, NPLS, NRILS, NNVLS, NRHSLS
      READ(IUNIT,9020) CODE, NROW, NCOL, NIND, NELE
      READ(IUNIT,9030) PNTFMT, RINFMT, NVLFMT, RHSFMT
!
      IF( NROW.GT.N ) THEN
         N = NROW
         JOBRET = -1
         GOTO 999
      end if
      IF( NIND.GT.NELT ) THEN
         NELT = NIND
         JOBRET = -2
         GOTO 999
      end if
!
!         Set the parameters.
!
      N    = NROW
      NELT = NIND
      IF( CODE.EQ.'RUA' ) THEN
         ISYM = 0
      ELSE IF( CODE.EQ.'RSA' ) THEN
         ISYM = 1
      ELSE
         JOBRET = -3
         GOTO 999
      end if
      READ(IUNIT,PNTFMT) (JA(I), I = 1, N+1)
      READ(IUNIT,RINFMT) (IA(I), I = 1, NELT)
      JOBRET = 10
      IF( NNVLS.GT.0 ) THEN
         READ(IUNIT,NVLFMT) (A(I),  I = 1, NELT)
         JOBRET = 0
      end if
      IF( NRHSLS.GT.0 .AND. MOD(JOB,2).EQ.1 ) THEN
         READ(5,RHSFMT) (RHS(I), I = 1, N)
         JOBRET = JOBRET + 1
      end if
!
!         Now loop thru the IA(i) array making sure that the Diagonal
!         matrix element appears first in the column.  Then sort the
!         rest of the column in ascending order.
!
!VD$R NOCONCUR
!VD$R NOVECTOR
      DO 70 ICOL = 1, N
         IBGN = JA(ICOL)
         IEND = JA(ICOL+1)-1
         DO 30 I = IBGN, IEND
            IF( IA(I).EQ.ICOL ) THEN
!         Swap the diag element with the first element in the column.
               ITEMP = IA(I)
               IA(I) = IA(IBGN)
               IA(IBGN) = ITEMP
               TEMP = A(I)
               A(I) = A(IBGN)
               A(IBGN) = TEMP
               GOTO 40
            end if
 30      CONTINUE
 40      IBGN = IBGN + 1
         IF( IBGN.LT.IEND ) THEN
            DO 60 I = IBGN, IEND
               DO 50 J = I+1, IEND
                  IF( IA(I).GT.IA(J) ) THEN
                     ITEMP = IA(I)
                     IA(I) = IA(J)
                     IA(J) = ITEMP
                     TEMP = A(I)
                     A(I) = A(J)
                     A(J) = TEMP
                  end if
 50            CONTINUE
 60         CONTINUE
         end if
 70   CONTINUE
!
!         Set return flag.
 999  JOB = JOBRET
      RETURN
 9000 FORMAT( A80 )
 9010 FORMAT( 5I14 )
 9020 FORMAT( A3, 11X, 4I14 )
 9030 FORMAT( 2A16, 2A20 )
!------------- LAST LINE OF DBHIN FOLLOWS ------------------------------
      END
      SUBROUTINE DCHKW( NAME, LOCIW, LENIW, LOCW, LENW, &
           IERR, ITER, ERR )
!***BEGIN PROLOGUE  DCHKW
!***DATE WRITTEN   880225   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  R2
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DCHKW-D),
!             SLAP, Error Checking, Workspace Checking
!***AUTHOR  Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP WORK/IWORK Array Bounds Checker.
!            This routine checks the work array lengths  and  inter-
!            faces to the SLATEC  error  handler  if  a  problem  is
!            found.
!***DESCRIPTION
! *Usage:
!     CHARACTER*(*) NAME
!     INTEGER LOCIW, LENIW, LOCW, LENW, IERR, ITER
!     DOUBLE PRECISION ERR
!
!     CALL DCHKW( NAME, LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
!
! *Arguments:
! NAME   :IN       Character*(*).
!         Name of the calling routine.  This is used in the output
!         message, if an error is detected.
! LOCIW  :IN       Integer.
!         Location of the first free element in the integer workspace
!         array.
! LENIW  :IN       Integer.
!         Length of the integer workspace array.
! LOCW   :IN       Integer.
!         Location of the first free element in the double precision
!         workspace array.
! LENRW  :IN       Integer.
!         Length of the double precision workspace array.
! IERR   :OUT      Integer.
!         Return error flag.
!               IERR = 0 => All went well.
!               IERR = 1 => Insufficient storage allocated for
!                           WORK or IWORK.
! ITER   :OUT      Integer.
!         Set to 0 if an error is detected.
! ERR    :OUT      Double Precision.
!         Set to a very large number if an error is detected.
!
! *Precision:           Double Precision
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, XERRWV
!***END PROLOGUE  DCHKW
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*(*) NAME
      CHARACTER*72 MESG
      INTEGER LOCIW, LENIW, LOCW, LENW, IERR, ITER
      DOUBLE PRECISION ERR, D1MACH
      EXTERNAL D1MACH, XERRWV
!
!         Check the Integer workspace situation.
!***FIRST EXECUTABLE STATEMENT  DCHKW
      IERR = 0
      IF( LOCIW.GT.LENIW ) THEN
         IERR = 1
         ITER = 0
         ERR = huge ( err )
         MESG = NAME // ': INTEGER work array too short. '// &
              ' IWORK needs i1: have allocated i2.'
         CALL XERRWV( MESG, LEN(MESG), 1, 1, 2, LOCIW, LENIW, &
              0, 0.0, 0.0 )
      end if
!
!         Check the Double Precision workspace situation.
      IF( LOCW.GT.LENW ) THEN
         IERR = 1
         ITER = 0
         ERR = huge ( err )
         MESG = NAME // ': DOUBLE PRECISION work array too short. '// &
              ' RWORK needs i1: have allocated i2.'
         CALL XERRWV( MESG, LEN(MESG), 1, 1, 2, LOCW, LENW, &
              0, 0.0, 0.0 )
      end if
      RETURN
!------------- LAST LINE OF DCHKW FOLLOWS ----------------------------
      END
      SUBROUTINE QS2I1D( IA, JA, A, N, KFLAG )
!***BEGIN PROLOGUE  QS2I1D
!***DATE WRITTEN   761118   (YYMMDD)
!***REVISION DATE  890125   (YYMMDD)
!***CATEGORY NO.  N6A2A
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=INTEGER(QS2I1D-I),
!             QUICKSORT,DOUBLETON QUICKSORT,SORT,SORTING
!***AUTHOR  Jones, R. E., (SNLA)
!           Kahaner, D. K., (NBS)
!           Seager, M. K., (LLNL) seager@lll-crg.llnl.gov
!           Wisniewski, J. A., (SNLA)
!***PURPOSE  Sort an integer array also moving an integer and DP array
!            This routine sorts the integer  array  IA and makes the
!            same interchanges   in the integer   array  JA  and the
!            double precision array A.  The  array IA may be  sorted
!            in increasing order or decreas- ing  order.  A slightly
!            modified QUICKSORT algorithm is used.
!
!***DESCRIPTION
!     Written by Rondall E Jones
!     Modified by John A. Wisniewski to use the Singleton QUICKSORT
!     algorithm. date 18 November 1976.
!
!     Further modified by David K. Kahaner
!     National Bureau of Standards
!     August, 1981
!
!     Even further modification made to bring the code up to the
!     Fortran 77 level and make it more readable and to carry
!     along one integer array and one double precision array during
!     the sort by
!     Mark K. Seager
!     Lawrence Livermore National Laboratory
!     November, 1987
!     This routine was adapted from the ISORT routine.
!
!     ABSTRACT
!         This routine sorts an integer array IA and makes the same
!         interchanges in the integer array JA and the double precision
!          array A.
!         The array a may be sorted in increasing order or decreasing
!         order.  A slightly modified quicksort algorithm is used.
!
!     DESCRIPTION OF PARAMETERS
!        IA - Integer array of values to be sorted.
!        JA - Integer array to be carried along.
!         A - Double Precision array to be carried along.
!         N - Number of values in integer array IA to be sorted.
!     KFLAG - Control parameter
!           = 1 means sort IA in INCREASING order.
!           =-1 means sort IA in DECREASING order.
!
!***REFERENCES
!     Singleton, R. C., Algorithm 347, "An Efficient Algorithm for
!     Sorting with Minimal Storage", cacm, Vol. 12, No. 3, 1969,
!     Pp. 185-187.
!***ROUTINES CALLED  XERROR
!***END PROLOGUE  QS2I1D
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!VD$R NOVECTOR
!VD$R NOCONCUR
      DIMENSION IL(21),IU(21)
      INTEGER   IA(N),JA(N),IT,IIT,JT,JJT
      DOUBLE PRECISION A(N), TA, TTA
!
!***FIRST EXECUTABLE STATEMENT  QS2I1D
      NN = N
      IF (NN.LT.1) THEN
         CALL XERROR ( &
         'QS2I1D- the number of values to be sorted was not positive.',59,1,1)
         RETURN
      end if
      IF( N.EQ.1 ) RETURN
      KK = IABS(KFLAG)
      IF ( KK.NE.1 ) THEN
         CALL XERROR ( &
           'QS2I1D- the sort control parameter, k, was not 1 or -1.',55,2,1)
         RETURN
      end if
!
!     Alter array IA to get decreasing order if needed.
!
      IF( KFLAG.LT.1 ) THEN
         IA(1:nn) = -IA(1:nn)
      end if
!
!     Sort IA and carry JA and A along.
!     And now...Just a little black magic...
      M = 1
      I = 1
      J = NN
      R = .375
 210  IF( R.LE.0.5898437 ) THEN
         R = R + 3.90625E-2
      ELSE
         R = R-.21875
      end if
 225  K = I
!
!     Select a central element of the array and save it in location
!     it, jt, at.
!
      IJ = I + IDINT( DBLE(J-I)*R )
      IT = IA(IJ)
      JT = JA(IJ)
      TA = A(IJ)
!
!     If first element of array is greater than it, interchange with it.
!
      IF( IA(I).GT.IT ) THEN
         IA(IJ) = IA(I)
         IA(I)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(I)
         JA(I)  = JT
         JT     = JA(IJ)
         A(IJ)  = A(I)
         A(I)   = TA
         TA     = A(IJ)
      end if
      L=J
!
!     If last element of array is less than it, swap with it.
!
      IF( IA(J).LT.IT ) THEN
         IA(IJ) = IA(J)
         IA(J)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(J)
         JA(J)  = JT
         JT     = JA(IJ)
         A(IJ)  = A(J)
         A(J)   = TA
         TA     = A(IJ)
!
!     If first element of array is greater than it, swap with it.
!
         IF ( IA(I).GT.IT ) THEN
            IA(IJ) = IA(I)
            IA(I)  = IT
            IT     = IA(IJ)
            JA(IJ) = JA(I)
            JA(I)  = JT
            JT     = JA(IJ)
            A(IJ)  = A(I)
            A(I)   = TA
            TA     = A(IJ)
         end if
      end if
!
!     Find an element in the second half of the array which is
!     smaller than it.
!
  240 L=L-1
      IF( IA(L).GT.IT ) GO TO 240
!
!     Find an element in the first half of the array which is
!     greater than it.
!
  245 K=K+1
      IF( IA(K).LT.IT ) GO TO 245
!
!     Interchange these elements.
!
      IF( K.LE.L ) THEN
         IIT   = IA(L)
         IA(L) = IA(K)
         IA(K) = IIT
         JJT   = JA(L)
         JA(L) = JA(K)
         JA(K) = JJT
         TTA   = A(L)
         A(L)  = A(K)
         A(K)  = TTA
         GOTO 240
      end if
!
!     Save upper and lower subscripts of the array yet to be sorted.
!
      IF( L-I.GT.J-K ) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      end if
      GO TO 260
!
!     Begin again on another portion of the unsorted array.
!
  255 M = M-1
      IF( M.EQ.0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  260 IF( J-I.GE.1 ) GO TO 225
      IF( I.EQ.J ) GO TO 255
      IF( I.EQ.1 ) GO TO 210
      I = I-1
  265 I = I+1
      IF( I.EQ.J ) GO TO 255
      IT = IA(I+1)
      JT = JA(I+1)
      TA =  A(I+1)
      IF( IA(I).LE.IT ) GO TO 265
      K=I
  270 IA(K+1) = IA(K)
      JA(K+1) = JA(K)
      A(K+1)  =  A(K)
      K = K-1
      IF( IT.LT.IA(K) ) GO TO 270
      IA(K+1) = IT
      JA(K+1) = JT
      A(K+1)  = TA
      GO TO 265
!
!     Clean up, if necessary.
!
  300 IF( KFLAG.LT.1 ) THEN
         DO 310 I=1,NN
            IA(I) = -IA(I)
 310     CONTINUE
      end if
      RETURN
!------------- LAST LINE OF QS2I1D FOLLOWS ----------------------------
      END
      SUBROUTINE DS2Y(N, NELT, IA, JA, A, ISYM )
!***BEGIN PROLOGUE  DS2Y
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DS2Y-D),
!             Linear system, SLAP Sparse
!***AUTHOR  Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP Triad to SLAP Column Format Converter.
!            Routine to convert from the SLAP Triad to SLAP Column
!            format.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
!     DOUBLE PRECISION A(NELT)
!
!     CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! NELT   :IN       Integer.
!         Number of non-zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "LONG
!         DESCRIPTION", below.  If the SLAP Triad format is used
!         this format is translated to the SLAP Column format by
!         this routine.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the lower
!         triangle of the matrix is stored.
!
! *Precision:           Double Precision
!
!***LONG DESCRIPTION
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures.  If the SLAP Triad format is give
!       as input then this routine transforms it into SLAP Column
!       format.  The way this routine tells which format is given as
!       input is to look at JA(N+1).  If JA(N+1) = NELT+1 then we
!       have the SLAP Column format.  If that equality does not hold
!       then it is assumed that the IA, JA, A arrays contain the
!       SLAP Triad format.
!
!       =================== S L A P Triad format ===================
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  QS2I1D
!***END PROLOGUE  DS2Y
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
      DOUBLE PRECISION A(NELT)
!
!         Check to see if the (IA,JA,A) arrays are in SLAP Column
!         format.  If it's not then transform from SLAP Triad.
!***FIRST EXECUTABLE STATEMENT  DS2LT
      IF( JA(N+1).EQ.NELT+1 ) RETURN
!
!         Sort into ascending order by COLUMN (on the ja array).
!         This will line up the columns.
!
      CALL QS2I1D( JA, IA, A, NELT, 1 )
!
!         Loop over each column to see where the column indicies change
!         in the column index array ja.  This marks the beginning of the
!         next column.
!
!VD$R NOVECTOR
      JA(1) = 1
      DO 20 ICOL = 1, N-1
         DO J = JA(ICOL)+1, NELT
            IF( JA(J).NE.ICOL ) THEN
               JA(ICOL+1) = J
               GOTO 20
            end if
         end do
 20   CONTINUE
      JA(N+1) = NELT+1
!
!         Mark the n+2 element so that future calls to a SLAP routine
!         utilizing the YSMP-Column storage format will be able to tell.
!
      JA(N+2) = 0
!
!         Now loop thru the ia(i) array making sure that the Diagonal
!         matrix element appears first in the column.  Then sort the
!         rest of the column in ascending order.
!
      DO 70 ICOL = 1, N
         IBGN = JA(ICOL)
         IEND = JA(ICOL+1)-1
         DO 30 I = IBGN, IEND
            IF( IA(I).EQ.ICOL ) THEN
!         Swap the diag element with the first element in the column.
               ITEMP = IA(I)
               IA(I) = IA(IBGN)
               IA(IBGN) = ITEMP
               TEMP = A(I)
               A(I) = A(IBGN)
               A(IBGN) = TEMP
               GOTO 40
            end if
 30      CONTINUE
 40      IBGN = IBGN + 1
         IF( IBGN.LT.IEND ) THEN
            DO 60 I = IBGN, IEND
               DO 50 J = I+1, IEND
                  IF( IA(I).GT.IA(J) ) THEN
                     ITEMP = IA(I)
                     IA(I) = IA(J)
                     IA(J) = ITEMP
                     TEMP = A(I)
                     A(I) = A(J)
                     A(J) = TEMP
                  end if
 50            CONTINUE
 60         CONTINUE
         end if
 70   CONTINUE
      RETURN
!------------- LAST LINE OF DS2Y FOLLOWS ----------------------------
      END
      SUBROUTINE DCPPLT( N, NELT, IA, JA, A, ISYM, IUNIT )
!***BEGIN PROLOGUE  DCPPLT
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DCPPLT-D),
!             Linear system, SLAP Sparse, Diagnostics
!***AUTHOR  Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Printer Plot of SLAP Column Format Matrix.
!            Routine to print out a SLAP Column format matrix in
!            a "printer plot" graphical representation.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(N+1), ISYM, IUNIT
!     DOUBLE PRECISION A(NELT)
!
!     CALL DCPPLT( N, NELT, IA, JA, A, ISYM, IUNIT )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! NELT   :IN       Integer.
!         Number of non-zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(N+1).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in the SLAP
!         Column format.  See "LONG DESCRIPTION", below.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the lower
!         triangle of the matrix is stored.
! IUNIT  :IN       Integer.
!         Fortran logical I/O device unit number to write the matrix
!         to.  This unit must be connected in a system dependent fashion
!         to a file or the console or you will get a nasty message
!         from the Fortran I/O libraries.
!
! *Precision:           Double Precision
! *Portability:
!         You must make sure that IUNIT is a valid Fortran logical
!         I/O device unit number and that the unit number has been
!         associated with a file or the console.  This is a system
!         dependent function.
!
!***LONG DESCRIPTION
!       This routine prints out a SLAP  Column format matrix  to the
!       Fortran logical I/O unit   number  IUNIT.  The  numbers them
!       selves  are not printed  out, but   rather  a one  character
!       representation of the numbers.   Elements of the matrix that
!       are not represented in the (IA,JA,A)  arrays are  denoted by
!       ' ' character (a blank).  Elements of A that are *ZERO* (and
!       hence  should  really not be  stored) are  denoted  by a '0'
!       character.  Elements of A that are *POSITIVE* are denoted by
!       'D' if they are Diagonal elements  and '#' if  they are off
!       Diagonal  elements.  Elements of  A that are *NEGATIVE* are
!       denoted by 'N'  if they  are Diagonal  elements and  '*' if
!       they are off Diagonal elements.
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DCPPLT
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
      DOUBLE PRECISION A(NELT)
      CHARACTER*225 CHMAT(225)
!
!         Set up the character matrix...
!***FIRST EXECUTABLE STATEMENT  DCPPLT
      NMAX = MIN( 225, N)
      DO I = 1, NMAX
         CHMAT(I)(1:NMAX) = ' '
      end do

      DO 30 ICOL = 1, NMAX
         JBGN = JA(ICOL)
         JEND = JA(ICOL+1)-1
         DO 20 J = JBGN, JEND
            IROW = IA(J)
            IF( IROW.LE.NMAX ) THEN
               IF( ISYM.NE.0 ) THEN
!         Put in non-dym part as well...
                  IF( A(J).EQ.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = '0'
                  ELSEIF( A(J).GT.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = '#'
                  ELSE
                     CHMAT(IROW)(ICOL:ICOL) = '*'
                  end if
               end if
               IF( IROW.EQ.ICOL ) THEN
!         Diagonal entry.
                  IF( A(J).EQ.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = '0'
                  ELSEIF( A(J).GT.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = 'D'
                  ELSE
                     CHMAT(IROW)(ICOL:ICOL) = 'N'
                  end if
               ELSE
!         Off-Diagonal entry
                  IF( A(J).EQ.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = '0'
                  ELSEIF( A(J).GT.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = '#'
                  ELSE
                     CHMAT(IROW)(ICOL:ICOL) = '*'
                  end if
               end if
            end if
 20      CONTINUE
 30   CONTINUE
!
!         Write out the heading.
      WRITE(IUNIT,1000) N, NELT, FLOAT(NELT)/FLOAT(N*N)
      WRITE(IUNIT,1010) (MOD(I,10),I=1,NMAX)
!
!         Write out the character representations matrix elements.
      DO 40 IROW = 1, NMAX
         WRITE(IUNIT,1020) IROW, CHMAT(IROW)(1:NMAX)
 40   CONTINUE
      RETURN
 1000 FORMAT(/'**** Picture of Column SLAP matrix follows ****'/ &
           ' N, NELT and Density = ',2I10,E16.7)
 1010 FORMAT(4X,255(I1))
 1020 FORMAT(1X,I3,A)
!------------- LAST LINE OF DCPPLT FOLLOWS ----------------------------
      END
      SUBROUTINE DTOUT( N, NELT, IA, JA, A, ISYM, SOLN, RHS, &
           IUNIT, JOB )
!***BEGIN PROLOGUE  DTOUT
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DTOUT-D),
!             Linear system, SLAP Sparse, Diagnostics
!***AUTHOR  Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Write out SLAP Triad Format Linear System.
!            Routine to write out a SLAP Triad format matrix and
!            right hand side and solution to the system, if known.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, IUNIT, JOB
!     DOUBLE PRECISION A(NELT), SOLN(N), RHS(N)
!
!     CALL DTOUT( N, NELT, IA, JA, A, ISYM, SOLN, RHS, IUNIT, JOB )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! NELT   :IN       Integer.
!         Number of non-zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in the SLAP
!         Triad format.  See "LONG DESCRIPTION", below.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the lower
!         triangle of the matrix is stored.
! SOLN   :IN       Double Precision SOLN(N).
!         The solution to the linear system, if known.  This array
!         is accessed if and only if JOB is set to print it out,
!         see below.
! RHS    :IN       Double Precision RHS(N).
!         The right hand side vector.  This array is accessed if and
!         only if JOB is set to print it out, see below.
! IUNIT  :IN       Integer.
!         Fortran logical I/O device unit number to write the matrix
!         to.  This unit must be connected in a system dependent fashion
!         to a file or the console or you will get a nasty message
!         from the Fortran I/O libraries.
! JOB    :IN       Integer.
!         Flag indicating what I/O operations to perform.
!         JOB = 0 => Print only the matrix.
!             = 1 => Print matrix and RHS.
!             = 2 => Print matrix and SOLN.
!             = 3 => Print matrix, RHS and SOLN.
!
! *Precision:           Double Precision
! *Portability:
!         You must make sure that IUNIT is a valid Fortran logical
!         I/O device unit number and that the unit number has been
!         associated with a file or the console.  This is a system
!         dependent function.
!
!***LONG DESCRIPTION
!       The format for the output is as follows.  On  the first line
!       are counters and flags: N, NELT, ISYM, IRHS, ISOLN.  N, NELT
!       and ISYM are described above.  IRHS is  a flag indicating if
!       the RHS was  written out (1 is  yes, 0 is  no).  ISOLN  is a
!       flag indicating if the SOLN was written out  (1 is yes, 0 is
!       no).  The format for the fist line is: 5i10.  Then comes the
!       NELT Triad's IA(I), JA(I) and A(I), I = 1, NELT.  The format
!       for  these lines is   :  1X,I5,1X,I5,1X,E16.7.   Then  comes
!       RHS(I), I = 1, N, if IRHS = 1.  Then  comes SOLN(I), I  = 1,
!       N, if ISOLN = 1.  The format for these lines is: 1X,E16.7.
!
!       =================== S L A P Triad format ===================
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero  matrix  element is  placed   in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DTOUT
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, JOB
      DOUBLE PRECISION A(NELT), RHS(N), SOLN(N)
!
!         Local variables.
!
      INTEGER IRHS, ISOLN, I
!
!         If RHS and SOLN are to be printed also.
!         Write out the information heading.
!***FIRST EXECUTABLE STATEMENT  DTOUT
      IRHS = 0
      ISOLN = 0
      IF( JOB.EQ.1 .OR. JOB.EQ.3 ) IRHS = 1
      IF( JOB.GT.1 ) ISOLN = 1
      WRITE(IUNIT,1000) N, NELT, ISYM, IRHS, ISOLN
!
!         Write out the matrix non-zeros in Triad format.
      DO I = 1, NELT
         WRITE(IUNIT,1010) IA(I), JA(I), A(I)
      end do
!
!         If requested, write out the rhs.
      IF( IRHS.EQ.1 ) THEN
         WRITE(IUNIT,1020) (RHS(I),I=1,N)
      end if
!
!         If requested, write out the soln.
      IF( ISOLN.EQ.1 ) THEN
         WRITE(IUNIT,1020) (SOLN(I),I=1,N)
      end if
      RETURN
 1000 FORMAT(5I10)
 1010 FORMAT(1X,I5,1X,I5,1X,E16.7)
 1020 FORMAT(1X,E16.7)
!------------- LAST LINE OF DTOUT FOLLOWS ----------------------------
      END
      SUBROUTINE DTIN( N, NELT, IA, JA, A, ISYM, SOLN, RHS, &
           IUNIT, JOB )
!***BEGIN PROLOGUE  DTIN
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DTIN-D),
!             Linear system, SLAP Sparse, Diagnostics
!***AUTHOR  Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Read in SLAP Triad Format Linear System.
!            Routine to read in a SLAP Triad format matrix and
!            right hand side and solution to the system, if known.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, IUNIT, JOB
!     DOUBLE PRECISION A(NELT), SOLN(N), RHS(N)
!
!     CALL DTIN( N, NELT, IA, JA, A, ISYM, SOLN, RHS, IUNIT, JOB )
!
! *Arguments:
! N      :OUT      Integer
!         Order of the Matrix.
! NELT   :INOUT    Integer.
!         On input NELT is the maximum number of non-zeros that
!         can be stored in the IA, JA, A arrays.
!         On output NELT is the number of non-zeros stored in A.
! IA     :OUT      Integer IA(NELT).
! JA     :OUT      Integer JA(NELT).
! A      :OUT      Double Precision A(NELT).
!         On output these arrays hold the matrix A in the SLAP
!         Triad format.  See "LONG DESCRIPTION", below.
! ISYM   :OUT      Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the lower
!         triangle of the matrix is stored.
! SOLN   :OUT      Double Precision SOLN(N).
!         The solution to the linear system, if present.  This array
!         is accessed if and only if JOB to read it in, see below.
!         If the user requests that SOLN be read in, but it is not in
!         the file, then it is simply zeroed out.
! RHS    :OUT      Double Precision RHS(N).
!         The right hand side vector.  This array is accessed if and
!         only if JOB is set to read it in, see below.
!         If the user requests that RHS be read in, but it is not in
!         the file, then it is simply zeroed out.
! IUNIT  :IN       Integer.
!         Fortran logical I/O device unit number to write the matrix
!         to.  This unit must be connected in a system dependent fashion
!         to a file or the console or you will get a nasty message
!         from the Fortran I/O libraries.
! JOB    :INOUT    Integer.
!         Flag indicating what I/O operations to perform.
!         On input JOB indicates what Input operations to try to
!         perform.
!         JOB = 0 => Read only the matrix.
!             = 1 => Read matrix and RHS (if present).
!             = 2 => Read matrix and SOLN (if present).
!             = 3 => Read matrix, RHS and SOLN (if present).
!         On output JOB indicates what operations were actually
!         performed.
!         JOB = 0 => Read in only the matrix.
!             = 1 => Read in the matrix and RHS.
!             = 2 => Read in the matrix and SOLN.
!             = 3 => Read in the matrix, RHS and SOLN.
!
! *Precision:           Double Precision
! *Portability:
!         You must make sure that IUNIT is a valid Fortran logical
!         I/O device unit number and that the unit number has been
!         associated with a file or the console.  This is a system
!         dependent function.
!
!***LONG DESCRIPTION
!       The format for the output is as follows.  On  the first line
!       are counters and flags: N, NELT, ISYM, IRHS, ISOLN.  N, NELT
!       and ISYM are described above.  IRHS is  a flag indicating if
!       the RHS was  written out (1 is  yes, 0 is  no).  ISOLN  is a
!       flag indicating if the SOLN was written out  (1 is yes, 0 is
!       no).  The format for the fist line is: 5i10.  Then comes the
!       NELT Triad's IA(I), JA(I) and A(I), I = 1, NELT.  The format
!       for  these lines is   :  1X,I5,1X,I5,1X,E16.7.   Then  comes
!       RHS(I), I = 1, N, if IRHS = 1.  Then  comes SOLN(I), I  = 1,
!       N, if ISOLN = 1.  The format for these lines is: 1X,E16.7.
!
!       =================== S L A P Triad format ===================
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero  matrix  element is  placed   in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DTIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, JOB
      DOUBLE PRECISION A(NELT), RHS(N), SOLN(N)
!
!         Local variables.
!
      INTEGER IRHS, ISOLN, I, NELTMAX
!
!         Read in the information heading.
!***FIRST EXECUTABLE STATEMENT  DTIN
      NELTMAX = NELT
      READ(IUNIT,1000) N, NELT, ISYM, IRHS, ISOLN
      NELT = MIN( NELT, NELTMAX )
!
!         Read in the matrix non-zeros in Triad format.
      DO I = 1, NELT
         READ(IUNIT,1010) IA(I), JA(I), A(I)
      end do
!
!         If requested, read in the rhs.
      JOBRET = 0
      IF( JOB.EQ.1 .OR. JOB.EQ.3 ) THEN
!
!         Check to see if rhs is in the file.
         IF( IRHS.EQ.1 ) THEN
            JOBRET = 1
            READ(IUNIT,1020) (RHS(I),I=1,N)
         ELSE
            RHS(1:n) = 0.0D0
         end if
      end if
!
!         If requested, read in the soln.
      IF( JOB.GT.1 ) THEN
!
!         Check to see if soln is in the file.
         IF( ISOLN.EQ.1 ) THEN
            JOBRET = JOBRET + 2
            READ(IUNIT,1020) (SOLN(I),I=1,N)
         ELSE
            SOLN(1:n) = 0.0D0
         end if
      end if
!
      JOB = JOBRET
      RETURN
 1000 FORMAT(5I10)
 1010 FORMAT(1X,I5,1X,I5,1X,E16.7)
 1020 FORMAT(1X,E16.7)
!------------- LAST LINE OF DTIN FOLLOWS ----------------------------
      END
      SUBROUTINE DSDS(N, NELT, IA, JA, A, ISYM, DINV)
!***BEGIN PROLOGUE  DSDS
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSDS-D),
!             SLAP Sparse, Diagonal
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Diagonal Scaling Preconditioner SLAP Set Up.
!            Routine to compute the inverse of the diagonal of a matrix
!            stored in the SLAP Column format.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
!     DOUBLE PRECISION A(NELT), DINV(N)
!
!     CALL DSDS( N, NELT, IA, JA, A, ISYM, DINV )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! NELT   :IN       Integer.
!         Number of elements in arrays IA, JA, and A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in the SLAP Column
!         format.  See "Description", below.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! DINV   :OUT      Double Precision DINV(N).
!         Upon return this array holds 1./DIAG(A).
!
! *Description
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       With the SLAP  format  all  of  the   "inner  loops" of this
!       routine should vectorize  on  machines with hardware support
!       for vector   gather/scatter  operations.  Your compiler  may
!       require a compiler directive to  convince it that  there are
!       no  implicit  vector  dependencies.  Compiler directives for
!       the Alliant    FX/Fortran and CRI   CFT/CFT77 compilers  are
!       supplied with the standard SLAP distribution.
!
! *Precision:           Double Precision
!
! *Cautions:
!       This routine assumes that the diagonal of A is all  non-zero
!       and that the operation DINV = 1.0/DIAG(A) will not underflow
!       or overflow.    This  is done so that the  loop  vectorizes.
!       Matricies with zero or near zero or very  large entries will
!       have numerical difficulties  and  must  be fixed before this
!       routine is called.
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DSDS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
      DOUBLE PRECISION A(NELT), DINV(N)
!
!         Assume the Diagonal elements are the first in each column.
!         This loop should *VECTORIZE*.  If it does not you may have
!         to add a compiler directive.  We do not check for a zero
!         (or near zero) diagonal element since this would interfere
!         with vectorization.  If this makes you nervous put a check
!         in!  It will run much slower.
!***FIRST EXECUTABLE STATEMENT  DSDS
 1    CONTINUE

      DINV(1:n) = 1.0D0/A(JA(1:n))

!
      RETURN
!------------- LAST LINE OF DSDS FOLLOWS ----------------------------
      END
      SUBROUTINE DSDSCL( N, NELT, IA, JA, A, ISYM, X, B, DINV, JOB, &
           ITOL )
!***BEGIN PROLOGUE  DSDSCL
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSDSCL-D),
!             SLAP Sparse, Diagonal
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Diagonal Scaling of system Ax = b.
!            This routine scales (and unscales) the system Ax = b
!            by symmetric diagonal scaling.  The new system is:
!             -1/2  -1/2  1/2      -1/2
!            D    AD    (D   x) = D    b
!            when scaling is selected with the JOB parameter.  When
!            unscaling is selected this process is reversed.
!            The true solution is also scaled or unscaled if ITOL is set
!            appropriately, see below.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, JOB, ITOL
!     DOUBLE PRECISION A(NELT), DINV(N)
!
!     CALL DSDSCL( N, NELT, IA, JA, A, ISYM, X, B, DINV, JOB, ITOL )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! NELT   :IN       Integer.
!         Number of elements in arrays IA, JA, and A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays should hold the matrix A in the SLAP Column
!         format.  See "Description", below.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! X      :INOUT    Double Precision X(N).
!         Initial guess that will be later used in the iterative
!         solution.
!         of the scaled system.
! B      :INOUT    Double Precision B(N).
!         Right hand side vector.
! DINV   :OUT      Double Precision DINV(N).
!         Upon return this array holds 1./DIAG(A).
! JOB    :IN       Integer.
!         Flag indicating weather to scale or not.  JOB nonzero means
!         do scaling.  JOB = 0 means do unscaling.
! ITOL   :IN       Integer.
!         Flag indicating what type of error estimation to do in the
!         iterative method.  When ITOL = 11 the exact solution from
!         common block solblk will be used.  When the system is scaled
!         then the true solution must also be scaled.  If ITOL is not
!         11 then this vector is not referenced.
!
! *Common Blocks:
! SOLN    :INOUT   Double Precision SOLN(N).  COMMON BLOCK /SOLBLK/
!         The true solution, SOLN, is scaled (or unscaled) if ITOL is
!         set to 11, see above.
!
! *Description
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       With the SLAP  format  all  of  the   "inner  loops" of this
!       routine should vectorize  on  machines with hardware support
!       for vector   gather/scatter  operations.  Your compiler  may
!       require a compiler directive to  convince it that  there are
!       no  implicit  vector  dependencies.  Compiler directives for
!       the Alliant    FX/Fortran and CRI   CFT/CFT77 compilers  are
!       supplied with the standard SLAP distribution.
!
! *Precision:           Double Precision
!
! *Cautions:
!       This routine assumes that the diagonal of A is all  non-zero
!       and that the operation DINV = 1.0/DIAG(A)  will  not  under-
!       flow or overflow. This is done so that the loop  vectorizes.
!       Matricies with zero or near zero or very  large entries will
!       have numerical difficulties  and  must  be fixed before this
!       routine is called.
!
! *See Also:
!       DSDCG
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DSDSCL
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, JOB, ITOL
      DOUBLE PRECISION A(NELT), X(N), B(N), DINV(N)
      COMMON /SOLBLK/ SOLN(1)
!
!         SCALING...
!
      IF( JOB.NE.0 ) THEN

         DINV(1:n) = 1.0D0/SQRT( A(JA(1:n)) )

      ELSE
!
!         UNSCALING...
!
         DO 15 ICOL = 1, N
            DINV(ICOL) = 1.0D0/DINV(ICOL)
 15      CONTINUE
      end if
!
      DO 30 ICOL = 1, N
         JBGN = JA(ICOL)
         JEND = JA(ICOL+1)-1
         DI = DINV(ICOL)
         DO 20 J = JBGN, JEND
            A(J) = DINV(IA(J))*A(J)*DI
 20      CONTINUE
 30   CONTINUE
!
      DO 40 ICOL = 1, N
         B(ICOL) = B(ICOL)*DINV(ICOL)
         X(ICOL) = X(ICOL)/DINV(ICOL)
 40   CONTINUE
!
!         Check to see if we need to scale the "true solution" as well.
!
      IF( ITOL.EQ.11 ) THEN
         DO 50 ICOL = 1, N
            SOLN(ICOL) = SOLN(ICOL)/DINV(ICOL)
 50      CONTINUE
      end if
!
      RETURN
      END
      SUBROUTINE DSD2S(N, NELT, IA, JA, A, ISYM, DINV)
!***BEGIN PROLOGUE  DSD2S
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSD2S-D),
!             SLAP Sparse, Diagonal
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Diagonal Scaling Preconditioner SLAP Normal Eqns Set Up.
!            Routine to compute the inverse of the diagonal of the
!            matrix A*A'.  Where A is stored in SLAP-Column format.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
!     DOUBLE PRECISION A(NELT), DINV(N)
!
!     CALL DSD2S( N, NELT, IA, JA, A, ISYM, DINV )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! NELT   :IN       Integer.
!         Number of elements in arrays IA, JA, and A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays should hold the matrix A in the SLAP Column
!         format.  See "Description", below.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! DINV   :OUT      Double Precision DINV(N).
!         Upon return this array holds 1./DIAG(A*A').
!
! *Description
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       With the SLAP  format  all  of  the   "inner  loops" of this
!       routine should vectorize  on  machines with hardware support
!       for vector   gather/scatter  operations.  Your compiler  may
!       require a compiler directive to  convince it that  there are
!       no  implicit  vector  dependencies.  Compiler directives for
!       the Alliant    FX/Fortran and CRI   CFT/CFT77 compilers  are
!       supplied with the standard SLAP distribution.
!
! *Precision:           Double Precision
!
! *Cautions:
!       This routine assumes that the diagonal of A is all  non-zero
!       and that the operation DINV = 1.0/DIAG(A*A') will not under-
!       flow or overflow. This is done so that the loop  vectorizes.
!       Matricies with zero or near zero or very  large entries will
!       have numerical difficulties  and  must  be fixed before this
!       routine is called.
!
! *See Also:
!       DSDCGN
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DSD2S
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
      DOUBLE PRECISION A(NELT), DINV(N)
!
!***FIRST EXECUTABLE STATEMENT  DSD2S

      DINV(1:n) = 0.

!
!         Loop over each column.
!VD$R NOCONCUR
      DO 40 I = 1, N
         KBGN = JA(I)
         KEND = JA(I+1) - 1
!
!         Add in the contributions for each row that has a non-zero
!         in this column.
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NODEPCHK
         DO 20 K = KBGN, KEND
            DINV(IA(K)) = DINV(IA(K)) + A(K)**2
 20      CONTINUE
         IF( ISYM.EQ.1 ) THEN
!
!         Lower triangle stored by columns => upper triangle stored by
!         rows with Diagonal being the first entry.  Loop across the
!         rest of the row.
            KBGN = KBGN + 1
            IF( KBGN.LE.KEND ) THEN
               DO 30 K = KBGN, KEND
                  DINV(I) = DINV(I) + A(K)**2
 30            CONTINUE
            end if
         end if
 40   CONTINUE
      DO 50 I=1,N
         DINV(I) = 1./DINV(I)
 50   CONTINUE
!
      RETURN
!------------- LAST LINE OF DSD2S FOLLOWS ----------------------------
      END
      SUBROUTINE DS2LT( N, NELT, IA, JA, A, ISYM, NEL, IEL, JEL, EL )
!***BEGIN PROLOGUE  DS2LT
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DS2LT-D),
!             Linear system, SLAP Sparse, Lower Triangle
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Lower Triangle Preconditioner SLAP Set Up.
!            Routine to store the lower triangle of a matrix stored
!            in the Slap Column format.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
!     INTEGER NEL, IEL(N+1), JEL(NEL), NROW(N)
!     DOUBLE PRECISION A(NELT), EL(NEL)
!
!     CALL DS2LT( N, NELT, IA, JA, A, ISYM, NEL, IEL, JEL, EL )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! NELT   :IN       Integer.
!         Number of non-zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays should hold the matrix A in the SLAP Column
!         format.  See "Description", below.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the lower
!         triangle of the matrix is stored.
! NEL    :OUT      Integer.
!         Number of non-zeros in the lower triangle of A.   Also
!         coresponds to the length of the JEL, EL arrays.
! IEL    :OUT      Integer IEL(N+1).
! JEL    :OUT      Integer JEL(NEL).
! EL     :OUT      Double Precision     EL(NEL).
!         IEL, JEL, EL contain the lower triangle of the A matrix
!         stored in SLAP Column format.  See "Description", below
!         for more details bout the SLAP Column format.
!
! *Description
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DS2LT
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
      INTEGER NEL, IEL(NEL), JEL(NEL)
      DOUBLE PRECISION A(NELT), EL(NELT)
!***FIRST EXECUTABLE STATEMENT  DS2LT
      IF( ISYM.EQ.0 ) THEN
!
!         The matrix is stored non-symmetricly.  Pick out the lower
!         triangle.
!
         NEL = 0
         DO 20 ICOL = 1, N
            JEL(ICOL) = NEL+1
            JBGN = JA(ICOL)
            JEND = JA(ICOL+1)-1
!VD$ NOVECTOR
            DO J = JBGN, JEND
               IF( IA(J).GE.ICOL ) THEN
                  NEL = NEL + 1
                  IEL(NEL) = IA(J)
                  EL(NEL)  = A(J)
               end if
            end do
 20      CONTINUE
         JEL(N+1) = NEL+1
      ELSE
!
!         The matrix is symmetric and only the lower triangle is
!         stored.  Copy it to IEL, JEL, EL.
!
         NEL = NELT
         DO 30 I = 1, NELT
            IEL(I) = IA(I)
            EL(I) = A(I)
 30      CONTINUE
         DO 40 I = 1, N+1
            JEL(I) = JA(I)
 40      CONTINUE
      end if
      RETURN
!------------- LAST LINE OF DS2LT FOLLOWS ----------------------------
      END
      SUBROUTINE DSICS(N, NELT, IA, JA, A, ISYM, NEL, IEL, JEL, &
           EL, D, R, IWARN )
!***BEGIN PROLOGUE  DSICS
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSICS-D),
!             Linear system, SLAP Sparse, Iterative Precondition
!             Incomplete Cholesky Factorization.
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Incompl Cholesky Decomposition Preconditioner SLAP Set Up.
!            Routine to generate the Incomplete Cholesky decomposition,
!            L*D*L-trans, of  a symmetric positive definite  matrix, A,
!            which  is stored  in  SLAP Column format.  The  unit lower
!            triangular matrix L is  stored by rows, and the inverse of
!            the diagonal matrix D is stored.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
!     INTEGER NEL, IEL(NEL), JEL(N+1), IWARN
!     DOUBLE PRECISION A(NELT), EL(NEL), D(N), R(N)
!
!     CALL DSICS( N, NELT, IA, JA, A, ISYM, NEL, IEL, JEL, EL, D, R,
!    $    IWARN )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! NELT   :IN       Integer.
!         Number of elements in arrays IA, JA, and A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in the SLAP Column
!         format.  See "Description", below.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the lower
!         triangle of the matrix is stored.
! NEL    :OUT      Integer.
!         Number of non-zeros in the lower triangle of A.   Also
!         coresponds to the length of the JEL, EL arrays.
! IEL    :OUT      Integer IEL(N+1).
! JEL    :OUT      Integer JEL(NEL).
! EL     :OUT      Double Precision EL(NEL).
!         IEL, JEL, EL contain the unit lower triangular factor  of the
!         incomplete decomposition   of the A  matrix  stored  in  SLAP
!         Row format.   The Diagonal of   ones   *IS*   stored.     See
!         "Description", below for more details about the SLAP Row fmt.
! D      :OUT      Double Precision D(N)
!         Upon return this array holds D(I) = 1./DIAG(A).
! R      :WORK     Double Precision R(N).
!         Temporary double precision workspace needed for the
!         factorization.
! IWARN  :OUT      Integer.
!         This is a warning variable and is zero if the IC factoriza-
!         tion goes well.  It is set to the row index corresponding to
!         the last zero pivot found.  See "Description", below.
!
! *Description
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       ==================== S L A P Row format ====================
!       This routine requires  that the matrix A  be  stored  in the
!       SLAP  Row format.   In this format  the non-zeros are stored
!       counting across  rows (except for the diagonal  entry, which
!       must appear first in each "row") and  are stored in the
!       double precision
!       array A.  In other words, for each row in the matrix put the
!       diagonal entry in  A.   Then   put  in the   other  non-zero
!       elements   going  across the  row (except   the diagonal) in
!       order.   The  JA array  holds   the column   index for  each
!       non-zero.   The IA  array holds the  offsets into  the JA, A
!       arrays  for   the   beginning  of   each  row.   That    is,
!       JA(IA(IROW)),  A(IA(IROW)) points  to  the beginning  of the
!       IROW-th row in JA and A.   JA(IA(IROW+1)-1), A(IA(IROW+1)-1)
!       points to the  end of the  IROW-th row.  Note that we always
!       have IA(N+1) =  NELT+1, where  N  is  the number of rows  in
!       the matrix  and NELT  is the  number   of  non-zeros in  the
!       matrix.
!
!       Here is an example of the SLAP Row storage format for a  5x5
!       Matrix (in the A and JA arrays '|' denotes the end of a row):
!
!           5x5 Matrix         SLAP Row format for 5x5 matrix on left.
!                              1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 12 15 | 22 21 | 33 35 | 44 | 55 51 53
!       |21 22  0  0  0|  JA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  IA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       With the SLAP  format some  of  the   "inner  loops" of this
!       routine should vectorize  on  machines with hardware support
!       for vector   gather/scatter  operations.  Your compiler  may
!       require a compiler directive to  convince it that  there are
!       no  implicit  vector  dependencies.  Compiler directives for
!       the Alliant    FX/Fortran and CRI   CFT/CFT77 compilers  are
!       supplied with the standard SLAP distribution.
!
!       The IC  factorization is not  alway exist for SPD matricies.
!       In the event that a zero pivot is found it is set  to be 1.0
!       and the factorization procedes.   The integer variable IWARN
!       is set to the last row where the Diagonal was fudged.  This
!       eventuality hardly ever occurs in practice
!
! *Precision:           Double Precision
!
! *See Also:
!       SCG, DSICCG
!***REFERENCES  1. Gene Golub & Charles Van Loan, "Matrix Computations",
!                 John Hopkins University Press; 3 (1983) IBSN
!                 0-8018-3010-9.
!***ROUTINES CALLED  XERRWV
!***END PROLOGUE  DSICS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
      INTEGER NEL, IEL(NEL), JEL(NEL)
      DOUBLE PRECISION A(NELT), EL(NEL), D(N), R(N)
!
!         Set the lower triangle in IEL, JEL, EL
!***FIRST EXECUTABLE STATEMENT  DSICS
      IWARN = 0
!
!         All matrix elements stored in IA, JA, A.  Pick out the lower
!         triangle (making sure that the Diagonal of EL is one) and
!         store by rows.
!
      NEL = 1
      IEL(1) = 1
      JEL(1) = 1
      EL(1) = 1.0D0
      D(1) = A(1)
!VD$R NOCONCUR
      DO 30 IROW = 2, N
!         Put in the Diagonal.
         NEL = NEL + 1
         IEL(IROW) = NEL
         JEL(NEL) = IROW
         EL(NEL) = 1.0D0
         D(IROW) = A(JA(IROW))
!
!         Look in all the lower triangle columns for a matching row.
!         Since the matrix is symmetric, we can look across the
!         irow-th row by looking down the irow-th column (if it is
!         stored ISYM=0)...
         IF( ISYM.EQ.0 ) THEN
            ICBGN = JA(IROW)
            ICEND = JA(IROW+1)-1
         ELSE
            ICBGN = 1
            ICEND = IROW-1
         end if
         DO 20 IC = ICBGN, ICEND
            IF( ISYM.EQ.0 ) THEN
               ICOL = IA(IC)
               IF( ICOL.GE.IROW ) GOTO 20
            ELSE
               ICOL = IC
            end if
            JBGN = JA(ICOL)+1
            JEND = JA(ICOL+1)-1
            IF( JBGN.LE.JEND .AND. IA(JEND).GE.IROW ) THEN
!VD$ NOVECTOR
               DO J = JBGN, JEND
                  IF( IA(J).EQ.IROW ) THEN
                     NEL = NEL + 1
                     JEL(NEL) = ICOL
                     EL(NEL)  = A(J)
                     GOTO 20
                  end if
               end do
            end if
 20      CONTINUE
 30   CONTINUE
      IEL(N+1) = NEL+1
!
!         Sort ROWS of lower triangle into descending order (count out
!         along rows out from Diagonal).
!
      DO 60 IROW = 2, N
         IBGN = IEL(IROW)+1
         IEND = IEL(IROW+1)-1
         IF( IBGN.LT.IEND ) THEN
            DO 50 I = IBGN, IEND-1
!VD$ NOVECTOR
               DO 40 J = I+1, IEND
                  IF( JEL(I).GT.JEL(J) ) THEN
                     JELTMP = JEL(J)
                     JEL(J) = JEL(I)
                     JEL(I) = JELTMP
                     ELTMP = EL(J)
                     EL(J) = EL(I)
                     EL(I) = ELTMP
                  end if
 40            CONTINUE
 50         CONTINUE
         end if
 60   CONTINUE
!
!         Perform the Incomplete Cholesky decomposition by looping
!         over the rows.
!         Scale the first column.  Use the structure of A to pick out
!         the rows with something in column 1.
!
      IRBGN = JA(1)+1
      IREND = JA(2)-1
      DO 65 IRR = IRBGN, IREND
         IR = IA(IRR)
!         Find the index into EL for EL(1,IR).
!         Hint: it's the second entry.
         I = IEL(IR)+1
         EL(I) = EL(I)/D(1)
 65   CONTINUE
!
      DO 110 IROW = 2, N
!
!         Update the IROW-th diagonal.
!
         DO 66 I = 1, IROW-1
            R(I) = 0.0D0
 66      CONTINUE
         IBGN = IEL(IROW)+1
         IEND = IEL(IROW+1)-1
         IF( IBGN.LE.IEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NODEPCHK
            DO 70 I = IBGN, IEND
               R(JEL(I)) = EL(I)*D(JEL(I))
               D(IROW) = D(IROW) - EL(I)*R(JEL(I))
 70         CONTINUE
!
!         Check to see if we gota problem with the diagonal.
!
            IF( D(IROW).LE.0.0D0 ) THEN
               IF( IWARN.EQ.0 ) IWARN = IROW
               D(IROW) = 1.0D0
            end if
         end if
!
!         Update each EL(IROW+1:N,IROW), if there are any.
!         Use the structure of A to determine the Non-zero elements
!         of the IROW-th column of EL.
!
         IRBGN = JA(IROW)
         IREND = JA(IROW+1)-1

         DO 100 IRR = IRBGN, IREND

            IR = IA(IRR)

            IF( IR.LE.IROW ) then
              GO TO 100
            end if

!         Find the index into EL for EL(IR,IROW)
            IBGN = IEL(IR)+1
            IEND = IEL(IR+1)-1
            IF( JEL(IBGN).GT.IROW ) GOTO 100
            DO 90 I = IBGN, IEND
               IF( JEL(I).EQ.IROW ) THEN
                  ICEND = IEND
 91               IF( JEL(ICEND).GE.IROW ) THEN
                     ICEND = ICEND - 1
                     GOTO 91
                  end if
!         Sum up the EL(IR,1:IROW-1)*R(1:IROW-1) contributions.
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NODEPCHK
                  DO 80 IC = IBGN, ICEND
                     EL(I) = EL(I) - EL(IC)*R(JEL(IC))
 80               CONTINUE
                  EL(I) = EL(I)/D(IROW)
                  GOTO 100
               end if
 90         CONTINUE
!
!         If we get here, we have real problems...
            CALL XERRWV('DSICS -- A and EL data structure mismatch'// &
                 ' in row (i1)',53,1,2,1,IROW,0,0,0.0,0.0)
 100     CONTINUE
 110  CONTINUE
!
!         Replace diagonals by their inverses.
!
!VD$ CONCUR
      DO 120 I =1, N
         D(I) = 1.0D0/D(I)
 120  CONTINUE
      RETURN
!------------- LAST LINE OF DSICS FOLLOWS ----------------------------
      END
      SUBROUTINE DSILUS(N, NELT, IA, JA, A, ISYM, NL, IL, JL, &
           L, DINV, NU, IU, JU, U, NROW, NCOL)
!***BEGIN PROLOGUE  DSILUS
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSILUS-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Incomplete LU Factorization
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Incomplete LU Decomposition Preconditioner SLAP Set Up.
!            Routine to generate the incomplete LDU decomposition of a
!            matrix.  The  unit lower triangular factor L is stored by
!            rows and the  unit upper triangular factor U is stored by
!            columns.  The inverse of the diagonal matrix D is stored.
!            No fill in is allowed.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
!     INTEGER NL, IL(N+1), JL(NL), NU, IU(N+1), JU(NU)
!     INTEGER NROW(N), NCOL(N)
!     DOUBLE PRECISION A(NELT), L(NL), U(NU), DINV(N)
!
!     CALL DSILUS( N, NELT, IA, JA, A, ISYM, NL, IL, JL, L,
!    $    DINV, NU, IU, JU, U, NROW, NCOL )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! NELT   :IN       Integer.
!         Number of elements in arrays IA, JA, and A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays should hold the matrix A in the SLAP Column
!         format.  See "Description", below.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the lower
!         triangle of the matrix is stored.
! NL     :OUT      Integer.
!         Number of non-zeros in the EL array.
! IL     :OUT      Integer IL(N+1).
! JL     :OUT      Integer JL(NL).
! L      :OUT      Double Precision L(NL).
!         IL, JL, L  contain the unit ower  triangular factor of  the
!         incomplete decomposition  of some  matrix stored  in   SLAP
!         Row format.     The   Diagonal  of ones  *IS*  stored.  See
!         "DESCRIPTION", below for more details about the SLAP format.
! NU     :OUT      Integer.
!         Number of non-zeros in the U array.
! IU     :OUT      Integer IU(N+1).
! JU     :OUT      Integer JU(NU).
! U      :OUT      Double Precision     U(NU).
!         IU, JU, U contain   the unit upper triangular factor of the
!         incomplete  decomposition    of some matrix  stored in SLAP
!         Column  format.   The Diagonal of ones   *IS*  stored.  See
!         "Description", below  for  more  details  about  the   SLAP
!         format.
! NROW   :WORK     Integer NROW(N).
!         NROW(I) is the number of non-zero elements in the I-th row
!         of L.
! NCOL   :WORK     Integer NCOL(N).
!         NCOL(I) is the number of non-zero elements in the I-th
!         column of U.
!
! *Description
!       IL, JL, L should contain the unit  lower triangular factor of
!       the incomplete decomposition of the A matrix  stored in SLAP
!       Row format.  IU, JU, U should contain  the unit upper factor
!       of the  incomplete decomposition of  the A matrix  stored in
!       SLAP Column format This ILU factorization can be computed by
!       the DSILUS routine.  The diagonals (which is all one's) are
!       stored.
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       ==================== S L A P Row format ====================
!       This routine requires  that the matrix A  be  stored  in the
!       SLAP  Row format.   In this format  the non-zeros are stored
!       counting across  rows (except for the diagonal  entry, which
!       must appear first in each "row") and  are stored in the
!       double precision
!       array A.  In other words, for each row in the matrix put the
!       diagonal entry in  A.   Then   put  in the   other  non-zero
!       elements   going  across the  row (except   the diagonal) in
!       order.   The  JA array  holds   the column   index for  each
!       non-zero.   The IA  array holds the  offsets into  the JA, A
!       arrays  for   the   beginning  of   each  row.   That    is,
!       JA(IA(IROW)),  A(IA(IROW)) points  to  the beginning  of the
!       IROW-th row in JA and A.   JA(IA(IROW+1)-1), A(IA(IROW+1)-1)
!       points to the  end of the  IROW-th row.  Note that we always
!       have IA(N+1) =  NELT+1, where  N  is  the number of rows  in
!       the matrix  and NELT  is the  number   of  non-zeros in  the
!       matrix.
!
!       Here is an example of the SLAP Row storage format for a  5x5
!       Matrix (in the A and JA arrays '|' denotes the end of a row):
!
!           5x5 Matrix         SLAP Row format for 5x5 matrix on left.
!                              1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 12 15 | 22 21 | 33 35 | 44 | 55 51 53
!       |21 22  0  0  0|  JA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  IA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *See Also:
!       SILUR
!***REFERENCES  1. Gene Golub & Charles Van Loan, "Matrix Computations",
!                 John Hopkins University Press; 3 (1983) IBSN
!                 0-8018-3010-9.
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DSILUS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, NL, IL(NL), JL(NL)
      INTEGER NU, IU(NU), JU(NU), NROW(N), NCOL(N)
      DOUBLE PRECISION A(NELT), L(NL), DINV(N), U(NU)
!
!         Count number of elements in each row of the lower triangle.
!***FIRST EXECUTABLE STATEMENT  DSILUS

         NROW(1:n) = 0
         NCOL(1:n) = 0

!VD$R NOCONCUR
!VD$R NOVECTOR
      DO 30 ICOL = 1, N
         JBGN = JA(ICOL)+1
         JEND = JA(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
            DO 20 J = JBGN, JEND
               IF( IA(J).LT.ICOL ) THEN
                  NCOL(ICOL) = NCOL(ICOL) + 1
               ELSE
                  NROW(IA(J)) = NROW(IA(J)) + 1
                  IF( ISYM.NE.0 ) NCOL(IA(J)) = NCOL(IA(J)) + 1
               end if
 20         CONTINUE
         end if
 30   CONTINUE
      JU(1) = 1
      IL(1) = 1
      DO 40 ICOL = 1, N
         IL(ICOL+1) = IL(ICOL) + NROW(ICOL)
         JU(ICOL+1) = JU(ICOL) + NCOL(ICOL)
         NROW(ICOL) = IL(ICOL)
         NCOL(ICOL) = JU(ICOL)
 40   CONTINUE
!
!         Copy the matrix A into the L and U structures.
      DO 60 ICOL = 1, N
         DINV(ICOL) = A(JA(ICOL))
         JBGN = JA(ICOL)+1
         JEND = JA(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
            DO 50 J = JBGN, JEND
               IROW = IA(J)
               IF( IROW.LT.ICOL ) THEN
!         Part of the upper triangle.
                  IU(NCOL(ICOL)) = IROW
                  U(NCOL(ICOL)) = A(J)
                  NCOL(ICOL) = NCOL(ICOL) + 1
               ELSE
!         Part of the lower triangle (stored by row).
                  JL(NROW(IROW)) = ICOL
                  L(NROW(IROW)) = A(J)
                  NROW(IROW) = NROW(IROW) + 1
                  IF( ISYM.NE.0 ) THEN
!         Symmetric...Copy lower triangle into upper triangle as well.
                     IU(NCOL(IROW)) = ICOL
                     U(NCOL(IROW)) = A(J)
                     NCOL(IROW) = NCOL(IROW) + 1
                  end if
               end if
 50         CONTINUE
         end if
 60   CONTINUE
!
!         Sort the rows of L and the columns of U.
      DO 110 K = 2, N
         JBGN = JU(K)
         JEND = JU(K+1)-1
         IF( JBGN.LT.JEND ) THEN
            DO 80 J = JBGN, JEND-1
               DO 70 I = J+1, JEND
                  IF( IU(J).GT.IU(I) ) THEN
                     ITEMP = IU(J)
                     IU(J) = IU(I)
                     IU(I) = ITEMP
                     TEMP = U(J)
                     U(J) = U(I)
                     U(I) = TEMP
                  end if
 70            CONTINUE
 80         CONTINUE
         end if
         IBGN = IL(K)
         IEND = IL(K+1)-1
         IF( IBGN.LT.IEND ) THEN
            DO I = IBGN, IEND-1
               DO 90 J = I+1, IEND
                  IF( JL(I).GT.JL(J) ) THEN
                     JTEMP = JU(I)
                     JU(I) = JU(J)
                     JU(J) = JTEMP
                     TEMP = L(I)
                     L(I) = L(J)
                     L(J) = TEMP
                  end if
 90            CONTINUE
           end do
         end if
 110  CONTINUE
!
!         Perform the incomplete LDU decomposition.
      DO 300 I=2,N
!
!           I-th row of L
         INDX1 = IL(I)
         INDX2 = IL(I+1) - 1
         IF(INDX1 .GT. INDX2) GO TO 200
         DO 190 INDX=INDX1,INDX2
            IF(INDX .EQ. INDX1) GO TO 180
            INDXR1 = INDX1
            INDXR2 = INDX - 1
            INDXC1 = JU(JL(INDX))
            INDXC2 = JU(JL(INDX)+1) - 1
            IF(INDXC1 .GT. INDXC2) GO TO 180
 160        KR = JL(INDXR1)
 170        KC = IU(INDXC1)
            IF(KR .GT. KC) THEN
               INDXC1 = INDXC1 + 1
               IF(INDXC1 .LE. INDXC2) GO TO 170
            ELSEIF(KR .LT. KC) THEN
               INDXR1 = INDXR1 + 1
               IF(INDXR1 .LE. INDXR2) GO TO 160
            ELSEIF(KR .EQ. KC) THEN
               L(INDX) = L(INDX) - L(INDXR1)*DINV(KC)*U(INDXC1)
               INDXR1 = INDXR1 + 1
               INDXC1 = INDXC1 + 1
               IF(INDXR1 .LE. INDXR2 .AND. INDXC1 .LE. INDXC2) GO TO 160
            end if
 180        L(INDX) = L(INDX)/DINV(JL(INDX))
 190     CONTINUE
!
!         ith column of u
 200     INDX1 = JU(I)
         INDX2 = JU(I+1) - 1
         IF(INDX1 .GT. INDX2) GO TO 260
         DO 250 INDX=INDX1,INDX2
            IF(INDX .EQ. INDX1) GO TO 240
            INDXC1 = INDX1
            INDXC2 = INDX - 1
            INDXR1 = IL(IU(INDX))
            INDXR2 = IL(IU(INDX)+1) - 1
            IF(INDXR1 .GT. INDXR2) GO TO 240
 210        KR = JL(INDXR1)
 220        KC = IU(INDXC1)
            IF(KR .GT. KC) THEN
               INDXC1 = INDXC1 + 1
               IF(INDXC1 .LE. INDXC2) GO TO 220
            ELSEIF(KR .LT. KC) THEN
               INDXR1 = INDXR1 + 1
               IF(INDXR1 .LE. INDXR2) GO TO 210
            ELSEIF(KR .EQ. KC) THEN
               U(INDX) = U(INDX) - L(INDXR1)*DINV(KC)*U(INDXC1)
               INDXR1 = INDXR1 + 1
               INDXC1 = INDXC1 + 1
               IF(INDXR1 .LE. INDXR2 .AND. INDXC1 .LE. INDXC2) GO TO 210
            end if
 240        U(INDX) = U(INDX)/DINV(IU(INDX))
 250     CONTINUE
!
!         ith diagonal element
 260     INDXR1 = IL(I)
         INDXR2 = IL(I+1) - 1
         IF(INDXR1 .GT. INDXR2) GO TO 300
         INDXC1 = JU(I)
         INDXC2 = JU(I+1) - 1
         IF(INDXC1 .GT. INDXC2) GO TO 300
 270     KR = JL(INDXR1)
 280     KC = IU(INDXC1)
         IF(KR .GT. KC) THEN
            INDXC1 = INDXC1 + 1
            IF(INDXC1 .LE. INDXC2) GO TO 280
         ELSEIF(KR .LT. KC) THEN
            INDXR1 = INDXR1 + 1
            IF(INDXR1 .LE. INDXR2) GO TO 270
         ELSEIF(KR .EQ. KC) THEN
            DINV(I) = DINV(I) - L(INDXR1)*DINV(KC)*U(INDXC1)
            INDXR1 = INDXR1 + 1
            INDXC1 = INDXC1 + 1
            IF(INDXR1 .LE. INDXR2 .AND. INDXC1 .LE. INDXC2) GO TO 270
         end if
!
 300  CONTINUE
!
!         replace diagonal lts by their inverses.
!VD$ VECTOR
      DO 430 I=1,N
         DINV(I) = 1./DINV(I)
 430  CONTINUE
!
      RETURN
!------------- LAST LINE OF DSILUS FOLLOWS ----------------------------
      END
      SUBROUTINE DSMV( N, X, Y, NELT, IA, JA, A, ISYM )
!***BEGIN PROLOGUE  DSMV
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSMV-S),
!             Matrix Vector Multiply, Sparse
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP Column Format Sparse Matrix Vector Product.
!            Routine to calculate the sparse matrix vector product:
!            Y = A*X.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(N+1), ISYM
!     DOUBLE PRECISION X(N), Y(N), A(NELT)
!
!     CALL DSMV(N, X, Y, NELT, IA, JA, A, ISYM )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! X      :IN       Double Precision X(N).
!         The vector that should be multiplied by the matrix.
! Y      :OUT      Double Precision Y(N).
!         The product of the matrix and the vector.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(N+1).
! A      :IN       Integer A(NELT).
!         These arrays should hold the matrix A in the SLAP Column
!         format.  See "Description", below.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
!
! *Description
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       With  the SLAP  format  the "inner  loops" of  this  routine
!       should vectorize   on machines with   hardware  support  for
!       vector gather/scatter operations.  Your compiler may require
!       a  compiler directive  to  convince   it that there  are  no
!       implicit vector  dependencies.  Compiler directives  for the
!       Alliant FX/Fortran and CRI CFT/CFT77 compilers  are supplied
!       with the standard SLAP distribution.
!
! *Precision:           Double Precision
! *Cautions:
!     This   routine   assumes  that  the matrix A is stored in SLAP
!     Column format.  It does not check  for  this (for  speed)  and
!     evil, ugly, ornery and nasty things  will happen if the matrix
!     data  structure  is,  in fact, not SLAP Column.  Beware of the
!     wrong data structure!!!
!
! *See Also:
!       DSMTV
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DSMV
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
      DOUBLE PRECISION A(NELT), X(N), Y(N)
!
!         Zero out the result vector.
!***FIRST EXECUTABLE STATEMENT  DSMV

      Y(1:n) = 0.0D0

!
!         Multiply by A.
!
!VD$R NOCONCUR
      DO 30 ICOL = 1, N
         IBGN = JA(ICOL)
         IEND = JA(ICOL+1)-1
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NODEPCHK
         DO 20 I = IBGN, IEND
            Y(IA(I)) = Y(IA(I)) + A(I)*X(ICOL)
 20      CONTINUE
 30   CONTINUE
!
      IF( ISYM.EQ.1 ) THEN
!
!         The matrix is non-symmetric.  Need to get the other half in...
!         This loops assumes that the diagonal is the first entry in
!         each column.
!
         DO 50 IROW = 1, N
            JBGN = JA(IROW)+1
            JEND = JA(IROW+1)-1
            IF( JBGN.GT.JEND ) GOTO 50
            DO 40 J = JBGN, JEND
               Y(IROW) = Y(IROW) + A(J)*X(IA(J))
 40         CONTINUE
 50      CONTINUE
      end if
      RETURN
!------------- LAST LINE OF DSMV FOLLOWS ----------------------------
      END
      SUBROUTINE DSMTV( N, X, Y, NELT, IA, JA, A, ISYM )
!***BEGIN PROLOGUE  DSMTV
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSMTV-S),
!             Matrix transpose Vector Multiply, Sparse
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP Column Format Sparse Matrix (transpose) Vector Prdt.
!            Routine to calculate the sparse matrix vector product:
!            Y = A'*X, where ' denotes transpose.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(N+1), ISYM
!     DOUBLE PRECISION X(N), Y(N), A(NELT)
!
!     CALL DSMTV(N, X, Y, NELT, IA, JA, A, ISYM )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! X      :IN       Double Precision X(N).
!         The vector that should be multiplied by the transpose of
!         the matrix.
! Y      :OUT      Double Precision Y(N).
!         The product of the transpose of the matrix and the vector.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(N+1).
! A      :IN       Integer A(NELT).
!         These arrays should hold the matrix A in the SLAP Column
!         format.  See "Description", below.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
!
! *Description
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       With  the SLAP  format  the "inner  loops" of  this  routine
!       should vectorize   on machines with   hardware  support  for
!       vector gather/scatter operations.  Your compiler may require
!       a  compiler directive  to  convince   it that there  are  no
!       implicit vector  dependencies.  Compiler directives  for the
!       Alliant FX/Fortran and CRI CFT/CFT77 compilers  are supplied
!       with the standard SLAP distribution.
!
! *Precision:           Double Precision
! *Cautions:
!     This   routine   assumes  that  the matrix A is stored in SLAP
!     Column format.  It does not check  for  this (for  speed)  and
!     evil, ugly, ornery and nasty things  will happen if the matrix
!     data  structure  is,  in fact, not SLAP Column.  Beware of the
!     wrong data structure!!!
!
! *See Also:
!       DSMV
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DSMTV
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM
      DOUBLE PRECISION X(N), Y(N), A(NELT)
!
!         Zero out the result vector.
!***FIRST EXECUTABLE STATEMENT  DSMTV

      Y(1:n) = 0.0D0

!
!         Multiply by A-Transpose.
!         A-Transpose is stored by rows...
!VD$R NOCONCUR
      DO 30 IROW = 1, N
         IBGN = JA(IROW)
         IEND = JA(IROW+1)-1
!VD$ ASSOC
         DO 20 I = IBGN, IEND
            Y(IROW) = Y(IROW) + A(I)*X(IA(I))
 20      CONTINUE
 30   CONTINUE
!
      IF( ISYM.EQ.1 ) THEN
!
!         The matrix is non-symmetric.  Need to get the other half in...
!         This loops assumes that the diagonal is the first entry in
!         each column.
!
         DO 50 ICOL = 1, N
            JBGN = JA(ICOL)+1
            JEND = JA(ICOL+1)-1
            IF( JBGN.GT.JEND ) GOTO 50
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NODEPCHK
            DO 40 J = JBGN, JEND
               Y(IA(J)) = Y(IA(J)) + A(J)*X(ICOL)
 40         CONTINUE
 50      CONTINUE
      end if
      RETURN
!------------- LAST LINE OF DSMTV FOLLOWS ----------------------------
      END
      SUBROUTINE DSDI(N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!***BEGIN PROLOGUE  DSDI
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213  (YYMMDD)
!***CATEGORY NO.  D2A4, D2B4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSDI-S),
!             Linear system solve, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Diagonal Matrix Vector Multiply.
!            Routine to calculate the product  X = DIAG*B,
!            where DIAG is a diagonal matrix.
!***DESCRIPTION
! *Usage:
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Vector to multiply the diagonal by.
! X      :OUT      Double Precision X(N).
!         Result of DIAG*B.
! NELT   :DUMMY    Integer.
!         Retained for compatibility with SLAP MSOLVE calling sequence.
! IA     :DUMMY    Integer IA(NELT).
!         Retained for compatibility with SLAP MSOLVE calling sequence.
! JA     :DUMMY    Integer JA(N+1).
!         Retained for compatibility with SLAP MSOLVE calling sequence.
!  A     :DUMMY    Double Precision A(NELT).
!         Retained for compatibility with SLAP MSOLVE calling sequence.
! ISYM   :DUMMY    Integer.
!         Retained for compatibility with SLAP MSOLVE calling sequence.
! RWORK  :IN       Double Precision RWORK(USER DEFINABLE).
!         Work array holding the diagonal of some matrix to scale
!         B by.  This array must be set by the user or by a call
!         to the slap routine DSDS or DSD2S.  The length of RWORK
!         must be > IWORK(4)+N.
! IWORK  :IN       Integer IWORK(10).
!         IWORK(4) holds the offset into RWORK for the diagonal matrix
!         to scale B by.  This is usually set up by the SLAP pre-
!         conditioner setup routines DSDS or DSD2S.
!
! *Description:
!         This routine is supplied with the SLAP package to perform
!         the  MSOLVE  operation for iterative drivers that require
!         diagonal  Scaling  (e.g., DSDCG, DSDBCG).   It  conforms
!         to the SLAP MSOLVE CALLING CONVENTION  and hence does not
!         require an interface routine as do some of the other pre-
!         conditioners supplied with SLAP.
!
! *Precision:           Double Precision
! *See Also:
!       DSDS, DSD2S
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DSDI
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, IWORK(10)
      DOUBLE PRECISION B(N), X(N), A(NELT), RWORK(1)
!
!         Determine where the inverse of the diagonal
!         is in the work array and then scale by it.
!***FIRST EXECUTABLE STATEMENT  DSDI
      LOCD = IWORK(4) - 1
      DO I = 1, N
         X(I) = RWORK(LOCD+I)*B(I)
      end do
      RETURN
!------------- LAST LINE OF DSDI FOLLOWS ----------------------------
      END
      SUBROUTINE DSLI(N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK )
!***BEGIN PROLOGUE  DSLI
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSLI-S),
!             Linear system solve, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP MSOLVE for Lower Triangle Matrix.
!            This routine acts as an interface between the SLAP generic
!            MSOLVE calling convention and the routine that actually
!                      -1
!            computes L  B = X.
!
! *Description
!       See the Description of SLLI2 for the gory details.
!***ROUTINES CALLED  SLLI2
!***END PROLOGUE  DSLI
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, IWORK(10)
      DOUBLE PRECISION B(N), X(N), A(NELT), RWORK(1)
!***FIRST EXECUTABLE STATEMENT  DSLI
!
      NEL = IWORK(1)
      LOCIEL = IWORK(2)
      LOCJEL = IWORK(3)
      LOCEL = IWORK(4)
      CALL DSLI2(N, B, X, NEL, IWORK(LOCIEL), IWORK(LOCJEL), &
           RWORK(LOCEL))
!
      RETURN
!------------- LAST LINE OF DSLI FOLLOWS ----------------------------
      END
      SUBROUTINE DSLI2(N, B, X, NEL, IEL, JEL, EL)
!***BEGIN PROLOGUE  DSLI2
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSLI2-S),
!             Linear system solve, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP for Lower Triangle Matrix Backsolve.
!            Routine to solve a system of the form  Lx = b , where
!            L is a lower triangular matrix.
!***DESCRIPTION
! *Usage:
!     INTEGER N,  NEL, IEL(N+1), JEL(NEL)
!     DOUBLE PRECISION B(N), X(N), EL(NEL)
!
!     CALL DSLI2( N, B, X, NEL, IEL, JEL, EL )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right hand side vector.
! X      :OUT      Double Precision X(N).
!         Solution to Lx = b.
! NEL    :IN       Integer.
!         Number of non-zeros in the EL array.
! IEL    :IN       Integer IEL(N+1).
! JEL    :IN       Integer JEL(NEL).
! EL     :IN       Double Precision EL(NEL).
!         IEL, JEL, EL contain the unit lower triangular factor   of
!         the incomplete decomposition   of the A  matrix  stored in
!         SLAP Row format.  The diagonal of  ones *IS* stored.  This
!         structure can be set up by the  DS2LT  routine.  See "LONG
!         DESCRIPTION", below for more details about  the  SLAP  Row
!         format.
!
! *Description:
!       This routine is supplied with the SLAP package  as a routine
!       to  perform the  MSOLVE operation in  the SIR for the driver
!       routine DSGS.  It must be called via the SLAP MSOLVE calling
!       sequence convention interface routine DSLI.
!         **** THIS ROUTINE ITSELF DOES NOT CONFORM TO THE ****
!               **** SLAP MSOLVE CALLING CONVENTION ****
!
!       ==================== S L A P Row format ====================
!       This routine requires  that the matrix A  be  stored  in the
!       SLAP  Row format.   In this format  the non-zeros are stored
!       counting across  rows (except for the diagonal  entry, which
!       must appear first in each "row") and  are stored in the
!       double precision
!       array A.  In other words, for each row in the matrix put the
!       diagonal entry in  A.   Then   put  in the   other  non-zero
!       elements   going  across the  row (except   the diagonal) in
!       order.   The  JA array  holds   the column   index for  each
!       non-zero.   The IA  array holds the  offsets into  the JA, A
!       arrays  for   the   beginning  of   each  row.   That    is,
!       JA(IA(IROW)),  A(IA(IROW)) points  to  the beginning  of the
!       IROW-th row in JA and A.   JA(IA(IROW+1)-1), A(IA(IROW+1)-1)
!       points to the  end of the  IROW-th row.  Note that we always
!       have IA(N+1) =  NELT+1, where  N  is  the number of rows  in
!       the matrix  and NELT  is the  number   of  non-zeros in  the
!       matrix.
!
!       Here is an example of the SLAP Row storage format for a  5x5
!       Matrix (in the A and JA arrays '|' denotes the end of a row):
!
!           5x5 Matrix         SLAP Row format for 5x5 matrix on left.
!                              1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 12 15 | 22 21 | 33 35 | 44 | 55 51 53
!       |21 22  0  0  0|  JA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  IA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       With  the SLAP  Row format  the "inner loop" of this routine
!       should vectorize   on machines with   hardware  support  for
!       vector gather/scatter operations.  Your compiler may require
!       a  compiler directive  to  convince   it that there  are  no
!       implicit vector  dependencies.  Compiler directives  for the
!       Alliant FX/Fortran and CRI CFT/CFT77 compilers  are supplied
!       with the standard SLAP distribution.
!
! *Precision: Double Precision
! *See Also:
!         DSLI
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DSLI2
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NEL, IEL(NEL), JEL(NEL)
      DOUBLE PRECISION B(N), X(N), EL(NEL)
!
!         Initialize the solution by copying the right hands side
!         into it.
!***FIRST EXECUTABLE STATEMENT  DSLI2

      X(1:n) = B(1:n)

!
!VD$ NOCONCUR
      DO 30 ICOL = 1, N
         X(ICOL) = X(ICOL)/EL(JEL(ICOL))
         JBGN = JEL(ICOL) + 1
         JEND = JEL(ICOL+1) - 1
         IF( JBGN.LE.JEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NOCONCUR
!VD$ NODEPCHK
            DO 20 J = JBGN, JEND
               X(IEL(J)) = X(IEL(J)) - EL(J)*X(ICOL)
 20         CONTINUE
         end if
 30   CONTINUE
!
      RETURN
!------------- LAST LINE OF DSLI2 FOLLOWS ----------------------------
      END
      SUBROUTINE DSLLTI(N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!***BEGIN PROLOGUE  DSLLTI
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSLLTI-S),
!             Linear system solve, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP MSOLVE for LDL' (IC) Factorization.
!            This routine acts as an interface between the SLAP generic
!            MSOLVE calling convention and the routine that actually
!                           -1
!            computes (LDL')  B = X.
!***DESCRIPTION
!       See the DESCRIPTION of SLLTI2 for the gory details.
!***ROUTINES CALLED SLLTI2
!
!***END PROLOGUE  DSLLTI
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, IWORK(*)
      DOUBLE PRECISION B(*), X(*), A(NELT), RWORK(*)
!
!***FIRST EXECUTABLE STATEMENT  DSLLTI
!
      nel = iwork(1)
      lociel = iwork(3)
      locjel = iwork(2)
      locel  = iwork(4)
      locdin = iwork(5)

      call sllti2(n, b, x, nel, iwork(lociel), iwork(locjel), &
           rwork(locel), rwork(locdin))

      return
      end
      SUBROUTINE SLLTI2(N, B, X, NEL, IEL, JEL, EL, DINV)
!***BEGIN PROLOGUE  SLLTI2
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(SLLTI2-S),
!             Symmetric Linear system solve, Sparse,
!             Iterative Precondition, Incomplete Factorization
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP back solve routine for LDL' Factorization.
!            Routine to solve a system of the  form  L*D*L' X  =  B,
!            where L is a unit lower triangular  matrix  and  D is a
!            diagonal matrix and ' means transpose.
!***DESCRIPTION
! *Usage:
!     INTEGER N,  NEL, IEL(N+1), JEL(NEL)
!     DOUBLE PRECISION B(N), X(N), EL(NEL), DINV(N)
!
!     CALL SLLTI2( N, B, X, NEL, IEL, JEL, EL, DINV )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right hand side vector.
! X      :OUT      Double Precision X(N).
!         Solution to L*D*L' x = b.
! NEL    :IN       Integer.
!         Number of non-zeros in the EL array.
! IEL    :IN       Integer IEL(N+1).
! JEL    :IN       Integer JEL(NEL).
! EL     :IN       Double Precision     EL(NEL).
!         IEL, JEL, EL contain the unit lower triangular factor   of
!         the incomplete decomposition   of the A  matrix  stored in
!         SLAP Row format.   The diagonal of ones *IS* stored.  This
!         structure can be set  up  by  the DS2LT routine. See
!         "Description", below for more details about the   SLAP Row
!         format.
! DINV   :IN       Double Precision DINV(N).
!         Inverse of the diagonal matrix D.
!
! *Description:
!       This routine is supplied with  the SLAP package as a routine
!       to perform the MSOLVE operation in the SCG iteration routine
!       for  the driver  routine DSICCG.   It must be called via the
!       SLAP  MSOLVE calling sequence  convention  interface routine
!       DSLLI.
!         **** THIS ROUTINE ITSELF DOES NOT CONFORM TO THE ****
!               **** SLAP MSOLVE CALLING CONVENTION ****
!
!       IEL, JEL, EL should contain the unit lower triangular factor
!       of  the incomplete decomposition of  the A matrix  stored in
!       SLAP Row format.   This IC factorization  can be computed by
!       the  DSICS routine.  The  diagonal  (which is all one's) is
!       stored.
!
!       ==================== S L A P Row format ====================
!       This routine requires  that the matrix A  be  stored  in the
!       SLAP  Row format.   In this format  the non-zeros are stored
!       counting across  rows (except for the diagonal  entry, which
!       must appear first in each "row") and  are stored in the
!       double precision
!       array A.  In other words, for each row in the matrix put the
!       diagonal entry in  A.   Then   put  in the   other  non-zero
!       elements   going  across the  row (except   the diagonal) in
!       order.   The  JA array  holds   the column   index for  each
!       non-zero.   The IA  array holds the  offsets into  the JA, A
!       arrays  for   the   beginning  of   each  row.   That    is,
!       JA(IA(IROW)),  A(IA(IROW)) points  to  the beginning  of the
!       IROW-th row in JA and A.   JA(IA(IROW+1)-1), A(IA(IROW+1)-1)
!       points to the  end of the  IROW-th row.  Note that we always
!       have IA(N+1) =  NELT+1, where  N  is  the number of rows  in
!       the matrix  and NELT  is the  number   of  non-zeros in  the
!       matrix.
!
!       Here is an example of the SLAP Row storage format for a  5x5
!       Matrix (in the A and JA arrays '|' denotes the end of a row):
!
!           5x5 Matrix         SLAP Row format for 5x5 matrix on left.
!                              1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 12 15 | 22 21 | 33 35 | 44 | 55 51 53
!       |21 22  0  0  0|  JA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  IA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       With  the SLAP  Row format  the "inner loop" of this routine
!       should vectorize   on machines with   hardware  support  for
!       vector gather/scatter operations.  Your compiler may require
!       a  compiler directive  to  convince   it that there  are  no
!       implicit vector  dependencies.  Compiler directives  for the
!       Alliant FX/Fortran and CRI CFT/CFT77 compilers  are supplied
!       with the standard SLAP distribution.
!
! *Precision:           Double Precision
! *See Also:
!       DSICCG, DSICS
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  SLLTI2
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NEL, IEL(NEL), JEL(1)
      DOUBLE PRECISION B(N), X(N), EL(NEL), DINV(N)
!
!         solve  l*y = b,  storing result in x.
!***FIRST EXECUTABLE STATEMENT  SLLTI2

      X(1:n) = B(1:n)

      DO 30 IROW = 1, N
         IBGN = IEL(IROW) + 1
         IEND = IEL(IROW+1) - 1
         IF( IBGN.LE.IEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NOCONCUR
!VD$ NODEPCHK
            DO 20 I = IBGN, IEND
               X(IROW) = X(IROW) - EL(I)*X(JEL(I))
 20         CONTINUE
         end if
 30   CONTINUE
!
!         Solve  D*Z = Y,  storing result in X.
!
      DO 40 I=1,N
         X(I) = X(I)*DINV(I)
 40   CONTINUE
!
!         Solve  L-trans*X = Z.
!
      DO 60 IROW = N, 2, -1
         IBGN = IEL(IROW) + 1
         IEND = IEL(IROW+1) - 1
         IF( IBGN.LE.IEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NOCONCUR
!VD$ NODEPCHK
            DO 50 I = IBGN, IEND
               X(JEL(I)) = X(JEL(I)) - EL(I)*X(IROW)
 50         CONTINUE
         end if
 60   CONTINUE
!
      RETURN
!------------- LAST LINE OF SLTI2 FOLLOWS ----------------------------
      END
      SUBROUTINE DSLUI(N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!***BEGIN PROLOGUE  DSLUI
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSLUI-S),
!             Non-Symmetric Linear system solve, Sparse,
!             Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP MSOLVE for LDU Factorization.
!            This routine  acts as an  interface between  the   SLAP
!            generic MSLOVE calling convention and the routine  that
!            actually computes:     -1
!                              (LDU)  B = X.
!***DESCRIPTION
!       See the "DESCRIPTION" of DSLUI2 for the gory details.
!***ROUTINES CALLED  DSLUI2
!***END PROLOGUE  DSLUI
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, IWORK(10)
      DOUBLE PRECISION B(N), X(N), A(NELT), RWORK(1)
!
!         Pull out the locations of the arrays holding the ILU
!         factorization.
!***FIRST EXECUTABLE STATEMENT  DSLUI
      LOCIL = IWORK(1)
      LOCJL = IWORK(2)
      LOCIU = IWORK(3)
      LOCJU = IWORK(4)
      LOCL = IWORK(5)
      LOCDIN = IWORK(6)
      LOCU = IWORK(7)
!
!         Solve the system LUx = b
      CALL DSLUI2(N, B, X, IWORK(LOCIL), IWORK(LOCJL), RWORK(LOCL), &
           RWORK(LOCDIN), IWORK(LOCIU), IWORK(LOCJU), RWORK(LOCU) )
!
      RETURN
!------------- LAST LINE OF DSLUI FOLLOWS ----------------------------
      END
      SUBROUTINE DSLUI2(N, B, X, IL, JL, L, DINV, IU, JU, U )
!***BEGIN PROLOGUE  DSLUI2
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSLUI2-S),
!             Non-Symmetric Linear system solve, Sparse,
!             Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP Back solve for LDU Factorization.
!            Routine  to  solve a system of the form  L*D*U X  =  B,
!            where L is a unit  lower  triangular  matrix,  D  is  a
!            diagonal matrix, and U is a unit upper triangular matrix.
!***DESCRIPTION
! *Usage:
!     INTEGER N, IL(N+1), JL(NL), IU(NU), JU(N+1)
!     DOUBLE PRECISION B(N), X(N), L(NL), DINV(N), U(NU)
!
!     CALL DSLUI2( N, B, X, IL, JL, L, DINV, IU, JU, U )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right hand side.
! X      :OUT      Double Precision X(N).
!         Solution of L*D*U x = b.
! NEL    :IN       Integer.
!         Number of non-zeros in the EL array.
! IL     :IN       Integer IL(N+1).
! JL     :IN       Integer JL(NL).
!  L     :IN       Double Precision L(NL).
!         IL, JL, L contain the unit  lower triangular factor of the
!         incomplete decomposition of some matrix stored in SLAP Row
!         format.  The diagonal of ones *IS* stored.  This structure
!         can   be   set up  by   the  DSILUS routine.   See
!         "DESCRIPTION", below  for more   details about   the  SLAP
!         format.
! DINV   :IN       Double Precision DINV(N).
!         Inverse of the diagonal matrix D.
! NU     :IN       Integer.
!         Number of non-zeros in the U array.
! IU     :IN       Integer IU(N+1).
! JU     :IN       Integer JU(NU).
! U      :IN       Double Precision U(NU).
!         IU, JU, U contain the unit upper triangular factor  of the
!         incomplete decomposition  of  some  matrix stored in  SLAP
!         Column format.   The diagonal of ones  *IS* stored.   This
!         structure can be set up  by the DSILUS routine.  See
!         "DESCRIPTION", below   for  more   details about  the SLAP
!         format.
!
! *Description:
!       This routine is supplied with  the SLAP package as a routine
!       to  perform  the  MSOLVE operation  in   the  SIR and   SBCG
!       iteration routines for  the  drivers DSILUR and DSLUBC.   It
!       must  be called  via   the  SLAP  MSOLVE  calling   sequence
!       convention interface routine DSLUI.
!         **** THIS ROUTINE ITSELF DOES NOT CONFORM TO THE ****
!               **** SLAP MSOLVE CALLING CONVENTION ****
!
!       IL, JL, L should contain the unit lower triangular factor of
!       the incomplete decomposition of the A matrix  stored in SLAP
!       Row format.  IU, JU, U should contain  the unit upper factor
!       of the  incomplete decomposition of  the A matrix  stored in
!       SLAP Column format This ILU factorization can be computed by
!       the DSILUS routine.  The diagonals (which is all one's) are
!       stored.
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       ==================== S L A P Row format ====================
!       This routine requires  that the matrix A  be  stored  in the
!       SLAP  Row format.   In this format  the non-zeros are stored
!       counting across  rows (except for the diagonal  entry, which
!       must appear first in each "row") and  are stored in the
!       double precision
!       array A.  In other words, for each row in the matrix put the
!       diagonal entry in  A.   Then   put  in the   other  non-zero
!       elements   going  across the  row (except   the diagonal) in
!       order.   The  JA array  holds   the column   index for  each
!       non-zero.   The IA  array holds the  offsets into  the JA, A
!       arrays  for   the   beginning  of   each  row.   That    is,
!       JA(IA(IROW)),  A(IA(IROW)) points  to  the beginning  of the
!       IROW-th row in JA and A.   JA(IA(IROW+1)-1), A(IA(IROW+1)-1)
!       points to the  end of the  IROW-th row.  Note that we always
!       have IA(N+1) =  NELT+1, where  N  is  the number of rows  in
!       the matrix  and NELT  is the  number   of  non-zeros in  the
!       matrix.
!
!       Here is an example of the SLAP Row storage format for a  5x5
!       Matrix (in the A and JA arrays '|' denotes the end of a row):
!
!           5x5 Matrix         SLAP Row format for 5x5 matrix on left.
!                              1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 12 15 | 22 21 | 33 35 | 44 | 55 51 53
!       |21 22  0  0  0|  JA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  IA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       With  the SLAP  format  the "inner  loops" of  this  routine
!       should vectorize   on machines with   hardware  support  for
!       vector gather/scatter operations.  Your compiler may require
!       a  compiler directive  to  convince   it that there  are  no
!       implicit vector  dependencies.  Compiler directives  for the
!       Alliant FX/Fortran and CRI CFT/CFT77 compilers  are supplied
!       with the standard SLAP distribution.
!
! *Precision:           Double Precision
! *See Also:
!       DSILUS
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DSLUI2
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, IL(1), JL(1), IU(1), JU(1)
      DOUBLE PRECISION B(N), X(N), L(1), DINV(N), U(1)
!
!         Solve  L*Y = B,  storing result in X, L stored by rows.
!***FIRST EXECUTABLE STATEMENT  DSLUI2

      X(1:n) = B(1:n)

      DO 30 IROW = 2, N
         JBGN = IL(IROW)
         JEND = IL(IROW+1)-1
         IF( JBGN.LE.JEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ ASSOC
!VD$ NODEPCHK
            DO 20 J = JBGN, JEND
               X(IROW) = X(IROW) - L(J)*X(JL(J))
 20         CONTINUE
         end if
 30   CONTINUE
!
!         Solve  D*Z = Y,  storing result in X.
      DO 40 I=1,N
         X(I) = X(I)*DINV(I)
 40   CONTINUE
!
!         Solve  U*X = Z, U stored by columns.
      DO 60 ICOL = N, 2, -1
         JBGN = JU(ICOL)
         JEND = JU(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NODEPCHK
            DO 50 J = JBGN, JEND
               X(IU(J)) = X(IU(J)) - U(J)*X(ICOL)
 50         CONTINUE
         end if
 60   CONTINUE
!
      RETURN
!------------- LAST LINE OF DSLUI2 FOLLOWS ----------------------------
      END
      SUBROUTINE DSLUTI(N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!***BEGIN PROLOGUE  DSLUTI
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSLUTI-S),
!             Linear system solve, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP MTSOLV for LDU Factorization.
!            This routine acts as  an  interface  between  the  SLAP
!            generic MTSOLV calling convention and  the routine that
!            actually computes:       -T
!                                (LDU)  B = X.
!***DESCRIPTION
!       See the "DESCRIPTION" of DSLUI4 for the gory details.
!***ROUTINES CALLED  DSLUI4
!***END PROLOGUE  DSLUTI
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, IWORK(10)
      DOUBLE PRECISION B(N), X(N), A(N), RWORK(1)
!
!         Pull out the pointers to the L, D and U matricies and call
!         the workhorse routine.
!***FIRST EXECUTABLE STATEMENT  DSLUTI
      LOCIL = IWORK(1)
      LOCJL = IWORK(2)
      LOCIU = IWORK(3)
      LOCJU = IWORK(4)
      LOCL = IWORK(5)
      LOCDIN = IWORK(6)
      LOCU = IWORK(7)
!
      CALL DSLUI4(N, B, X, IWORK(LOCIL), IWORK(LOCJL), RWORK(LOCL), &
           RWORK(LOCDIN), IWORK(LOCIU), IWORK(LOCJU), RWORK(LOCU))
!
      RETURN
!------------- LAST LINE OF DSLUTI FOLLOWS ----------------------------
      END
      SUBROUTINE DSLUI4(N, B, X, IL, JL, L, DINV, IU, JU, U )
!***BEGIN PROLOGUE  DSLUI4
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSLUI4-S),
!             Non-Symmetric Linear system solve, Sparse,
!             Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP back solve for LDU Factorization.
!            Routine to solve a system of the form  (L*D*U)' X =  B,
!            where L is a unit  lower  triangular  matrix,  D  is  a
!            diagonal matrix, and  U  is  a  unit  upper  triangular
!            matrix and ' denotes transpose.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NL, IL(N+1), JL(NL), NU, IU(N+1), JU(NU)
!     DOUBLE PRECISION B(N), X(N), L(NEL), DINV(N), U(NU)
!
!     CALL DSLUI4( N, B, X, IL, JL, L, DINV, IU, JU, U )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right hand side.
! X      :OUT      Double Precision X(N).
!         Solution of (L*D*U)trans x = b.
! IL     :IN       Integer IL(N+1).
! JL     :IN       Integer JL(NL).
!  L     :IN       Double Precision L(NL).
!         IL, JL, L contain the unit lower triangular  factor of the
!         incomplete decomposition of some matrix stored in SLAP Row
!         format.  The diagonal of ones *IS* stored.  This structure
!         can    be set  up  by   the  DSILUS routine.   See
!         "DESCRIPTION",  below for  more  details about  the   SLAP
!         format.
! DINV   :IN       Double Precision DINV(N).
!         Inverse of the diagonal matrix D.
! IU     :IN       Integer IU(N+1).
! JU     :IN       Integer JU(NU).
! U      :IN       Double Precision U(NU).
!         IU, JU, U contain the  unit upper triangular factor of the
!         incomplete  decomposition of some  matrix stored  in  SLAP
!         Column  format.   The diagonal of  ones *IS* stored.  This
!         structure can be set up by the  DSILUS routine.  See
!         "DESCRIPTION",  below for  more  details  about  the  SLAP
!         format.
!
! *Description:
!       This routine is supplied with the SLAP package as  a routine
!       to  perform  the  MTSOLV  operation  in  the SBCG  iteration
!       routine for the  driver DSLUBC.   It must  be called via the
!       SLAP  MTSOLV calling  sequence convention interface  routine
!       DSLUTI.
!         **** THIS ROUTINE ITSELF DOES NOT CONFORM TO THE ****
!               **** SLAP MSOLVE CALLING CONVENTION ****
!
!       IL, JL, L should contain the unit lower triangular factor of
!       the incomplete decomposition of the A matrix  stored in SLAP
!       Row format.  IU, JU, U should contain  the unit upper factor
!       of the  incomplete decomposition of  the A matrix  stored in
!       SLAP Column format This ILU factorization can be computed by
!       the DSILUS routine.  The diagonals (which is all one's) are
!       stored.
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       ==================== S L A P Row format ====================
!       This routine requires  that the matrix A  be  stored  in the
!       SLAP  Row format.   In this format  the non-zeros are stored
!       counting across  rows (except for the diagonal  entry, which
!       must appear first in each "row") and  are stored in the
!       double precision
!       array A.  In other words, for each row in the matrix put the
!       diagonal entry in  A.   Then   put  in the   other  non-zero
!       elements   going  across the  row (except   the diagonal) in
!       order.   The  JA array  holds   the column   index for  each
!       non-zero.   The IA  array holds the  offsets into  the JA, A
!       arrays  for   the   beginning  of   each  row.   That    is,
!       JA(IA(IROW)),  A(IA(IROW)) points  to  the beginning  of the
!       IROW-th row in JA and A.   JA(IA(IROW+1)-1), A(IA(IROW+1)-1)
!       points to the  end of the  IROW-th row.  Note that we always
!       have IA(N+1) =  NELT+1, where  N  is  the number of rows  in
!       the matrix  and NELT  is the  number   of  non-zeros in  the
!       matrix.
!
!       Here is an example of the SLAP Row storage format for a  5x5
!       Matrix (in the A and JA arrays '|' denotes the end of a row):
!
!           5x5 Matrix         SLAP Row format for 5x5 matrix on left.
!                              1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 12 15 | 22 21 | 33 35 | 44 | 55 51 53
!       |21 22  0  0  0|  JA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  IA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       With  the SLAP  format  the "inner  loops" of  this  routine
!       should vectorize   on machines with   hardware  support  for
!       vector gather/scatter operations.  Your compiler may require
!       a  compiler directive  to  convince   it that there  are  no
!       implicit vector  dependencies.  Compiler directives  for the
!       Alliant FX/Fortran and CRI CFT/CFT77 compilers  are supplied
!       with the standard SLAP distribution.
!
! *Precision:           Double Precision
! *See Also:
!       DSILUS
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DSLUI4
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, IL(*), JL(*), IU(*), JU(*)
      DOUBLE PRECISION B(N), X(N), L(*), DINV(N), U(*)
!
!***FIRST EXECUTABLE STATEMENT  DSLUI4

      X(1:n) = B(1:n)

!
!         Solve  U'*Y = X,  storing result in X, U stored by columns.
      DO 80 IROW = 2, N
         JBGN = JU(IROW)
         JEND = JU(IROW+1) - 1
         IF( JBGN.LE.JEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ ASSOC
!VD$ NODEPCHK
            DO 70 J = JBGN, JEND
               X(IROW) = X(IROW) - U(J)*X(IU(J))
 70         CONTINUE
         end if
 80   CONTINUE
!
!         Solve  D*Z = Y,  storing result in X.
      DO 90 I = 1, N
         X(I) = X(I)*DINV(I)
 90   CONTINUE
!
!         Solve  L'*X = Z, L stored by rows.
      DO 110 ICOL = N, 2, -1
         JBGN = IL(ICOL)
         JEND = IL(ICOL+1) - 1
         IF( JBGN.LE.JEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NODEPCHK
            DO J = JBGN, JEND
               X(JL(J)) = X(JL(J)) - L(J)*X(ICOL)
            end do

         end if
 110  CONTINUE
      RETURN
!------------- LAST LINE OF DSLUI4 FOLLOWS ----------------------------
      END
      SUBROUTINE DSMMTI(N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK )
!***BEGIN PROLOGUE  DSMMTI
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSMMTI-S),
!             Linear system solve, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP MSOLVE for LDU Factorization of Normal Equations.
!            This routine acts as  an  interface  between  the  SLAP
!            generic MMTSLV calling convention and the routine  that
!            actually computes:            -1
!                            [(LDU)*(LDU)']  B = X.
!***DESCRIPTION
!       See the "DESCRIPTION" of DSMMI2 for the gory details.
!***ROUTINES CALLED  DSMMI2
!***END PROLOGUE  DSMMTI
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, IWORK(10)
      DOUBLE PRECISION B(N), X(N), A(NELT), RWORK(1)
!
!         Pull out the locations of the arrays holding the ILU
!         factorization.
!***FIRST EXECUTABLE STATEMENT  DSMMTI
      LOCIL = IWORK(1)
      LOCJL = IWORK(2)
      LOCIU = IWORK(3)
      LOCJU = IWORK(4)
      LOCL = IWORK(5)
      LOCDIN = IWORK(6)
      LOCU = IWORK(7)
!
      CALL DSMMI2(N, B, X, IWORK(LOCIL), IWORK(LOCJL), &
           RWORK(LOCL), RWORK(LOCDIN), IWORK(LOCIU), &
           IWORK(LOCJU), RWORK(LOCU))
!
      RETURN
!------------- LAST LINE OF DSMMTI FOLLOWS ----------------------------
      END
      SUBROUTINE DSMMI2( N, B, X, IL, JL, L, DINV, IU, JU, U )
!***BEGIN PROLOGUE  DSMMI2
!***DATE WRITTEN   871119   (YYMMDD)
!***REVISION DATE  881213   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DSMMI2-S),
!             Linear system, Sparse, Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  SLAP Back solve for LDU Factorization of Normal Equations.
!            To solve a system of the form (L*D*U)*(L*D*U)' X  =  B,
!            where  L  is a unit lower triangular matrix,  D   is  a
!            diagonal matrix, and  U  is  a  unit  upper  triangular
!            matrix and ' denotes transpose.
!***DESCRIPTION
! *Usage:
!     INTEGER N, IL(N+1), JL(NL), IU(N+1), JU(NU)
!     DOUBLE PRECISION B(N), X(N), L(NL), DINV(N), U(NU)
!
!     CALL DSMMI2( N, B, X, IL, JE, L, DINV, IU, JU, U )
!
! *Arguments:
! N      :IN       Integer
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right hand side.
! X      :OUT      Double Precision X(N).
!         Solution of (L*D*U)(L*D*U)trans x = b.
! IL     :IN       Integer IL(N+1).
! JL     :IN       Integer JL(NL).
!  L     :IN       Double Precision L(NL).
!         IL, JL, L contain the unit lower  triangular factor of the
!         incomplete decomposition of some matrix stored in SLAP Row
!         format.  The diagonal of ones *IS* stored.  This structure
!         can  be  set up by   the  DSILUS   routine.    See
!         "DESCRIPTION", below for  more   details   about  the SLAP
!         format.
! DINV   :IN       Double Precision DINV(N).
!         Inverse of the diagonal matrix D.
! IU     :IN       Integer IU(N+1).
! JU     :IN       Integer JU(NU).
! U      :IN       Double Precision U(NU).
!         IU, JU, U contain the unit upper  triangular factor of the
!         incomplete decomposition  of   some matrix stored in  SLAP
!         Column  format.  The diagonal  of  ones *IS* stored.  This
!         structure can be set up  by the DSILUS routine.  See
!         "DESCRIPTION",  below  for  more  details  about  the SLAP
!         format.
!
! *Description:
!       This routine is supplied with the SLAP package as  a routine
!       to  perform  the  MSOLVE  operation  in  the SBCGN iteration
!       routine for the  driver DSLUCN.   It must  be called via the
!       SLAP  MSOLVE calling  sequence convention interface  routine
!       DSMMTI.
!         **** THIS ROUTINE ITSELF DOES NOT CONFORM TO THE ****
!               **** SLAP MSOLVE CALLING CONVENTION ****
!
!       IL, JL, L should contain the unit lower triangular factor of
!       the incomplete decomposition of the A matrix  stored in SLAP
!       Row format.  IU, JU, U should contain  the unit upper factor
!       of the  incomplete decomposition of  the A matrix  stored in
!       SLAP Column format This ILU factorization can be computed by
!       the DSILUS routine.  The diagonals (which is all one's) are
!       stored.
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       ==================== S L A P Row format ====================
!       This routine requires  that the matrix A  be  stored  in the
!       SLAP  Row format.   In this format  the non-zeros are stored
!       counting acrods  rows (except for the diagonal  entry, which
!       must appear first in each "row") and  are stored in the
!       double precision
!       array A.  In other words, for each row in the matrix put the
!       diagonal entry in  A.   Then   put  in the   other  non-zero
!       elements   going  across the  row (except   the diagonal) in
!       order.   The  JA array  holds   the column   index for  each
!       non-zero.   The IA  array holds the  offsets into  the JA, A
!       arrays  for   the   beginning  of   each  row.   That    is,
!       JA(IA(IROW)),  A(IA(IROW)) points  to  the beginning  of the
!       IROW-th row in JA and A.   JA(IA(IROW+1)-1), A(IA(IROW+1)-1)
!       points to the  end of the  IROW-th row.  Note that we always
!       have IA(N+1) =  NELT+1, where  N  is  the number of rows  in
!       the matrix  and NELT  is the  number   of  non-zeros in  the
!       matrix.
!
!       Here is an example of the SLAP Row storage format for a  5x5
!       Matrix (in the A and JA arrays '|' denotes the end of a row):
!
!           5x5 Matrix         SLAP Row format for 5x5 matrix on left.
!                              1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 12 15 | 22 21 | 33 35 | 44 | 55 51 53
!       |21 22  0  0  0|  JA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  IA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       With  the SLAP  format  the "inner  loops" of  this  routine
!       should vectorize   on machines with   hardware  support  for
!       vector gather/scatter operations.  Your compiler may require
!       a  compiler directive  to  convince   it that there  are  no
!       implicit vector  dependencies.  Compiler directives  for the
!       Alliant FX/Fortran and CRI CFT/CFT77 compilers  are supplied
!       with the standard SLAP distribution.
!
! *Precision:           Double Precision
! *See Also:
!       DSILUS
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DSMMI2
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, IL(1), JL(1), IU(1), JU(1)
      DOUBLE PRECISION B(N), X(N), L(1), DINV(N), U(N)
!
!         Solve  L*Y = B,  storing result in X, L stored by rows.
!***FIRST EXECUTABLE STATEMENT  DSMMI2

      X(1:n) = B(1:n)

      DO 30 IROW = 2, N
         JBGN = IL(IROW)
         JEND = IL(IROW+1)-1
         IF( JBGN.LE.JEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ ASSOC
!VD$ NODEPCHK
            DO 20 J = JBGN, JEND
               X(IROW) = X(IROW) - L(J)*X(JL(J))
 20         CONTINUE
         end if
 30   CONTINUE
!
!         Solve  D*Z = Y,  storing result in X.
!
      X(1:n) = X(1:n) * DINV(1:n)
!
!         Solve  U*X = Z, U stored by columns.
      DO 60 ICOL = N, 2, -1
         JBGN = JU(ICOL)
         JEND = JU(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NODEPCHK
            DO 50 J = JBGN, JEND
               X(IU(J)) = X(IU(J)) - U(J)*X(ICOL)
 50         CONTINUE
         end if
 60   CONTINUE
!
!         Solve  U'*Y = X,  storing result in X, U stored by columns.
      DO 80 IROW = 2, N
         JBGN = JU(IROW)
         JEND = JU(IROW+1) - 1
         IF( JBGN.LE.JEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ ASSOC
!VD$ NODEPCHK
            DO 70 J = JBGN, JEND
               X(IROW) = X(IROW) - U(J)*X(IU(J))
 70         CONTINUE
         end if
 80   CONTINUE
!
!         Solve  D*Z = Y,  storing result in X.
      DO 90 I = 1, N
         X(I) = X(I)*DINV(I)
 90   CONTINUE
!
!         Solve  L'*X = Z, L stored by rows.
      DO 110 ICOL = N, 2, -1
         JBGN = IL(ICOL)
         JEND = IL(ICOL+1) - 1
         IF( JBGN.LE.JEND ) THEN
!LLL. OPTION ASSERT (NOHAZARD)
!DIR$ IVDEP
!VD$ NODEPCHK
            DO J = JBGN, JEND
               X(JL(J)) = X(JL(J)) - L(J)*X(ICOL)
            end do

         end if
 110  CONTINUE
!
      RETURN
!------------- LAST LINE OF DSMMI2 FOLLOWS ----------------------------
      END
      SUBROUTINE DOMN( N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE, &
           NSAVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, &
           AP, EMAP, DZ, CSAV, RWORK, IWORK )
!***BEGIN PROLOGUE  DOMN
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(DOMN-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Orthomin
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned Orthomin Sparse Iterative Ax=b Solver.
!            Routine to solve a general linear system  Ax = b  using
!            the Preconditioned Orthomin method.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE, ITOL, ITMAX
!     INTEGER  ITER, IERR, IUNIT, IWORK(USER DEFINED)
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N)
!     DOUBLE PRECISION P(N,0:NSAVE), AP(N,0:NSAVE), EMAP(N,0:NSAVE)
!     DOUBLE PRECISION DZ(N), CSAV(NSAVE), RWORK(USER DEFIED)
!     EXTERNAL MATVEC, MSOLVE
!
!     CALL DOMN(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE,
!    $     NSAVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R,
!    $     Z, P, AP, EMAP, DZ, PSAV, APSV, QSAV, CSAV, RWORK, IWORK)
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays contain the matrix data structure for A.
!         It could take any form.  See "LONG DESCRIPTION", below
!         for more late breaking details...
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MATVEC :EXT      External.
!         Name of a routine which performs the matrix vector multiply
!         Y = A*X given A and X.  The name of the MATVEC routine must
!         be declared external in the calling program.  The calling
!         sequence to MATVEC is:
!             CALL MATVEC( N, X, Y, NELT, IA, JA, A, ISYM )
!         Where N is the number of unknowns, Y is the product A*X
!         upon return X is an input vector, NELT is the number of
!         non-zeros in the SLAP IA, JA, A storage for the matrix A.
!         ISYM is a flag which, if non-zero, denotest that A is
!         symmetric and only the lower or upper triangle is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R for
!         Z given R with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays).  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSOLVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is the right-hand side
!         vector, and Z is the solution upon return.  RWORK is a
!         double precision
!         array that can be used to pass necessary preconditioning
!         information and/or workspace to MSOLVE.  IWORK is an integer
!         work array for the same purpose as RWORK.
! NSAVE  :IN       Integer.
!         Number of  direction vectors to save and orthogonalize
!         against.  NSAVE >= 0.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Breakdown of method detected.
!                       $(p,Ap) < epsilon**2$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :WORK     Double Precision R(N).
! Z      :WORK     Double Precision Z(N).
! P      :WORK     Double Precision P(N,0:NSAVE).
! AP     :WORK     Double Precision AP(N,0:NSAVE).
! EMAP   :WORK     Double Precision EMAP(N,0:NSAVE).
! DZ     :WORK     Double Precision DZ(N).
! CSAV   :WORK     Double Precision CSAV(NSAVE)
! RWORK  :WORK     Double Precision RWORK(USER DEFINED).
!         Double Precision array that can be used for workspace in
!         MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINED).
!         Integer array that can be used for workspace in MSOLVE.
!
! *Precision:           Double Precision
! *See Also:
!         DSDOMN, DSLUOM, ISDOMN
!
! *Description
!       This routine does  not care  what matrix data   structure is
!       used for  A and M.  It simply   calls  the MATVEC and MSOLVE
!       routines, with  the arguments as  described above.  The user
!       could write any type of structure and the appropriate MATVEC
!       and MSOLVE routines.  It is assumed  that A is stored in the
!       IA, JA, A  arrays in some fashion and  that M (or INV(M)) is
!       stored  in  IWORK  and  RWORK)  in  some fashion.   The SLAP
!       routines DSDOMN and DSLUOM are examples of this procedure.
!
!       Two  examples  of  matrix  data structures  are the: 1) SLAP
!       Triad  format and 2) SLAP Column format.
!
!       =================== S L A P Triad format ===================
!       In  this   format only the  non-zeros are  stored.  They may
!       appear  in *ANY* order.   The user  supplies three arrays of
!       length NELT, where  NELT  is the number  of non-zeros in the
!       matrix:  (IA(NELT), JA(NELT),  A(NELT)).  For each  non-zero
!       the  user puts   the row  and  column index   of that matrix
!       element in the IA and JA arrays.  The  value of the non-zero
!       matrix  element is  placed in  the corresponding location of
!       the A  array.  This is  an extremely easy data  structure to
!       generate.  On  the other hand it  is  not too  efficient  on
!       vector  computers   for the  iterative  solution  of  linear
!       systems.  Hence, SLAP  changes this input  data structure to
!       the SLAP   Column  format for the  iteration (but   does not
!       change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  MATVEC, MSOLVE, ISDOMN,
!                    DCOPY, DDOT, DAXPY, D1MACH
!***END PROLOGUE  DOMN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE, ITOL, ITMAX
      INTEGER  ITER, IERR, IUNIT, IWORK(*)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N)
      DOUBLE PRECISION P(N,0:NSAVE), AP(N,0:NSAVE), EMAP(N,0:NSAVE)
      DOUBLE PRECISION DZ(N), CSAV(NSAVE), RWORK(*)
      EXTERNAL MATVEC, MSOLVE
!
!         Check some of the input data.
!***FIRST EXECUTABLE STATEMENT  DOMN
      ITER = 0
      IERR = 0
      IF( N.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      EPS = epsilon ( eps )
      IF( TOL.LT.500.0*EPS ) THEN
         TOL = 500.0*EPS
         IERR = 4
      end if
      FUZZ = EPS*EPS
!
!         Calculate initial residual and pseudo-residual, and check
!         stopping criterion.
      CALL MATVEC(N, X, R, NELT, IA, JA, A, ISYM)

      R(1:n)  = B(1:n) - R(1:n)

      CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!
      IF( ISDOMN(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, NSAVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           R, Z, P, AP, EMAP, DZ, CSAV, &
           RWORK, IWORK, AK, BNRM, SOLNRM) .NE. 0 ) GO TO 200
      IF( IERR.NE.0 ) RETURN
!
!
!         ***** iteration loop *****
!
!VD$R NOVECTOR
!VD$R NOCONCUR

      DO 100 K = 1, ITMAX
         ITER = K
         IP = MOD( ITER-1, NSAVE+1 )
!
!         calculate direction vector p, a*p, and (m-inv)*a*p,
!         and save if desired.
         CALL DCOPY(N, Z, 1, P(1,IP), 1)
         CALL MATVEC(N, P(1,IP), AP(1,IP), NELT, IA, JA, A, ISYM)
         CALL MSOLVE(N, AP(1,IP), EMAP(1,IP), NELT, IA, JA, A, ISYM, &
              RWORK, IWORK)
         IF( NSAVE.EQ.0 ) THEN
            AKDEN = DDOT(N, EMAP, 1, EMAP, 1)
         ELSE
            IF( ITER.GT.1 ) THEN
               LMAX = MIN( NSAVE, ITER-1 )
               DO 20 L = 1, LMAX
                  IPO = MOD(IP+(NSAVE+1-L),NSAVE+1)
                  BKL = DDOT(N, EMAP(1,IP), 1, EMAP(1,IPO), 1)
                  BKL = BKL*CSAV(L)
                  CALL DAXPY(N, -BKL,    P(1,IPO), 1,    P(1,IP), 1)
                  CALL DAXPY(N, -BKL,   AP(1,IPO), 1,   AP(1,IP), 1)
                  CALL DAXPY(N, -BKL, EMAP(1,IPO), 1, EMAP(1,IP), 1)
 20            CONTINUE
               IF( NSAVE.GT.1 ) THEN
                  DO 30 L = NSAVE-1, 1, -1
                     CSAV(L+1) = CSAV(L)
 30               CONTINUE
               end if
            end if
            AKDEN = DDOT(N, EMAP(1,IP), 1, EMAP(1,IP), 1)
            IF( ABS(AKDEN).LT.EPS*EPS ) THEN
               IERR = 6
               RETURN
            end if
            CSAV(1) = 1./AKDEN
!
!         calculate coefficient ak, new iterate x, new residual r, and
!         new pseudo-residual z.
         end if
         AKNUM = DDOT(N, Z, 1, EMAP(1,IP), 1)
         AK = AKNUM/AKDEN
         CALL DAXPY(N,  AK,    P(1,IP), 1, X, 1)
         CALL DAXPY(N, -AK,   AP(1,IP), 1, R, 1)
         CALL DAXPY(N, -AK, EMAP(1,IP), 1, Z, 1)
!
!         check stopping criterion.
         IF( ISDOMN(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, NSAVE, &
              ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
              R, Z, P, AP, EMAP, DZ, CSAV, &
              RWORK, IWORK, AK, BNRM, SOLNRM) .NE. 0 ) GO TO 200
!
 100  CONTINUE
!
!         *****   end of loop  *****
!
!         Stopping criterion not satisfied.
      ITER = ITMAX + 1
      IERR = 2
!
 200  RETURN
!------------- LAST LINE OF DOMN FOLLOWS ----------------------------
      END
      SUBROUTINE DSDOMN(N, B, X, NELT, IA, JA, A, ISYM, NSAVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSDOMN
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(SSDOMN-D),
!             Non-Symmetric Linear system solve, Sparse,
!             Iterative Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Diagonally Scaled Orthomin Sparse Iterative Ax=b Solver.
!            Routine to solve a general linear system  Ax = b using
!            the Orthomin method with diagonal scaling.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK(10), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR
!     DOUBLE PRECISION RWORK(7*N+3*N*NSAVE+NSAVE)
!
!     CALL DSDOMN(N, B, X, NELT, IA, JA, A, ISYM, NSAVE, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the Matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "LONG
!         DESCRIPTION", below.  If the SLAP Triad format is chosen
!         it is changed internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! NSAVE  :IN       Integer.
!         Number of direction vectors to save and orthogonalize against.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!         COMMON /SOLBLK/ SOLN( )
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Breakdown of method detected.
!                       $(p,Ap) < epsilon**2$.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= 7*N+NSAVE*(3*N+1).
! IWORK  :WORK     Integer IWORK(LENIW).
!         Used to hold pointers into the RWORK array.
! LENIW  :IN       Integer.
!         Length of the double precision workspace, RWORK.  LENW >= 10.
!
! *Description:
!       This routine  is simply a driver  for  the DOMN routine.  It
!       calls the DSDS  routine  to set  up the  preconditioning and
!       then   calls DOMN with the   appropriate   MATVEC and MSOLVE
!       routines.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       In  this   format only the  non-zeros are  stored.  They may
!       appear  in *ANY* order.   The user  supplies three arrays of
!       length NELT, where  NELT  is the number  of non-zeros in the
!       matrix:  (IA(NELT), JA(NELT),  A(NELT)).  For each  non-zero
!       the  user puts   the row  and  column index   of that matrix
!       element in the IA and JA arrays.  The  value of the non-zero
!       matrix  element is  placed in  the corresponding location of
!       the A  array.  This is  an extremely easy data  structure to
!       generate.  On  the other hand it  is  not too  efficient  on
!       vector  computers   for the  iterative  solution  of  linear
!       systems.  Hence, SLAP  changes this input  data structure to
!       the SLAP   Column  format for the  iteration (but   does not
!       change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA, A)  is modified internally to
!       be the   SLAP Column format.    See  the "LONG DESCRIPTION",
!       below.
!
! *See Also:
!         DOMN, DSLUOM
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DS2Y, DCHKW, DSDS, DOMN, DSMV, DSDI
!***END PROLOGUE  DSDOMN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE, ITOL, ITMAX
      INTEGER ITER, IERR, IUNIT, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(N), TOL, ERR, RWORK(LENW)
      EXTERNAL DSMV, DSDI
      PARAMETER (LOCRB=1, LOCIB=11)
!
!         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
!***FIRST EXECUTABLE STATEMENT  DSDOMN
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Set up the workspace.  Compute the inverse of the
!         diagonal of the matrix.
      LOCIW = LOCIB
!
      LOCDIN = LOCRB
      LOCR = LOCDIN + N
      LOCZ = LOCR + N
      LOCP = LOCZ + N
      LOCAP = LOCP + N*(NSAVE+1)
      LOCEMA = LOCAP + N*(NSAVE+1)
      LOCDZ = LOCEMA + N*(NSAVE+1)
      LOCCSA = LOCDZ + N
      LOCW = LOCCSA + NSAVE
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSDOMN', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(4) = LOCDIN
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
      CALL DSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCDIN))
!
!         Perform the Diagonally Scaled Orthomin iteration algorithm.
      CALL DOMN(N, B, X, NELT, IA, JA, A, ISYM, DSMV, &
           DSDI, NSAVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           RWORK(LOCR), RWORK(LOCZ), RWORK(LOCP), RWORK(LOCAP), &
           RWORK(LOCEMA), RWORK(LOCDZ), RWORK(LOCCSA), &
           RWORK, IWORK )
      RETURN
!------------- LAST LINE OF DSDOMN FOLLOWS ----------------------------
      END
      SUBROUTINE DSLUOM(N, B, X, NELT, IA, JA, A, ISYM, NSAVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           RWORK, LENW, IWORK, LENIW )
!***BEGIN PROLOGUE  DSLUOM
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(SSLUOM-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative incomplete LU Precondition
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Incomplete LU Orthomin Sparse Iterative Ax=b Solver.
!            Routine to solve a general linear system  Ax = b  using
!            the Orthomin method with Incomplete LU decomposition.
!***DESCRIPTION
! *Usage:
!     INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE, ITOL, ITMAX
!     INTEGER ITER, IERR, IUNIT, LENW, IWORK(NEL+NU+4*N+2), LENIW
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR
!     DOUBLE PRECISION RWORK(NEL+NU+7*N+3*N*NSAVE+NSAVE)
!
!     CALL DSLUOM(N, B, X, NELT, IA, JA, A, ISYM, NSAVE, ITOL, TOL,
!    $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW )
!
! *Arguments:
! N      :IN       Integer.
!         Order of the matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :INOUT    Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :INOUT    Integer IA(NELT).
! JA     :INOUT    Integer JA(NELT).
! A      :INOUT    Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "LONG
!         DESCRIPTION", below.  If the SLAP Triad format is chosen
!         it is changed internally to the SLAP Column format.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! NSAVE  :IN       Integer.
!         Number of direction vectors to save and orthogonalize against.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :OUT      Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Return error flag.
!           IERR = 0 => All went well.
!           IERR = 1 => Insufficient storage allocated
!                       for WORK or IWORK.
!           IERR = 2 => Method failed to converge in
!                       ITMAX steps.
!           IERR = 3 => Error in user input.  Check input
!                       value of N, ITOL.
!           IERR = 4 => User error tolerance set too tight.
!                       Reset to 500.0*D1MACH(3).  Iteration proceeded.
!           IERR = 5 => Preconditioning matrix, M,  is not
!                       Positive Definite.  $(r,z) < 0.0$.
!           IERR = 6 => Breakdown of the method detected.
!                       $(p,Ap) < epsilon**2$.
!           IERR = 7 => Incomplete factorization broke down
!                       and was fudged.  Resulting preconditioning may
!                       be less than the best.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! RWORK  :WORK     Double Precision RWORK(LENW).
!         Double Precision array used for workspace.  NL is the
!         number of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal).  NU is the number of nonzeros in the upper
!         triangle of the matrix (including the diagonal).
! LENW   :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW >= NL+NU+4*N+NSAVE*(3*N+1)
! IWORK  :WORK     Integer IWORK(LENIW)
!         Integer array used for workspace.  NL is the number of non-
!         zeros in the lower triangle of the matrix (including the
!         diagonal).  NU is the number of nonzeros in the upper
!         triangle of the matrix (including the diagonal).
!         Upon return the following locations of IWORK hold information
!         which may be of use to the user:
!         IWORK(9)  Amount of Integer workspace actually used.
!         IWORK(10) Amount of Double Precision workspace actually used.
! LENIW  :IN       Integer.
!         Length of the double precision workspace, RWORK.
!         LENW > NL+NU+4*N+12.
!
! *Description:
!       This routine is  simply a driver  for  the DOMN routine.  It
!       calls the DSILUS routine  to set  up the preconditioning and
!       then  calls   DOMN  with the appropriate  MATVEC  and MSOLVE
!       routines.
!
!       The Sparse Linear Algebra Package (SLAP) utilizes two matrix
!       data structures: 1) the  SLAP Triad  format or  2)  the SLAP
!       Column format.  The user can hand this routine either of the
!       of these data structures and SLAP  will figure out  which on
!       is being used and act accordingly.
!
!       =================== S L A P Triad format ===================
!
!       This routine requires that the  matrix A be   stored in  the
!       SLAP  Triad format.  In  this format only the non-zeros  are
!       stored.  They may appear in  *ANY* order.  The user supplies
!       three arrays of  length NELT, where  NELT is  the number  of
!       non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
!       each non-zero the user puts the row and column index of that
!       matrix element  in the IA and  JA arrays.  The  value of the
!       non-zero   matrix  element is  placed  in  the corresponding
!       location of the A array.   This is  an  extremely  easy data
!       structure to generate.  On  the  other hand it   is  not too
!       efficient on vector computers for  the iterative solution of
!       linear systems.  Hence,   SLAP changes   this  input    data
!       structure to the SLAP Column format  for  the iteration (but
!       does not change it back).
!
!       Here is an example of the  SLAP Triad   storage format for a
!       5x5 Matrix.  Recall that the entries may appear in any order.
!
!           5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                              1  2  3  4  5  6  7  8  9 10 11
!       |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!       |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!       | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
! *Precision:           Double Precision
! *Side Effects:
!       The SLAP Triad format (IA, JA,  A) is modified internally to
!       be the  SLAP  Column format.  See  the   "LONG DESCRIPTION",
!       below.
!
! *See Also:
!         DOMN, DSDOMN
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DS2Y, DCHKW, DSILUS, DOMN, DSMV, DSLUI
!***END PROLOGUE  DSLUOM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE, ITOL, ITMAX
      INTEGER ITER, IERR, IUNIT, LENW, IWORK(LENIW), LENIW
      DOUBLE PRECISION B(N), X(N), A(N), RWORK(LENW)
      EXTERNAL DSMV, DSLUI
      PARAMETER (LOCRB=1, LOCIB=11)
!
!         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
!***FIRST EXECUTABLE STATEMENT  DSLUOM
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      end if
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
!
!         Count number of Non-Zero elements preconditioner ILU matrix.
!         Then set up the work arrays.
      NL = 0
      NU = 0
      DO 20 ICOL = 1, N
!         Don't count diagonal.
         JBGN = JA(ICOL)+1
         JEND = JA(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
!VD$ NOVECTOR
            DO J = JBGN, JEND
               IF( IA(J).GT.ICOL ) THEN
                  NL = NL + 1
                  IF( ISYM.NE.0 ) NU = NU + 1
               ELSE
                  NU = NU + 1
               end if
            end do
         end if
 20   CONTINUE
!
      LOCIL = LOCIB
      LOCJL = LOCIL + N+1
      LOCIU = LOCJL + NL
      LOCJU = LOCIU + NU
      LOCNR = LOCJU + N+1
      LOCNC = LOCNR + N
      LOCIW = LOCNC + N
!
      LOCL   = LOCRB
      LOCDIN = LOCL + NL
      LOCU   = LOCDIN + N
      LOCR   = LOCU + NU
      LOCZ   = LOCR + N
      LOCP   = LOCZ + N
      LOCAP  = LOCP + N*(NSAVE+1)
      LOCEMA = LOCAP + N*(NSAVE+1)
      LOCDZ  = LOCEMA + N*(NSAVE+1)
      LOCCSA = LOCDZ + N
      LOCW   = LOCCSA + NSAVE
!
!         Check the workspace allocations.
      CALL DCHKW( 'DSLUOM', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
!
      IWORK(1) = LOCIL
      IWORK(2) = LOCJL
      IWORK(3) = LOCIU
      IWORK(4) = LOCJU
      IWORK(5) = LOCL
      IWORK(6) = LOCDIN
      IWORK(7) = LOCU
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
!
!         Compute the Incomplete LU decomposition.
      CALL DSILUS( N, NELT, IA, JA, A, ISYM, NL, IWORK(LOCIL), &
           IWORK(LOCJL), RWORK(LOCL), RWORK(LOCDIN), NU, IWORK(LOCIU), &
           IWORK(LOCJU), RWORK(LOCU), IWORK(LOCNR), IWORK(LOCNC) )
!
!         Perform the incomplete LU preconditioned OrthoMin algorithm.
      CALL DOMN(N, B, X, NELT, IA, JA, A, ISYM, DSMV, &
           DSLUI, NSAVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           RWORK(LOCR), RWORK(LOCZ), RWORK(LOCP), RWORK(LOCAP), &
           RWORK(LOCEMA), RWORK(LOCDZ), RWORK(LOCCSA), &
           RWORK, IWORK )
      RETURN
      END
      FUNCTION ISDOMN(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, NSAVE, &
           ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, &
           R, Z, P, AP, EMAP, DZ, CSAV, &
           RWORK, IWORK, AK, BNRM, SOLNRM)
!***BEGIN PROLOGUE  ISDOMN
!***REFER TO  DOMN, DSDOMN, DSLUOM
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2A4
!***KEYWORDS  LIBRARY=SLATEC(SLAP),
!             TYPE=DOUBLE PRECISION(ISDOMN-D),
!             Non-Symmetric Linear system, Sparse,
!             Iterative Precondition, Stop Test, Orthomin
!***AUTHOR  Greenbaum, Anne, Courant Institute
!           Seager, Mark K., (LLNL)
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550 (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Preconditioned Orthomin Sparse Stop Test.
!            This routine calculates the stop  test for the Orthomin
!            iteration  scheme.  It returns a  nonzero if the  error
!            estimate (the type of  which is  determined by ITOL) is
!            less than the user specified tolerance TOL.
!***DESCRIPTION
! *Usage:
!     INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE, ITOL, ITMAX
!     INTEGER  ITER, IERR, IUNIT, IWORK(USER DEFINED)
!     DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N)
!     DOUBLE PRECISION P(N,0:NSAVE), AP(N,0:NSAVE), EMAP(N,0:NSAVE)
!     DOUBLE PRECISION DZ(N), CSAV(NSAVE), RWORK(USER DEFINED), AK
!     DOUBLE PRECISION BNRM, SOLNRM
!     EXTERNAL MSOLVE
!
!     IF( ISDOMN(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, NSAVE,
!    $     ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, AP,
!    $     EMAP, DZ, CSAV, RWORK, IWORK, AK, BNRM, SOLNRM)
!    $     .NE.0 ) THEN ITERATION CONVERGED
!
! *Arguments:
! N      :IN       Integer.
!         Order of the matrix.
! B      :IN       Double Precision B(N).
!         Right-hand side vector.
! X      :IN       Double Precision X(N).
!         On input X is your initial guess for solution vector.
!         On output X is the final approximate solution.
! NELT   :IN       Integer.
!         Number of Non-Zeros stored in A.
! IA     :IN       Integer IA(NELT).
! JA     :IN       Integer JA(NELT).
! A      :IN       Double Precision A(NELT).
!         These arrays should hold the matrix A in either the SLAP
!         Triad format or the SLAP Column format.  See "LONG
!         DESCRIPTION" in the DSDOMN or DSLUOM.
! ISYM   :IN       Integer.
!         Flag to indicate symmetric storage format.
!         If ISYM=0, all nonzero entries of the matrix are stored.
!         If ISYM=1, the matrix is symmetric, and only the upper
!         or lower triangle of the matrix is stored.
! MSOLVE :EXT      External.
!         Name of a routine which solves a linear system MZ = R for
!         Z given R with the preconditioning matrix M (M is supplied via
!         RWORK and IWORK arrays).  The name of the MSOLVE routine must
!         be declared external in the calling program.  The calling
!         sequence to MSOLVE is:
!             CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
!         Where N is the number of unknowns, R is the right-hand side
!         vector, and Z is the solution upon return.  RWORK is a
!         double precision
!         array that can be used to pass necessary preconditioning
!         information and/or workspace to MSOLVE.  IWORK is an integer
!         work array for the same purpose as RWORK.
! NSAVE  :IN       Integer.
!         Number of direction vectors to save and orthogonalize against.
! ITOL   :IN       Integer.
!         Flag to indicate type of convergence criterion.
!         If ITOL=1, iteration stops when the 2-norm of the residual
!         divided by the 2-norm of the right-hand side is less than TOL.
!         If ITOL=2, iteration stops when the 2-norm of M-inv times the
!         residual divided by the 2-norm of M-inv times the right hand
!         side is less than TOL, where M-inv is the inverse of the
!         diagonal of A.
!         ITOL=11 is often useful for checking and comparing different
!         routines.  For this case, the user must supply the "exact"
!         solution or a very accurate approximation (one with an error
!         much less than TOL) through a common block,
!                     COMMON /SOLBLK/ SOLN(1)
!         if ITOL=11, iteration stops when the 2-norm of the difference
!         between the iterative approximation and the user-supplied
!         solution divided by the 2-norm of the user-supplied solution
!         is less than TOL.  Note that this requires the user to set up
!         the "COMMON /SOLBLK/ SOLN(LENGTH)" in the calling routine.
!         The routine with this declaration should be loaded before the
!         stop test so that the correct length is used by the loader.
!         This procedure is not standard Fortran and may not work
!         correctly on your system (although it has worked on every
!         system the authors have tried).  If ITOL is not 11 then this
!         common block is indeed standard Fortran.
! TOL    :IN       Double Precision.
!         Convergence criterion, as described above.
! ITMAX  :IN       Integer.
!         Maximum number of iterations.
! ITER   :IN       Integer.
!         Number of iterations required to reach convergence, or
!         ITMAX+1 if convergence criterion could not be achieved in
!         ITMAX iterations.
! ERR    :OUT      Double Precision.
!         Error estimate of error in final approximate solution, as
!         defined by ITOL.
! IERR   :OUT      Integer.
!         Error flag.  IERR is set to 3 if ITOL is not on of the
!         acceptable values, see above.
! IUNIT  :IN       Integer.
!         Unit number on which to write the error at each iteration,
!         if this is desired for monitoring convergence.  If unit
!         number is 0, no writing will occur.
! R      :IN       Double Precision R(N).
!         The residual R = B-AX.
! Z      :WORK     Double Precision Z(N).
! P      :IN       Double Precision P(N,0:NSAVE).
!         Workspace used to hold the conjugate direction vector(s).
! AP     :IN       Double Precision AP(N,0:NSAVE).
!         Workspace used to hold the matrix A times the P vector(s).
! EMAP   :IN       Double Precision EMAP(N,0:NSAVE).
!         Workspace used to hold M-inv times the AP vector(s).
! DZ     :WORK     Double Precision DZ(N).
!         Workspace.
! CSAV   :DUMMY    Double Precision CSAV(NSAVE)
!         Reserved for future use.
! RWORK  :WORK     Double Precision RWORK(USER DEFINED).
!         Double Precision array that can be used for workspace in
!         MSOLVE.
! IWORK  :WORK     Integer IWORK(USER DEFINED).
!         Integer array that can be used for workspace in MSOLVE.
! AK     :IN       Double Precision.
!         Current iterate BiConjugate Gradient iteration parameter.
!
! *Function Return Values:
!       0 : Error estimate (determined by ITOL) is *NOT* less than the
!           specified tolerance, TOL.  The iteration must continue.
!       1 : Error estimate (determined by ITOL) is less than the
!           specified tolerance, TOL.  The iteration can be considered
!           complete.
!
! *Precision:           Double Precision
! *See Also:
!         DOMN, DSDOMN, DSLUOM
!
! *Cautions:
!     This routine will attempt to write to the fortran logical output
!     unit IUNIT, if IUNIT .ne. 0.  Thus, the user must make sure that
!     this  logical  unit  must  be  attached  to  a  file or terminal
!     before calling this routine with a non-zero  value  for   IUNIT.
!     This routine does not check for the validity of a non-zero IUNIT
!     unit number.
!***REFERENCES  (NONE)
!***ROUTINES CALLED  MSOLVE, DNRM2
!***COMMON BLOCKS    SOLBLK
!***END PROLOGUE  ISDOMN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER  N, NELT, IA(NELT), JA(NELT), ISYM, NSAVE, ITOL, ITMAX
      INTEGER  ITER, IUNIT, IWORK(*)
      DOUBLE PRECISION B(N), X(N), A(NELT), TOL, ERR, R(N), Z(N)
      DOUBLE PRECISION P(N,0:NSAVE), AP(N,0:NSAVE), EMAP(N,0:NSAVE)
      DOUBLE PRECISION DZ(N), CSAV(NSAVE), RWORK(*)
      EXTERNAL MSOLVE
      COMMON /SOLBLK/ SOLN(1)
!
!***FIRST EXECUTABLE STATEMENT  ISDOMN
      ISDOMN = 0
!
      IF( ITOL.EQ.1 ) THEN
!         err = ||Residual||/||RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) BNRM = DNRM2(N, B, 1)
         ERR = DNRM2(N, R, 1)/BNRM
      ELSE IF( ITOL.EQ.2 ) THEN
!                  -1              -1
!         err = ||M  Residual||/||M  RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) THEN
            CALL MSOLVE(N, B, DZ, NELT, IA, JA, A, ISYM, RWORK, IWORK)
            BNRM = DNRM2(N, DZ, 1)
         end if
         ERR = DNRM2(N, Z, 1)/BNRM
      ELSE IF( ITOL.EQ.11 ) THEN
!         err = ||x-TrueSolution||/||TrueSolution|| (2-Norms).
         IF(ITER .EQ. 0) SOLNRM = DNRM2(N, SOLN, 1)

         DZ(1:n) = X(1:n) - SOLN(1:n)

         ERR = DNRM2(N, DZ, 1)/SOLNRM
      ELSE
!
!         If we get here ITOL is not one of the acceptable values.
         ERR = 1.0E10
         IERR = 3
      end if
!
      IF(IUNIT .NE. 0) THEN
         IF( ITER.EQ.0 ) THEN
            WRITE(IUNIT,1000) NSAVE, N, ITOL
         end if
         WRITE(IUNIT,1010) ITER, ERR, AK
      end if
      IF(ERR .LE. TOL) ISDOMN = 1
!
      RETURN
 1000 FORMAT(' Preconditioned Orthomin(',I3,') for ', &
           'N, ITOL = ',I5, I5, &
           /' ITER','   Error Estimate','            Alpha')
 1010 FORMAT(1X,I4,1X,E16.7,1X,E16.7)
!------------- LAST LINE OF ISDOMN FOLLOWS ----------------------------
      END
      SUBROUTINE CAXPY(N,CA,CX,INCX,CY,INCY)
!
!     OVERWRITE COMPLEX CY WITH COMPLEX  CA*CX + CY.
!     FOR I = 0 TO N-1, REPLACE  CY(LY+I*INCY) WITH CA*CX(LX+I*INCX) +
!       CY(LY+I*INCY), WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N,
!       AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
!
      COMPLEX CX(1),CY(1),CA
!
      CANORM = ABS(REAL(CA)) + ABS(AIMAG(CA))
      IF(N.LE.0.OR.CANORM.EQ.0.E0) RETURN
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 20
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
      DO I = 1,N
          CY(KY) = CY(KY) + CA*CX(KX)
          KX = KX + INCX
          KY = KY + INCY
      end do

      RETURN
   20 CONTINUE
      NS = N*INCX
          DO 30 I=1,NS,INCX
          CY(I) = CA*CX(I) + CY(I)
   30     CONTINUE
      RETURN
      END
      SUBROUTINE CCOPY(N,CX,INCX,CY,INCY)
!
!     COPY COMPLEX CX TO COMPLEX CY.
!     FOR I = 0 TO N-1, COPY CX(LX+I*INCX) TO CY(LY+I*INCY),
!     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      COMPLEX CX(1),CY(1)
!
      IF(N .LE. 0)RETURN
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 20
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
      DO I = 1,N
          CY(KY) = CX(KX)
          KX = KX + INCX
          KY = KY + INCY
      end do

      RETURN
   20 CONTINUE
      NS = N*INCX
          DO 30 I=1,NS,INCX
          CY(I) = CX(I)
   30     CONTINUE
      RETURN
      END
      COMPLEX FUNCTION CDOTC(N,CX,INCX,CY,INCY)
!
!     RETURNS THE DOT PRODUCT FOR COMPLEX CX AND CY, USES CONJUGATE(CX)
!     CDOTC = SUM FOR I = 0 TO N-1 OF CONJ(CX(LX+I*INCX))*CY(LY+I*INCY),
!     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      COMPLEX CX(1),CY(1)
!
      CDOTC = (0.,0.)
      IF(N .LE. 0)RETURN
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 20
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
      DO I = 1,N
          CDOTC = CDOTC + CONJG(CX(KX))*CY(KY)
          KX = KX + INCX
          KY = KY + INCY
      end do

      RETURN
   20 CONTINUE
      NS = N*INCX
          DO 30 I=1,NS,INCX
          CDOTC = CONJG(CX(I))*CY(I) + CDOTC
   30     CONTINUE
      RETURN
      END
      COMPLEX FUNCTION CDOTU(N,CX,INCX,CY,INCY)
!
!     RETURNS THE DOT PRODUCT FOR COMPLEX CX AND CY, NO CONJUGATION
!     CDOTU = SUM FOR I = 0 TO N-1 OF  CX(LX+I*INCX) * CY(LY+I*INCY),
!     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      COMPLEX CX(1),CY(1)
!
      CDOTU = (0.,0.)
      IF(N .LE. 0)RETURN
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 20
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
      DO I = 1,N
          CDOTU = CDOTU + CX(KX)*CY(KY)
          KX = KX + INCX
          KY = KY + INCY
      end do

      RETURN
   20 CONTINUE
      NS = N*INCX
          DO 30 I=1,NS,INCX
          CDOTU = CDOTU + CX(I)*CY(I)
   30     CONTINUE
      RETURN
      END
      SUBROUTINE CROTG(CA,CB,C,S)
      COMPLEX CA,CB,S
      REAL C
      REAL NORM,SCALE
      COMPLEX ALPHA
      IF (CABS(CA) .NE. 0.) GO TO 10
         C = 0.
         S = (1.,0.)
         CA = CB
         GO TO 20
   10 CONTINUE
         SCALE = CABS(CA) + CABS(CB)
         NORM = SCALE * SQRT((CABS(CA/SCALE))**2 + (CABS(CB/SCALE))**2)
         ALPHA = CA /CABS(CA)
         C = CABS(CA) / NORM
         S = ALPHA * CONJG(CB) / NORM
         CA = ALPHA * NORM
   20 CONTINUE
      RETURN
      END
      SUBROUTINE CSCAL(N,CA,CX,INCX)
!
!     REPLACE COMPLEX CX BY COMPLEX CA*CX.
!     FOR I = 0 TO N-1, REPLACE CX(1+I*INCX) WITH  CA * CX(1+I*INCX)
!
      COMPLEX CA,CX(1)
!
      IF(N .LE. 0) RETURN
      NS = N*INCX
      DO I = 1,NS,INCX
          CX(i) = CA*CX(i)
      end do

      RETURN
      END
      SUBROUTINE CSSCAL(N,SA,CX,INCX)
!
!     REPLACE COMPLEX CX BY (SINGLE PRECISION SA) * (COMPLEX CX)
!     FOR I = 0 TO N-1, REPLACE CX(1+I*INCX) WITH  SA * CX(1+I*INCX)
!
      COMPLEX CX(1)
      REAL    SA
!
      IF(N .LE. 0) RETURN
      NS = N*INCX
      DO I = 1,NS,INCX
          CX(I) = SA*CX(I)
      end do

      RETURN
      END
      SUBROUTINE CSWAP(N,CX,INCX,CY,INCY)
!
!     INTERCHANGE COMPLEX CX AND COMPLEX CY
!     FOR I = 0 TO N-1, INTERCHANGE  CX(LX+I*INCX) AND CY(LY+I*INCY),
!     WHERE LX = 1 IF INCX .GT. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      COMPLEX CX(1),CY(1),CTEMP
!
      IF(N .LE. 0)RETURN
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 20
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
      DO I = 1,N
          CTEMP = CX(KX)
          CX(KX) = CY(KY)
          CY(KY) = CTEMP
          KX = KX + INCX
          KY = KY + INCY
      end do

      RETURN
   20 CONTINUE
      NS = N*INCX
          DO 30 I=1,NS,INCX
          CTEMP = CX(I)
          CX(I) = CY(I)
          CY(I) = CTEMP
   30     CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DASUM(N,DX,INCX)
!
!     RETURNS SUM OF MAGNITUDES OF DOUBLE PRECISION DX.
!     DASUM = SUM FROM 0 TO N-1 OF DABS(DX(1+I*INCX))
!
      DOUBLE PRECISION DX(1)
      DASUM = 0.D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
!
!        CODE FOR INCREMENTS NOT EQUAL TO 1.
!
      NS = N*INCX
      DO I=1,NS,INCX
          DASUM = DASUM + DABS(DX(I))
      end do

      RETURN
!
!        CODE FOR INCREMENTS EQUAL TO 1.
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6.
!
   20 M = MOD(N,6)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
         DASUM = DASUM + DABS(DX(I))
   30 CONTINUE
      IF( N .LT. 6 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
         DASUM = DASUM + DABS(DX(I)) + DABS(DX(I+1)) + DABS(DX(I+2)) &
         + DABS(DX(I+3)) + DABS(DX(I+4)) + DABS(DX(I+5))
   50 CONTINUE
      RETURN
      END
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
!
!     OVERWRITE DOUBLE PRECISION DY WITH DOUBLE PRECISION DA*DX + DY.
!     FOR I = 0 TO N-1, REPLACE  DY(LY+I*INCY) WITH DA*DX(LX+I*INCX) +
!       DY(LY+I*INCY), WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N,
!       AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
!
      DOUBLE PRECISION DX(1),DY(1),DA
      IF(N.LE.0.OR.DA.EQ.0.D0) RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
!
!        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS.
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO I = 1,N
        DY(IY) = DY(IY) + DA*DX(IX)
        IX = IX + INCX
        IY = IY + INCY
      end do

      RETURN
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4.
!
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
      RETURN
!
!        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
!
   60 CONTINUE
      NS = N*INCX
          DO 70 I=1,NS,INCX
          DY(I) = DA*DX(I) + DY(I)
   70     CONTINUE
      RETURN
      END
      SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
!
!     COPY DOUBLE PRECISION DX TO DOUBLE PRECISION DY.
!     FOR I = 0 TO N-1, COPY DX(LX+I*INCX) TO DY(LY+I*INCY),
!     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      DOUBLE PRECISION DX(1),DY(1)
      IF(N.LE.0)RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
!
!        CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO I = 1,N
        DY(IY) = DX(IX)
        IX = IX + INCX
        IY = IY + INCY
      end do

      RETURN
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 7.
!
   20 M = MOD(N,7)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DX(I)
   30 CONTINUE
      IF( N .LT. 7 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,7
        DY(I) = DX(I)
        DY(I + 1) = DX(I + 1)
        DY(I + 2) = DX(I + 2)
        DY(I + 3) = DX(I + 3)
        DY(I + 4) = DX(I + 4)
        DY(I + 5) = DX(I + 5)
        DY(I + 6) = DX(I + 6)
   50 CONTINUE
      RETURN
!
!        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
!
   60 CONTINUE
      NS=N*INCX
          DO 70 I=1,NS,INCX
          DY(I) = DX(I)
   70     CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
!
!     RETURNS THE DOT PRODUCT OF DOUBLE PRECISION DX AND DY.
!     DDOT = SUM FOR I = 0 TO N-1 OF  DX(LX+I*INCX) * DY(LY+I*INCY)
!     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      DOUBLE PRECISION DX(1),DY(1)
      DDOT = 0.D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
!
!         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO I = 1,N
         DDOT = DDOT + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
      end do

      RETURN
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1.
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
!
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
         DDOT = DDOT + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
         DDOT = DDOT + DX(I)*DY(I) + DX(I+1)*DY(I+1) + &
         DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
      RETURN
!
!         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.
!
   60 CONTINUE
      NS = N*INCX
          DO 70 I=1,NS,INCX
          DDOT = DDOT + DX(I)*DY(I)
   70     CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DNRM2 ( N, DX, INCX)
      INTEGER          NEXT
      DOUBLE PRECISION   DX(1), CUTLO, CUTHI, HITEST, SUM, XMAX,ZERO,ONE
      DATA   ZERO, ONE /0.0D0, 1.0D0/
!
!     EUCLIDEAN NORM OF THE N-VECTOR STORED IN DX() WITH STORAGE
!     INCREMENT INCX .
!     IF    N .LE. 0 RETURN WITH RESULT = 0.
!     IF N .GE. 1 THEN INCX MUST BE .GE. 1
!
!           C.L.LAWSON, 1978 JAN 08
!
!     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE
!     HOPEFULLY APPLICABLE TO ALL MACHINES.
!         CUTLO = MAXIMUM OF  DSQRT(U/EPS)  OVER ALL KNOWN MACHINES.
!         CUTHI = MINIMUM OF  DSQRT(V)      OVER ALL KNOWN MACHINES.
!     WHERE
!         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1.
!         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)
!         V   = LARGEST  NO.            (OVERFLOW  LIMIT)
!
!     BRIEF OUTLINE OF ALGORITHM..
!
!     PHASE 1    SCANS ZERO COMPONENTS.
!     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO
!     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO
!     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M
!     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX.
!
!     VALUES FOR CUTLO AND CUTHI..
!     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER
!     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS..
!     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE
!                   UNIVAC AND DEC AT 2**(-103)
!                   THUS CUTLO = 2**(-51) = 4.44089E-16
!     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC.
!                   THUS CUTHI = 2**(63.5) = 1.30438E19
!     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC.
!                   THUS CUTLO = 2**(-33.5) = 8.23181D-11
!     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D19
!     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
!     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
      DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
!
      IF(N .GT. 0) GO TO 10
         DNRM2  = ZERO
         GO TO 300
!
   10 ASSIGN 30 TO NEXT
      SUM = ZERO
      NN = N * INCX
!                                                 BEGIN MAIN LOOP
      I = 1
   20    GO TO NEXT,(30, 50, 70, 110)
   30 IF( DABS(DX(I)) .GT. CUTLO) GO TO 85
      ASSIGN 50 TO NEXT
      XMAX = ZERO
!
!                        PHASE 1.  SUM IS ZERO
!
   50 IF( DX(I) .EQ. ZERO) GO TO 200
      IF( DABS(DX(I)) .GT. CUTLO) GO TO 85
!
!                                PREPARE FOR PHASE 2.
      ASSIGN 70 TO NEXT
      GO TO 105
!
!                                PREPARE FOR PHASE 4.
!
  100 I = J
      ASSIGN 110 TO NEXT
      SUM = (SUM / DX(I)) / DX(I)
  105 XMAX = DABS(DX(I))
      GO TO 115
!
!                   PHASE 2.  SUM IS SMALL.
!                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
!
   70 IF( DABS(DX(I)) .GT. CUTLO ) GO TO 75
!
!                     COMMON CODE FOR PHASES 2 AND 4.
!                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
!
  110 IF( DABS(DX(I)) .LE. XMAX ) GO TO 115
         SUM = ONE + SUM * (XMAX / DX(I))**2
         XMAX = DABS(DX(I))
         GO TO 200
!
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
!
!
!                  PREPARE FOR PHASE 3.
!
   75 SUM = (SUM * XMAX) * XMAX
!
!
!     FOR REAL OR D.P. SET HITEST = CUTHI/N
!     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
!
   85 HITEST = CUTHI/FLOAT( N )
!
!                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
!
      DO 95 J =I,NN,INCX
      IF(DABS(DX(J)) .GE. HITEST) GO TO 100
   95    SUM = SUM + DX(J)**2
      DNRM2 = DSQRT( SUM )
      GO TO 300
!
  200 CONTINUE
      I = I + INCX
      IF ( I .LE. NN ) GO TO 20
!
!              END OF MAIN LOOP.
!
!              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
!
      DNRM2 = XMAX * DSQRT(SUM)
  300 CONTINUE
      RETURN
      END
      SUBROUTINE DROT(N,DX,INCX,DY,INCY,DC,DS)
!
!     MULTIPLY THE 2 X 2 MATRIX  ( DC DS) TIMES THE 2 X N MATRIX (DX**T)
!                                (-DS DC)                        (DY**T)
!     WHERE **T INDICATES TRANSPOSE.    THE ELEMENTS OF DX ARE IN
!     DX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
!     LX = (-INCX)*N, AND SIMILARLY FOR DY USING LY AND INCY.
      DOUBLE PRECISION DX,DY,DC,DS,ZERO,ONE,W,Z
      DIMENSION DX(1),DY(1)
!
      DATA ZERO,ONE/0.D0,1.D0/
      IF(N .LE. 0 .OR. (DS .EQ. ZERO .AND. DC .EQ. ONE)) GO TO 40
      IF(.NOT. (INCX .EQ. INCY .AND. INCX .GT. 0)) GO TO 20
!
           NSTEPS=INCX*N
           DO I=1,NSTEPS,INCX
                W=DX(I)
                Z=DY(I)
                DX(I)=DC*W+DS*Z
                DY(I)=-DS*W+DC*Z
           end do

           GO TO 40
!
   20 CONTINUE
           KX=1
           KY=1
!
           IF(INCX .LT. 0) KX=1-(N-1)*INCX
           IF(INCY .LT. 0) KY=1-(N-1)*INCY
!
           DO 30 I=1,N
                W=DX(KX)
                Z=DY(KY)
                DX(KX)=DC*W+DS*Z
                DY(KY)=-DS*W+DC*Z
                KX=KX+INCX
                KY=KY+INCY
   30           CONTINUE
   40 CONTINUE
!
      RETURN
      END
      SUBROUTINE DROTG(DA,DB,DC,DS)
!
!     DESIGNED BY C.L.LAWSON, JPL, 1977 SEPT 08
!
!
!     CONSTRUCT THE GIVENS TRANSFORMATION
!
!         ( DC  DS )
!     G = (        ) ,    DC**2 + DS**2 = 1 ,
!         (-DS  DC )
!
!     WHICH ZEROS THE SECOND ENTRY OF THE 2-VECTOR  (DA,DB)**T .
!
!     THE QUANTITY R = (+/-)DSQRT(DA**2 + DB**2) OVERWRITES DA IN
!     STORAGE.  THE VALUE OF DB IS OVERWRITTEN BY A VALUE Z WHICH
!     ALLOWS DC AND DS TO BE RECOVERED BY THE FOLLOWING ALGORITHM:
!           IF Z=1  SET  DC=0.D0  AND  DS=1.D0
!           IF DABS(Z) .LT. 1  SET  DC=DSQRT(1-Z**2)  AND  DS=Z
!           IF DABS(Z) .GT. 1  SET  DC=1/Z  AND  DS=DSQRT(1-DC**2)
!
!     NORMALLY, THE SUBPROGRAM DROT(N,DX,INCX,DY,INCY,DC,DS) WILL
!     NEXT BE CALLED TO APPLY THE TRANSFORMATION TO A 2 BY N MATRIX.
!
! ------------------------------------------------------------------
!
      DOUBLE PRECISION  DA, DB, DC, DS, U, V, R
      IF (DABS(DA) .LE. DABS(DB)) GO TO 10
!
! *** HERE DABS(DA) .GT. DABS(DB) ***
!
      U = DA + DA
      V = DB / U
!
!     NOTE THAT U AND R HAVE THE SIGN OF DA
!
      R = DSQRT(.25D0 + V**2) * U
!
!     NOTE THAT DC IS POSITIVE
!
      DC = DA / R
      DS = V * (DC + DC)
      DB = DS
      DA = R
      RETURN
!
! *** HERE DABS(DA) .LE. DABS(DB) ***
!
   10 IF (DB .EQ. 0.D0) GO TO 20
      U = DB + DB
      V = DA / U
!
!     NOTE THAT U AND R HAVE THE SIGN OF DB
!     (R IS IMMEDIATELY STORED IN DA)
!
      DA = DSQRT(.25D0 + V**2) * U
!
!     NOTE THAT DS IS POSITIVE
!
      DS = DB / DA
      DC = V * (DS + DS)
      IF (DC .EQ. 0.D0) GO TO 15
      DB = 1.D0 / DC
      RETURN
   15 DB = 1.D0
      RETURN
!
! *** HERE DA = DB = 0.D0 ***
!
   20 DC = 1.D0
      DS = 0.D0
      RETURN
!
      END
      SUBROUTINE DROTM (N,DX,INCX,DY,INCY,DPARAM)
!
!     APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
!
!     (DX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF DX ARE IN
!     (DY**T)
!
!     DX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
!     LX = (-INCX)*N, AND SIMILARLY FOR SY USING LY AND INCY.
!     WITH DPARAM(1)=DFLAG, H HAS ONE OF THE FOLLOWING FORMS..
!
!     DFLAG=-1.D0     DFLAG=0.D0        DFLAG=1.D0     DFLAG=-2.D0
!
!       (DH11  DH12)    (1.D0  DH12)    (DH11  1.D0)    (1.D0  0.D0)
!     H=(          )    (          )    (          )    (          )
!       (DH21  DH22),   (DH21  1.D0),   (-1.D0 DH22),   (0.D0  1.D0).
!     SEE DROTMG FOR A DESCRIPTION OF DATA STORAGE IN DPARAM.
!
      DOUBLE PRECISION DFLAG,DH12,DH22,DX,TWO,Z,DH11,DH21, &
       DPARAM,DY,W,ZERO
      DIMENSION DX(1),DY(1),DPARAM(5)
      DATA ZERO,TWO/0.D0,2.D0/
!
      DFLAG=DPARAM(1)
      IF(N .LE. 0 .OR.(DFLAG+TWO.EQ.ZERO)) GO TO 140
          IF(.NOT.(INCX.EQ.INCY.AND. INCX .GT.0)) GO TO 70
!
               NSTEPS=N*INCX
               IF(DFLAG) 50,10,30
   10          CONTINUE
               DH12=DPARAM(4)
               DH21=DPARAM(3)
                    DO 20 I=1,NSTEPS,INCX
                    W=DX(I)
                    Z=DY(I)
                    DX(I)=W+Z*DH12
                    DY(I)=W*DH21+Z
   20               CONTINUE
               GO TO 140
   30          CONTINUE
               DH11=DPARAM(2)
               DH22=DPARAM(5)
                    DO 40 I=1,NSTEPS,INCX
                    W=DX(I)
                    Z=DY(I)
                    DX(I)=W*DH11+Z
                    DY(I)=-W+DH22*Z
   40               CONTINUE
               GO TO 140
   50          CONTINUE
               DH11=DPARAM(2)
               DH12=DPARAM(4)
               DH21=DPARAM(3)
               DH22=DPARAM(5)
                    DO 60 I=1,NSTEPS,INCX
                    W=DX(I)
                    Z=DY(I)
                    DX(I)=W*DH11+Z*DH12
                    DY(I)=W*DH21+Z*DH22
   60               CONTINUE
               GO TO 140
   70     CONTINUE
          KX=1
          KY=1
          IF(INCX .LT. 0) KX=1+(1-N)*INCX
          IF(INCY .LT. 0) KY=1+(1-N)*INCY
!
          IF(DFLAG)120,80,100
   80     CONTINUE
          DH12=DPARAM(4)
          DH21=DPARAM(3)
               DO 90 I=1,N
               W=DX(KX)
               Z=DY(KY)
               DX(KX)=W+Z*DH12
               DY(KY)=W*DH21+Z
               KX=KX+INCX
               KY=KY+INCY
   90          CONTINUE
          GO TO 140
  100     CONTINUE
          DH11=DPARAM(2)
          DH22=DPARAM(5)
               DO 110 I=1,N
               W=DX(KX)
               Z=DY(KY)
               DX(KX)=W*DH11+Z
               DY(KY)=-W+DH22*Z
               KX=KX+INCX
               KY=KY+INCY
  110          CONTINUE
          GO TO 140
  120     CONTINUE
          DH11=DPARAM(2)
          DH12=DPARAM(4)
          DH21=DPARAM(3)
          DH22=DPARAM(5)
               DO 130 I=1,N
               W=DX(KX)
               Z=DY(KY)
               DX(KX)=W*DH11+Z*DH12
               DY(KY)=W*DH21+Z*DH22
               KX=KX+INCX
               KY=KY+INCY
  130          CONTINUE
  140     CONTINUE
          RETURN
          END
      SUBROUTINE DROTMG (DD1,DD2,DX1,DY1,DPARAM)
!
!     CONSTRUCT THE MODIFIED GIVENS TRANSFORMATION MATRIX H WHICH ZEROS
!     THE SECOND COMPONENT OF THE 2-VECTOR  (DSQRT(DD1)*DX1,DSQRT(DD2)*
!     DY2)**T.
!     WITH DPARAM(1)=DFLAG, H HAS ONE OF THE FOLLOWING FORMS..
!
!     DFLAG=-1.D0     DFLAG=0.D0        DFLAG=1.D0     DFLAG=-2.D0
!
!       (DH11  DH12)    (1.D0  DH12)    (DH11  1.D0)    (1.D0  0.D0)
!     H=(          )    (          )    (          )    (          )
!       (DH21  DH22),   (DH21  1.D0),   (-1.D0 DH22),   (0.D0  1.D0).
!     LOCATIONS 2-4 OF DPARAM CONTAIN DH11, DH21, DH12, AND DH22
!     RESPECTIVELY. (VALUES OF 1.D0, -1.D0, OR 0.D0 IMPLIED BY THE
!     VALUE OF DPARAM(1) ARE NOT STORED IN DPARAM.)
!
!     THE VALUES OF GAMSQ AND RGAMSQ SET IN THE DATA STATEMENT MAY BE
!     INEXACT.  THIS IS OK AS THEY ARE ONLY USED FOR TESTING THE SIZE
!     OF DD1 AND DD2.  ALL ACTUAL SCALING OF DATA IS DONE USING GAM.
!
      DOUBLE PRECISION GAM,ONE,RGAMSQ,DD2,DH11,DH21,DPARAM,DP2, &
       DQ2,DU,DY1,ZERO,GAMSQ,DD1,DFLAG,DH12,DH22,DP1,DQ1, &
       DTEMP,DX1,TWO
      DIMENSION DPARAM(5)
!
      DATA ZERO,ONE,TWO /0.D0,1.D0,2.D0/
      DATA GAM,GAMSQ,RGAMSQ/4096.D0,16777216.D0,5.9604645D-8/
      IF(.NOT. DD1 .LT. ZERO) GO TO 10
!       GO ZERO-H-D-AND-DX1..
          GO TO 60
   10 CONTINUE
!     CASE-DD1-NONNEGATIVE
      DP2=DD2*DY1
      IF(.NOT. DP2 .EQ. ZERO) GO TO 20
          DFLAG=-TWO
          GO TO 260
!     REGULAR-CASE..
   20 CONTINUE
      DP1=DD1*DX1
      DQ2=DP2*DY1
      DQ1=DP1*DX1
!
      IF(.NOT. DABS(DQ1) .GT. DABS(DQ2)) GO TO 40
          DH21=-DY1/DX1
          DH12=DP2/DP1
!
          DU=ONE-DH12*DH21
!
          IF(.NOT. DU .LE. ZERO) GO TO 30
!         GO ZERO-H-D-AND-DX1..
               GO TO 60
   30     CONTINUE
               DFLAG=ZERO
               DD1=DD1/DU
               DD2=DD2/DU
               DX1=DX1*DU
!         GO SCALE-CHECK..
               GO TO 100
   40 CONTINUE
          IF(.NOT. DQ2 .LT. ZERO) GO TO 50
!         GO ZERO-H-D-AND-DX1..
               GO TO 60
   50     CONTINUE
               DFLAG=ONE
               DH11=DP1/DP2
               DH22=DX1/DY1
               DU=ONE+DH11*DH22
               DTEMP=DD2/DU
               DD2=DD1/DU
               DD1=DTEMP
               DX1=DY1*DU
!         GO SCALE-CHECK
               GO TO 100
!     PROCEDURE..ZERO-H-D-AND-DX1..
   60 CONTINUE
          DFLAG=-ONE
          DH11=ZERO
          DH12=ZERO
          DH21=ZERO
          DH22=ZERO
!
          DD1=ZERO
          DD2=ZERO
          DX1=ZERO
!         RETURN..
          GO TO 220
!     PROCEDURE..FIX-H..
   70 CONTINUE
      IF(.NOT. DFLAG .GE. ZERO) GO TO 90
!
          IF(.NOT. DFLAG .EQ. ZERO) GO TO 80
          DH11=ONE
          DH22=ONE
          DFLAG=-ONE
          GO TO 90
   80     CONTINUE
          DH21=-ONE
          DH12=ONE
          DFLAG=-ONE
   90 CONTINUE
      GO TO IGO,(120,150,180,210)
!     PROCEDURE..SCALE-CHECK
  100 CONTINUE
  110     CONTINUE
          IF(.NOT. DD1 .LE. RGAMSQ) GO TO 130
               IF(DD1 .EQ. ZERO) GO TO 160
               ASSIGN 120 TO IGO
!              FIX-H..
               GO TO 70
  120          CONTINUE
               DD1=DD1*GAM**2
               DX1=DX1/GAM
               DH11=DH11/GAM
               DH12=DH12/GAM
          GO TO 110
  130 CONTINUE
  140     CONTINUE
          IF(.NOT. DD1 .GE. GAMSQ) GO TO 160
               ASSIGN 150 TO IGO
!              FIX-H..
               GO TO 70
  150          CONTINUE
               DD1=DD1/GAM**2
               DX1=DX1*GAM
               DH11=DH11*GAM
               DH12=DH12*GAM
          GO TO 140
  160 CONTINUE
  170     CONTINUE
          IF(.NOT. DABS(DD2) .LE. RGAMSQ) GO TO 190
               IF(DD2 .EQ. ZERO) GO TO 220
               ASSIGN 180 TO IGO
!              FIX-H..
               GO TO 70
  180          CONTINUE
               DD2=DD2*GAM**2
               DH21=DH21/GAM
               DH22=DH22/GAM
          GO TO 170
  190 CONTINUE
  200     CONTINUE
          IF(.NOT. DABS(DD2) .GE. GAMSQ) GO TO 220
               ASSIGN 210 TO IGO
!              FIX-H..
               GO TO 70
  210          CONTINUE
               DD2=DD2/GAM**2
               DH21=DH21*GAM
               DH22=DH22*GAM
          GO TO 200
  220 CONTINUE
          IF(DFLAG)250,230,240
  230     CONTINUE
               DPARAM(3)=DH21
               DPARAM(4)=DH12
               GO TO 260
  240     CONTINUE
               DPARAM(2)=DH11
               DPARAM(5)=DH22
               GO TO 260
  250     CONTINUE
               DPARAM(2)=DH11
               DPARAM(3)=DH21
               DPARAM(4)=DH12
               DPARAM(5)=DH22
  260 CONTINUE
          DPARAM(1)=DFLAG
          RETURN
      END
      SUBROUTINE DSCAL(N,DA,DX,INCX)
!
!     REPLACE DOUBLE PRECISION DX BY DOUBLE PRECISION DA*DX.
!     FOR I = 0 TO N-1, REPLACE DX(1+I*INCX) WITH  DA * DX(1+I*INCX)
!
      DOUBLE PRECISION DA,DX(1)
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
!
!        CODE FOR INCREMENTS NOT EQUAL TO 1.
!
      NS = N*INCX
      DO I = 1,NS,INCX
          DX(I) = DA*DX(I)
      end do

      RETURN
!
!        CODE FOR INCREMENTS EQUAL TO 1.
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
!
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DSDOT(N,SX,INCX,SY,INCY)
!
!     RETURNS D.P. DOT PRODUCT ACCUMULATED IN D.P., FOR S.P. SX AND SY
!     DSDOT = SUM FOR I = 0 TO N-1 OF  SX(LX+I*INCX) * SY(LY+I*INCY),
!     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      REAL SX(1),SY(1)
!
      DSDOT = 0.D0
      IF(N .LE. 0)RETURN
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 20
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
      DO I = 1,N
          DSDOT = DSDOT + DBLE(SX(KX))*DBLE(SY(KY))
          KX = KX + INCX
          KY = KY + INCY
      end do

      RETURN
   20 CONTINUE
      NS = N*INCX
          DO 30 I=1,NS,INCX
          DSDOT = DSDOT + DBLE(SX(I))*DBLE(SY(I))
   30     CONTINUE
      RETURN
      END
      SUBROUTINE DSWAP(N,DX,INCX,DY,INCY)
!
!     INTERCHANGE DOUBLE PRECISION DX AND DOUBLE PRECISION DY.
!     FOR I = 0 TO N-1, INTERCHANGE  DX(LX+I*INCX) AND DY(LY+I*INCY),
!     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      DOUBLE PRECISION DX(1),DY(1),DTEMP1,DTEMP2,DTEMP3
      IF(N.LE.0)RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
!
!       CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO I = 1,N
        DTEMP1 = DX(IX)
        DX(IX) = DY(IY)
        DY(IY) = DTEMP1
        IX = IX + INCX
        IY = IY + INCY
      end do

      RETURN
!
!       CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!       CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 3.
!
   20 M = MOD(N,3)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP1 = DX(I)
        DX(I) = DY(I)
        DY(I) = DTEMP1
   30 CONTINUE
      IF( N .LT. 3 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
        DTEMP1 = DX(I)
        DTEMP2 = DX(I+1)
        DTEMP3 = DX(I+2)
        DX(I) = DY(I)
        DX(I+1) = DY(I+1)
        DX(I+2) = DY(I+2)
        DY(I) = DTEMP1
        DY(I+1) = DTEMP2
        DY(I+2) = DTEMP3
   50 CONTINUE
      RETURN
   60 CONTINUE
!
!     CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
!
      NS = N*INCX
        DO 70 I=1,NS,INCX
        DTEMP1 = DX(I)
        DX(I) = DY(I)
        DY(I) = DTEMP1
   70   CONTINUE
      RETURN
      END
      INTEGER FUNCTION ICAMAX(N,CX,INCX)
!
!      RETURNS THE INDEX OF THE COMPONENT OF CX HAVING THE
!      LARGEST SUM OF MAGNITUDES OF REAL AND IMAGINARY PARTS.
!     ICAMAX = FIRST I, I = 1 TO N, TO MINIMIZE
!        ABS(REAL(CX(1-INCX+I*INCX))) + ABS(IMAG(CX(1-INCX+I*INCX)))
!
      COMPLEX CX(1)
!
      ICAMAX = 0
      IF(N.LE.0) RETURN
      ICAMAX = 1
      IF(N .LE. 1) RETURN
      NS = N*INCX
      II = 1
      SUMMAX = ABS(REAL(CX(1))) + ABS(AIMAG(CX(1)))
          DO 20 I=1,NS,INCX
          SUMRI = ABS(REAL(CX(I))) + ABS(AIMAG(CX(I)))
          IF(SUMMAX.GE.SUMRI) GO TO 10
          SUMMAX = SUMRI
          ICAMAX = II
   10     II = II + 1
   20     CONTINUE
      RETURN
      END
      INTEGER FUNCTION IDAMAX(N,DX,INCX)
!
!     FIND SMALLEST INDEX OF MAXIMUM MAGNITUDE OF DOUBLE PRECISION DX.
!     IDAMAX =  FIRST I, I = 1 TO N, TO MINIMIZE  ABS(DX(1-INCX+I*INCX))
!
      DOUBLE PRECISION DX(1),DMAX,XMAG
      IDAMAX = 0
      IF(N.LE.0) RETURN
      IDAMAX = 1
      IF(N.LE.1)RETURN
      IF(INCX.EQ.1)GOTO 20
!
!        CODE FOR INCREMENTS NOT EQUAL TO 1.
!
      DMAX = DABS(DX(1))
      NS = N*INCX
      II = 1
      DO I = 1,NS,INCX
          XMAG = DABS(DX(I))
          IF(XMAG.LE.DMAX) GO TO 5
          IDAMAX = II
          DMAX = XMAG
    5     II = II + 1
      end do

      RETURN
!
!        CODE FOR INCREMENTS EQUAL TO 1.
!
   20 DMAX = DABS(DX(1))
      DO 30 I = 2,N
          XMAG = DABS(DX(I))
          IF(XMAG.LE.DMAX) GO TO 30
          IDAMAX = I
          DMAX = XMAG
   30 CONTINUE
      RETURN
      END
      INTEGER FUNCTION ISAMAX(N,SX,INCX)
!
!     FIND SMALLEST INDEX OF MAXIMUM MAGNITUDE OF SINGLE PRECISION SX.
!     ISAMAX =  FIRST I, I = 1 TO N, TO MINIMIZE  ABS(SX(1-INCX+I*INCX))
!
      REAL SX(1),SMAX,XMAG
      ISAMAX = 0
      IF(N.LE.0) RETURN
      ISAMAX = 1
      IF(N.LE.1)RETURN
      IF(INCX.EQ.1)GOTO 20
!
!        CODE FOR INCREMENTS NOT EQUAL TO 1.
!
      SMAX = ABS(SX(1))
      NS = N*INCX
      II = 1
      do I=1,NS,INCX
          XMAG = ABS(SX(I))
          IF(XMAG.LE.SMAX) GO TO 5
          ISAMAX = II
          SMAX = XMAG
    5     II = II + 1
      end do

      RETURN
!
!        CODE FOR INCREMENTS EQUAL TO 1.
!
   20 SMAX = ABS(SX(1))
      DO 30 I = 2,N
         XMAG = ABS(SX(I))
         IF(XMAG.LE.SMAX) GO TO 30
         ISAMAX = I
         SMAX = XMAG
   30 CONTINUE
      RETURN
      END
      REAL FUNCTION SASUM(N,SX,INCX)
!
!     RETURNS SUM OF MAGNITUDES OF SINGLE PRECISION SX.
!     SASUM = SUM FROM 0 TO N-1 OF  ABS(SX(1+I*INCX))
!
      REAL SX(1)
      SASUM = 0.0E0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
!
!        CODE FOR INCREMENTS NOT EQUAL TO 1.
!
      NS = N*INCX
      DO I=1,NS,INCX
          SASUM = SASUM + ABS(SX(I))
      end do

      RETURN
!
!        CODE FOR INCREMENTS EQUAL TO 1.
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6.
!
   20 M = MOD(N,6)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SASUM = SASUM + ABS(SX(I))
   30 CONTINUE
      IF( N .LT. 6 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
        SASUM = SASUM + ABS(SX(I)) + ABS(SX(I + 1)) + ABS(SX(I + 2)) &
        + ABS(SX(I + 3)) + ABS(SX(I + 4)) + ABS(SX(I + 5))
   50 CONTINUE
      RETURN
      END
      SUBROUTINE SAXPY(N,SA,SX,INCX,SY,INCY)
!
!     OVERWRITE SINGLE PRECISION SY WITH SINGLE PRECISION SA*SX +SY.
!     FOR I = 0 TO N-1, REPLACE  SY(LY+I*INCY) WITH SA*SX(LX+I*INCX) +
!       SY(LY+I*INCY), WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N,
!       AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
!
      REAL SX(1),SY(1),SA
      IF(N.LE.0.OR.SA.EQ.0.E0) RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
!
!        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS.
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO I = 1,N
        SY(IY) = SY(IY) + SA*SX(IX)
        IX = IX + INCX
        IY = IY + INCY
      end do

      RETURN
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4.
!
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SY(I) = SY(I) + SA*SX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        SY(I) = SY(I) + SA*SX(I)
        SY(I + 1) = SY(I + 1) + SA*SX(I + 1)
        SY(I + 2) = SY(I + 2) + SA*SX(I + 2)
        SY(I + 3) = SY(I + 3) + SA*SX(I + 3)
   50 CONTINUE
      RETURN
!
!        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
!
   60 CONTINUE
      NS = N*INCX
          DO 70 I=1,NS,INCX
          SY(I) = SA*SX(I) + SY(I)
   70     CONTINUE
      RETURN
      END
      FUNCTION SCASUM(N,CX,INCX)
!     RETURNS SUMS OF MAGNITUDES OF REAL AND IMAGINARY PARTS OF
!     COMPONENTS OF CX.  NOTE THAT THIS IS NOT THE L1 NORM OF CX.
!     CASUM = SUM FROM 0 TO N-1 OF ABS(REAL(CX(1+I*INCX))) +
!             ABS(IMAG(CX(1+I*INCX)))
!
      COMPLEX CX(1)
!
      SCASUM=0.
      IF(N .LE. 0) RETURN
      NS = N*INCX
          DO I=1,NS,INCX
          SCASUM = SCASUM + ABS(REAL(CX(I))) + ABS(AIMAG(CX(I)))
          end do
      RETURN
      END
function scnrm2 ( n, x, incx )

!*******************************************************************************
!
!! SCNRM2 returns the euclidean norm of a complex vector.
!
!  Discussion:
!
!    SCNRM2 := sqrt ( sum ( conjg ( x(1:n) ) * x(1:n) ) )
!            = sqrt ( dot_product ( x(1:n), x(1:n) ) )
!
!  Reference:
!
!    Lawson, Hanson, Kincaid and Krogh,
!    Basic Linear Algebra Subprograms for FORTRAN usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, pages 308-323, 1979.
!
!  Parameters:
!
!    Input, integer N, the number of entries in the vector.
!
!    Input, complex X(*), the vector.
!
!    Input, integer INCX, the increment between successive entries of X.
!
!    Output, real SCNRM2, the norm of the vector.
!
  implicit none

  integer incx
  integer ix
  integer n
  real norm
  real, parameter :: one = 1.0E+00
  real scale
  real scnrm2
  real ssq
  real temp
  complex x(*)
  real, parameter :: zero = 0.0E+00

  if ( n < 1 .or. incx < 1 ) then

    norm  = zero

  else

    scale = zero
    ssq = one

    do ix = 1, 1 + ( n - 1 ) * incx, incx

      if ( real ( x(ix) ) /= zero ) then
        temp = abs ( real( x(ix) ) )
        if ( scale < temp ) then
          ssq = one + ssq * ( scale / temp )**2
          scale = temp
        else
          ssq = ssq + ( temp / scale )**2
        end if
      end if

      if ( aimag ( x(ix) ) /= zero ) then
        temp = abs ( aimag ( x(ix) ) )
        if ( scale < temp ) then
          ssq = one + ssq * ( scale / temp )**2
          scale = temp
        else
          ssq = ssq + ( temp / scale )**2
        end if

      end if

    end do

    norm  = scale * sqrt ( ssq )

  end if

  scnrm2 = norm

  return
end
      SUBROUTINE SCOPY(N,SX,INCX,SY,INCY)
!
!     COPY SINGLE PRECISION SX TO SINGLE PRECISION SY.
!     FOR I = 0 TO N-1, COPY  SX(LX+I*INCX) TO SY(LY+I*INCY),
!     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      REAL SX(1),SY(1)
      IF(N.LE.0)RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
!
!        CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO I = 1,N
        SY(IY) = SX(IX)
        IX = IX + INCX
        IY = IY + INCY
      end do

      RETURN
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 7.
!
   20 M = MOD(N,7)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SY(I) = SX(I)
   30 CONTINUE
      IF( N .LT. 7 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,7
        SY(I) = SX(I)
        SY(I + 1) = SX(I + 1)
        SY(I + 2) = SX(I + 2)
        SY(I + 3) = SX(I + 3)
        SY(I + 4) = SX(I + 4)
        SY(I + 5) = SX(I + 5)
        SY(I + 6) = SX(I + 6)
   50 CONTINUE
      RETURN
!
!        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
!
   60 CONTINUE
      NS = N*INCX
          DO 70 I=1,NS,INCX
          SY(I) = SX(I)
   70     CONTINUE
      RETURN
      END
      REAL FUNCTION SDOT(N,SX,INCX,SY,INCY)
!
!     RETURNS THE DOT PRODUCT OF SINGLE PRECISION SX AND SY.
!     SDOT = SUM FOR I = 0 TO N-1 OF  SX(LX+I*INCX) * SY(LY+I*INCY),
!     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      REAL SX(1),SY(1)
      SDOT = 0.0E0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.INCY) IF(INCX-1)5,20,60
    5 CONTINUE
!
!        CODE FOR UNEQUAL INCREMENTS OR NONPOSITIVE INCREMENTS.
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO I = 1,N
        SDOT = SDOT + SX(IX)*SY(IY)
        IX = IX + INCX
        IY = IY + INCY
      end do

      RETURN
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
!
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SDOT = SDOT + SX(I)*SY(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        SDOT = SDOT + SX(I)*SY(I) + SX(I + 1)*SY(I + 1) + &
         SX(I + 2)*SY(I + 2) + SX(I + 3)*SY(I + 3) + SX(I + 4)*SY(I + 4)
   50 CONTINUE
      RETURN
!
!        CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.
!
   60 CONTINUE
      NS=N*INCX
      DO 70 I=1,NS,INCX
        SDOT = SDOT + SX(I)*SY(I)
   70   CONTINUE
      RETURN
      END
      REAL FUNCTION SDSDOT(N,SB,SX,INCX,SY,INCY)
!
!     RETURNS S.P. RESULT WITH DOT PRODUCT ACCUMULATED IN D.P.
!     SDSDOT = SB + SUM FOR I = 0 TO N-1 OF SX(LX+I*INCX)*SY(LY+I*INCY),
!     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      REAL              SX(1),SY(1),SB
      DOUBLE PRECISION DSDOT
!
      DSDOT = DBLE(SB)
      IF(N .LE. 0) GO TO 30
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 40
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
      do I = 1,N
          DSDOT = DSDOT + DBLE(SX(KX))*DBLE(SY(KY))
          KX = KX + INCX
          KY = KY + INCY
      end do

   30 SDSDOT = SNGL(DSDOT)
      RETURN
   40 CONTINUE
      NS = N*INCX
          DO 50 I=1,NS,INCX
          DSDOT = DSDOT + DBLE(SX(I))*DBLE(SY(I))
   50     CONTINUE
      SDSDOT = SNGL(DSDOT)
      RETURN
      END
      REAL FUNCTION SNRM2 ( N, SX, INCX)
      INTEGER          NEXT
      REAL   SX(1),  CUTLO, CUTHI, HITEST, SUM, XMAX, ZERO, ONE
      DATA   ZERO, ONE /0.0E0, 1.0E0/
!
!     EUCLIDEAN NORM OF THE N-VECTOR STORED IN SX() WITH STORAGE
!     INCREMENT INCX .
!     IF    N .LE. 0 RETURN WITH RESULT = 0.
!     IF N .GE. 1 THEN INCX MUST BE .GE. 1
!
!           C.L.LAWSON, 1978 JAN 08
!
!     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE
!     HOPEFULLY APPLICABLE TO ALL MACHINES.
!         CUTLO = MAXIMUM OF  SQRT(U/EPS)  OVER ALL KNOWN MACHINES.
!         CUTHI = MINIMUM OF  SQRT(V)      OVER ALL KNOWN MACHINES.
!     WHERE
!         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1.
!         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)
!         V   = LARGEST  NO.            (OVERFLOW  LIMIT)
!
!     BRIEF OUTLINE OF ALGORITHM..
!
!     PHASE 1    SCANS ZERO COMPONENTS.
!     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO
!     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO
!     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M
!     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX.
!
!     VALUES FOR CUTLO AND CUTHI..
!     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER
!     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS..
!     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE
!                   UNIVAC AND DEC AT 2**(-103)
!                   THUS CUTLO = 2**(-51) = 4.44089E-16
!     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC.
!                   THUS CUTHI = 2**(63.5) = 1.30438E19
!     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC.
!                   THUS CUTLO = 2**(-33.5) = 8.23181D-11
!     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D19
!     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
!     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
      DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
!
      IF(N .GT. 0) GO TO 10
         SNRM2  = ZERO
         GO TO 300
!
   10 ASSIGN 30 TO NEXT
      SUM = ZERO
      NN = N * INCX
!                                                 BEGIN MAIN LOOP
      I = 1
   20    GO TO NEXT,(30, 50, 70, 110)
   30 IF( ABS(SX(I)) .GT. CUTLO) GO TO 85
      ASSIGN 50 TO NEXT
      XMAX = ZERO
!
!                        PHASE 1.  SUM IS ZERO
!
   50 IF( SX(I) .EQ. ZERO) GO TO 200
      IF( ABS(SX(I)) .GT. CUTLO) GO TO 85
!
!                                PREPARE FOR PHASE 2.
      ASSIGN 70 TO NEXT
      GO TO 105
!
!                                PREPARE FOR PHASE 4.
!
  100 I = J
      ASSIGN 110 TO NEXT
      SUM = (SUM / SX(I)) / SX(I)
  105 XMAX = ABS(SX(I))
      GO TO 115
!
!                   PHASE 2.  SUM IS SMALL.
!                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
!
   70 IF( ABS(SX(I)) .GT. CUTLO ) GO TO 75
!
!                     COMMON CODE FOR PHASES 2 AND 4.
!                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
!
  110 IF( ABS(SX(I)) .LE. XMAX ) GO TO 115
         SUM = ONE + SUM * (XMAX / SX(I))**2
         XMAX = ABS(SX(I))
         GO TO 200
!
  115 SUM = SUM + (SX(I)/XMAX)**2
      GO TO 200
!
!
!                  PREPARE FOR PHASE 3.
!
   75 SUM = (SUM * XMAX) * XMAX
!
!
!     FOR REAL OR D.P. SET HITEST = CUTHI/N
!     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
!
   85 HITEST = CUTHI/FLOAT( N )
!
!                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
!
      DO 95 J =I,NN,INCX
      IF(ABS(SX(J)) .GE. HITEST) GO TO 100
   95    SUM = SUM + SX(J)**2
      SNRM2 = SQRT( SUM )
      GO TO 300
!
  200 CONTINUE
      I = I + INCX
      IF ( I .LE. NN ) GO TO 20
!
!              END OF MAIN LOOP.
!
!              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
!
      SNRM2 = XMAX * SQRT(SUM)
  300 CONTINUE
      RETURN
      END
      SUBROUTINE SROT(N,SX,INCX,SY,INCY,SC,SS)
!
!     MULTIPLY THE 2 X 2 MATRIX  ( SC SS) TIMES THE 2 X N MATRIX (SX**T)
!                                (-SS SC)                        (SY**T)
!     WHERE **T INDICATES TRANSPOSE.    THE ELEMENTS OF SX ARE IN
!     SX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
!     LX = (-INCX)*N, AND SIMILARLY FOR SY USING LY AND INCY.
      REAL             SX,SY,SC,SS,ZERO,ONE,W,Z
      DIMENSION SX(1),SY(1)
!
      DATA ZERO,ONE/0.E0,1.E0/
      IF(N .LE. 0 .OR. (SS .EQ. ZERO .AND. SC .EQ. ONE)) GO TO 40
      IF(.NOT. (INCX .EQ. INCY .AND. INCX .GT. 0)) GO TO 20
!
           NSTEPS=INCX*N
           DO I=1,NSTEPS,INCX
                W=SX(I)
                Z=SY(I)
                SX(I)=SC*W+SS*Z
                SY(I)=-SS*W+SC*Z
           end do
           GO TO 40
!
   20 CONTINUE
           KX=1
           KY=1
!
           IF(INCX .LT. 0) KX=1-(N-1)*INCX
           IF(INCY .LT. 0) KY=1-(N-1)*INCY
!
           DO 30 I=1,N
                W=SX(KX)
                Z=SY(KY)
                SX(KX)=SC*W+SS*Z
                SY(KY)=-SS*W+SC*Z
                KX=KX+INCX
                KY=KY+INCY
   30           CONTINUE
   40 CONTINUE
!
      RETURN
      END
      SUBROUTINE SROTG(SA,SB,SC,SS)
!
!     DESIGNED BY C.L.LAWSON, JPL, 1977 SEPT 08
!
!
!     CONSTRUCT THE GIVENS TRANSFORMATION
!
!         ( SC  SS )
!     G = (        ) ,    SC**2 + SS**2 = 1 ,
!         (-SS  SC )
!
!     WHICH ZEROS THE SECOND ENTRY OF THE 2-VECTOR  (SA,SB)**T .
!
!     THE QUANTITY R = (+/-)SQRT(SA**2 + SB**2) OVERWRITES SA IN
!     STORAGE.  THE VALUE OF SB IS OVERWRITTEN BY A VALUE Z WHICH
!     ALLOWS SC AND SS TO BE RECOVERED BY THE FOLLOWING ALGORITHM:
!           IF Z=1  SET  SC=0.  AND  SS=1.
!           IF ABS(Z) .LT. 1  SET  SC=SQRT(1-Z**2)  AND  SS=Z
!           IF ABS(Z) .GT. 1  SET  SC=1/Z  AND  SS=SQRT(1-SC**2)
!
!     NORMALLY, THE SUBPROGRAM SROT(N,SX,INCX,SY,INCY,SC,SS) WILL
!     NEXT BE CALLED TO APPLY THE TRANSFORMATION TO A 2 BY N MATRIX.
!
! ------------------------------------------------------------------
!
      IF (ABS(SA) .LE. ABS(SB)) GO TO 10
!
! *** HERE ABS(SA) .GT. ABS(SB) ***
!
      U = SA + SA
      V = SB / U
!
!     NOTE THAT U AND R HAVE THE SIGN OF SA
!
      R = SQRT(.25 + V**2) * U
!
!     NOTE THAT SC IS POSITIVE
!
      SC = SA / R
      SS = V * (SC + SC)
      SB = SS
      SA = R
      RETURN
!
! *** HERE ABS(SA) .LE. ABS(SB) ***
!
   10 IF (SB .EQ. 0.) GO TO 20
      U = SB + SB
      V = SA / U
!
!     NOTE THAT U AND R HAVE THE SIGN OF SB
!     (R IS IMMEDIATELY STORED IN SA)
!
      SA = SQRT(.25 + V**2) * U
!
!     NOTE THAT SS IS POSITIVE
!
      SS = SB / SA
      SC = V * (SS + SS)
      IF (SC .EQ. 0.) GO TO 15
      SB = 1. / SC
      RETURN
   15 SB = 1.
      RETURN
!
! *** HERE SA = SB = 0. ***
!
   20 SC = 1.
      SS = 0.
      RETURN
!
      END
      SUBROUTINE SROTM (N,SX,INCX,SY,INCY,SPARAM)
!
!     APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
!
!     (SX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF SX ARE IN
!     (DX**T)
!
!     SX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
!     LX = (-INCX)*N, AND SIMILARLY FOR SY USING USING LY AND INCY.
!     WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS..
!
!     SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0
!
!       (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)
!     H=(          )    (          )    (          )    (          )
!       (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0).
!     SEE  SROTMG FOR A DESCRIPTION OF DATA STORAGE IN SPARAM.
!
      DIMENSION SX(1),SY(1),SPARAM(5)
      DATA ZERO,TWO/0.E0,2.E0/
!
      SFLAG=SPARAM(1)
      IF(N .LE. 0 .OR.(SFLAG+TWO.EQ.ZERO)) GO TO 140
          IF(.NOT.(INCX.EQ.INCY.AND. INCX .GT.0)) GO TO 70
!
               NSTEPS=N*INCX
               IF(SFLAG) 50,10,30
   10          CONTINUE
               SH12=SPARAM(4)
               SH21=SPARAM(3)
                    DO 20 I=1,NSTEPS,INCX
                    W=SX(I)
                    Z=SY(I)
                    SX(I)=W+Z*SH12
                    SY(I)=W*SH21+Z
   20               CONTINUE
               GO TO 140
   30          CONTINUE
               SH11=SPARAM(2)
               SH22=SPARAM(5)
                    DO 40 I=1,NSTEPS,INCX
                    W=SX(I)
                    Z=SY(I)
                    SX(I)=W*SH11+Z
                    SY(I)=-W+SH22*Z
   40               CONTINUE
               GO TO 140
   50          CONTINUE
               SH11=SPARAM(2)
               SH12=SPARAM(4)
               SH21=SPARAM(3)
               SH22=SPARAM(5)
                    DO 60 I=1,NSTEPS,INCX
                    W=SX(I)
                    Z=SY(I)
                    SX(I)=W*SH11+Z*SH12
                    SY(I)=W*SH21+Z*SH22
   60               CONTINUE
               GO TO 140
   70     CONTINUE
          KX=1
          KY=1
          IF(INCX .LT. 0) KX=1+(1-N)*INCX
          IF(INCY .LT. 0) KY=1+(1-N)*INCY
!
          IF(SFLAG)120,80,100
   80     CONTINUE
          SH12=SPARAM(4)
          SH21=SPARAM(3)
               DO 90 I=1,N
               W=SX(KX)
               Z=SY(KY)
               SX(KX)=W+Z*SH12
               SY(KY)=W*SH21+Z
               KX=KX+INCX
               KY=KY+INCY
   90          CONTINUE
          GO TO 140
  100     CONTINUE
          SH11=SPARAM(2)
          SH22=SPARAM(5)
               DO 110 I=1,N
               W=SX(KX)
               Z=SY(KY)
               SX(KX)=W*SH11+Z
               SY(KY)=-W+SH22*Z
               KX=KX+INCX
               KY=KY+INCY
  110          CONTINUE
          GO TO 140
  120     CONTINUE
          SH11=SPARAM(2)
          SH12=SPARAM(4)
          SH21=SPARAM(3)
          SH22=SPARAM(5)
               DO 130 I=1,N
               W=SX(KX)
               Z=SY(KY)
               SX(KX)=W*SH11+Z*SH12
               SY(KY)=W*SH21+Z*SH22
               KX=KX+INCX
               KY=KY+INCY
  130          CONTINUE
  140     CONTINUE
          RETURN
          END
      SUBROUTINE SROTMG (SD1,SD2,SX1,SY1,SPARAM)
!
!     CONSTRUCT THE MODIFIED GIVENS TRANSFORMATION MATRIX H WHICH ZEROS
!     THE SECOND COMPONENT OF THE 2-VECTOR  (SQRT(SD1)*SX1,SQRT(SD2)*
!     SY2)**T.
!     WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS..
!
!     SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0
!
!       (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)
!     H=(          )    (          )    (          )    (          )
!       (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0).
!     LOCATIONS 2-4 OF SPARAM CONTAIN SH11,SH21,SH12, AND SH22
!     RESPECTIVELY. (VALUES OF 1.E0, -1.E0, OR 0.E0 IMPLIED BY THE
!     VALUE OF SPARAM(1) ARE NOT STORED IN SPARAM.)
!
!     THE VALUES OF GAMSQ AND RGAMSQ SET IN THE DATA STATEMENT MAY BE
!     INEXACT.  THIS IS OK AS THEY ARE ONLY USED FOR TESTING THE SIZE
!     OF SD1 AND SD2.  ALL ACTUAL SCALING OF DATA IS DONE USING GAM.
!
      DIMENSION SPARAM(5)
!
      DATA ZERO,ONE,TWO /0.E0,1.E0,2.E0/
      DATA GAM,GAMSQ,RGAMSQ/4096.E0,1.67772E7,5.96046E-8/
      IF(.NOT. SD1 .LT. ZERO) GO TO 10
!       GO ZERO-H-D-AND-SX1..
          GO TO 60
   10 CONTINUE
!     CASE-SD1-NONNEGATIVE
      SP2=SD2*SY1
      IF(.NOT. SP2 .EQ. ZERO) GO TO 20
          SFLAG=-TWO
          GO TO 260
!     REGULAR-CASE..
   20 CONTINUE
      SP1=SD1*SX1
      SQ2=SP2*SY1
      SQ1=SP1*SX1
!
      IF(.NOT. ABS(SQ1) .GT. ABS(SQ2)) GO TO 40
          SH21=-SY1/SX1
          SH12=SP2/SP1
!
          SU=ONE-SH12*SH21
!
          IF(.NOT. SU .LE. ZERO) GO TO 30
!         GO ZERO-H-D-AND-SX1..
               GO TO 60
   30     CONTINUE
               SFLAG=ZERO
               SD1=SD1/SU
               SD2=SD2/SU
               SX1=SX1*SU
!         GO SCALE-CHECK..
               GO TO 100
   40 CONTINUE
          IF(.NOT. SQ2 .LT. ZERO) GO TO 50
!         GO ZERO-H-D-AND-SX1..
               GO TO 60
   50     CONTINUE
               SFLAG=ONE
               SH11=SP1/SP2
               SH22=SX1/SY1
               SU=ONE+SH11*SH22
               STEMP=SD2/SU
               SD2=SD1/SU
               SD1=STEMP
               SX1=SY1*SU
!         GO SCALE-CHECK
               GO TO 100
!     PROCEDURE..ZERO-H-D-AND-SX1..
   60 CONTINUE
          SFLAG=-ONE
          SH11=ZERO
          SH12=ZERO
          SH21=ZERO
          SH22=ZERO
!
          SD1=ZERO
          SD2=ZERO
          SX1=ZERO
!         RETURN..
          GO TO 220
!     PROCEDURE..FIX-H..
   70 CONTINUE
      IF(.NOT. SFLAG .GE. ZERO) GO TO 90
!
          IF(.NOT. SFLAG .EQ. ZERO) GO TO 80
          SH11=ONE
          SH22=ONE
          SFLAG=-ONE
          GO TO 90
   80     CONTINUE
          SH21=-ONE
          SH12=ONE
          SFLAG=-ONE
   90 CONTINUE
      GO TO IGO,(120,150,180,210)
!     PROCEDURE..SCALE-CHECK
  100 CONTINUE
  110     CONTINUE
          IF(.NOT. SD1 .LE. RGAMSQ) GO TO 130
               IF(SD1 .EQ. ZERO) GO TO 160
               ASSIGN 120 TO IGO
!              FIX-H..
               GO TO 70
  120          CONTINUE
               SD1=SD1*GAM**2
               SX1=SX1/GAM
               SH11=SH11/GAM
               SH12=SH12/GAM
          GO TO 110
  130 CONTINUE
  140     CONTINUE
          IF(.NOT. SD1 .GE. GAMSQ) GO TO 160
               ASSIGN 150 TO IGO
!              FIX-H..
               GO TO 70
  150          CONTINUE
               SD1=SD1/GAM**2
               SX1=SX1*GAM
               SH11=SH11*GAM
               SH12=SH12*GAM
          GO TO 140
  160 CONTINUE
  170     CONTINUE
          IF(.NOT. ABS(SD2) .LE. RGAMSQ) GO TO 190
               IF(SD2 .EQ. ZERO) GO TO 220
               ASSIGN 180 TO IGO
!              FIX-H..
               GO TO 70
  180          CONTINUE
               SD2=SD2*GAM**2
               SH21=SH21/GAM
               SH22=SH22/GAM
          GO TO 170
  190 CONTINUE
  200     CONTINUE
          IF(.NOT. ABS(SD2) .GE. GAMSQ) GO TO 220
               ASSIGN 210 TO IGO
!              FIX-H..
               GO TO 70
  210          CONTINUE
               SD2=SD2/GAM**2
               SH21=SH21*GAM
               SH22=SH22*GAM
          GO TO 200
  220 CONTINUE
          IF(SFLAG)250,230,240
  230     CONTINUE
               SPARAM(3)=SH21
               SPARAM(4)=SH12
               GO TO 260
  240     CONTINUE
               SPARAM(2)=SH11
               SPARAM(5)=SH22
               GO TO 260
  250     CONTINUE
               SPARAM(2)=SH11
               SPARAM(3)=SH21
               SPARAM(4)=SH12
               SPARAM(5)=SH22
  260 CONTINUE
          SPARAM(1)=SFLAG
          RETURN
      END
      SUBROUTINE SSCAL(N,SA,SX,INCX)
!
!     REPLACE SINGLE PRECISION SX BY SINGLE PRECISION SA*SX.
!     FOR I = 0 TO N-1, REPLACE SX(1+I*INCX) WITH  SA * SX(1+I*INCX)
!
      REAL SA,SX(1)
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
!
!        CODE FOR INCREMENTS NOT EQUAL TO 1.
!
      NS = N*INCX
      do I = 1,NS,INCX
          SX(I) = SA*SX(I)
      end do
      RETURN
!
!        CODE FOR INCREMENTS EQUAL TO 1.
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
!
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SX(I) = SA*SX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        SX(I) = SA*SX(I)
        SX(I + 1) = SA*SX(I + 1)
        SX(I + 2) = SA*SX(I + 2)
        SX(I + 3) = SA*SX(I + 3)
        SX(I + 4) = SA*SX(I + 4)
   50 CONTINUE
      RETURN
      END
      SUBROUTINE SSWAP (N,SX,INCX,SY,INCY)
!
!     INTERCHANGE SINGLE PRECISION SX AND SINGLE PRECISION SY.
!     FOR I = 0 TO N-1, INTERCHANGE  SX(LX+I*INCX) AND SY(LY+I*INCY),
!     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
!     DEFINED IN A SIMILAR WAY USING INCY.
!
      REAL SX(1),SY(1),STEMP1,STEMP2,STEMP3
      IF(N.LE.0)RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
!
!       CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO I = 1,N
        STEMP1 = SX(IX)
        SX(IX) = SY(IY)
        SY(IY) = STEMP1
        IX = IX + INCX
        IY = IY + INCY
      end do

      RETURN
!
!       CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!       CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 3.
!
   20 M = MOD(N,3)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        STEMP1 = SX(I)
        SX(I) = SY(I)
        SY(I) = STEMP1
   30 CONTINUE
      IF( N .LT. 3 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
        STEMP1 = SX(I)
        STEMP2 = SX(I+1)
        STEMP3 = SX(I+2)
        SX(I) = SY(I)
        SX(I+1) = SY(I+1)
        SX(I+2) = SY(I+2)
        SY(I) = STEMP1
        SY(I+1) = STEMP2
        SY(I+2) = STEMP3
   50 CONTINUE
      RETURN
   60 CONTINUE
!
!     CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
!
      NS = N*INCX
        DO 70 I=1,NS,INCX
        STEMP1 = SX(I)
        SX(I) = SY(I)
        SY(I) = STEMP1
   70   CONTINUE
      RETURN
      END
      subroutine vexopy2 (nn,v,x,y,icode)
!
! ... vexopy2 computes  v = x  op  y  where v, x, and y are vectors
! ... and op is one of the operations  + - .
! ... parameter list --
!          n       length of vectors  (= nn)
!          v,x,y   vectors of length n
!          icode   key indicating operation
!            = 1      for addition
!            = 2      for subtraction
!
      real    v(1), x(1), y(1)
!
      n = nn
      if(icode.eq.2) go to 20
!
! ... compute   v = x + y
!
      do 15 i = 1,n
         v(i) = x(i) + y(i)
 15   continue
      return
!
! ... compute   v = x - y
!
 20   do 25 i = 1,n
         v(i) = x(i) - y(i)
 25   continue
      return
      end
      subroutine vfill2 (n,v,val)
!
!     vfill2 fills a vector, v, with a constant value, val.
!
      real    v(n)
      if (n .le. 0) return
      nr=mod(n,4)
!
! The following construct assumes a zero pass do loop.
!
      is=1
      goto(1,2,3,4), nr+1
    4   is=4
        v(1)=val
        v(2)=val
        v(3)=val
        goto 1
    3   is=3
        v(1)=val
        v(2)=val
        goto 1
    2   is=2
        v(1)=val
    1 do i=is,n,4
        v(i)  =val
        v(i+1)=val
        v(i+2)=val
        v(i+3)=val
      end do

      return
      end
      subroutine saxpyx (nn,saa,sx,incxx,sy,incyy)
!
!   purpose             - compute a constant times a vector plus
!                           a vector, all single precision result
!                           ends up in x
!
!   usage               - call saxpyx (n,sa,sx,incx,sy,incy)
!   arguments    n      - length of vectors sx and sy. (input) (= nn)
!                sa     - real scalar. (input) (= saa)
!                sx     - real vector of length max(n*iabs(incx),1).
!                           (input/output)
!                incx   - displacement between elements of sx. (input)
!                         (= incxx)
!                           x(i) is defined to be..
!                           sx(1+(i-1)*incx) if incx .ge. 0 or
!                           sx(1+(i-n)*incx) if incx .lt. 0.
!                sy     - real vector of length max(n*iabs(incy),1).
!                           (input)
!                incy   - displacement between elements of sy. (input)
!                         (= incyy) see incx
!
      real               sy(1), sx(1), saa
!                                  first executable statement
      n    = nn
      sa   = saa
      incx = incxx
      incy = incyy
      if (n .le. 0) return
      if (incx .eq. incy) if (incx - 1) 5,15,35
    5 continue
!                                  code for nonequal or nonpositive
!                                    increments.
      ix = 1
      iy = 1
      if (incx .lt. 0) ix = (-n + 1)*incx + 1
      if (incy .lt. 0) iy = (-n + 1)*incy + 1
      do i = 1,n
         sx(ix) = sy(iy) + sa*sx(ix)
         ix = ix + incx
         iy = iy + incy
      end do

      return
!                                  code for both increments equal to 1
   15 do 20 i = 1,n
         sx(i) = sy(i) + sa*sx(i)
   20 continue
      return
!                                  code for equal, positive, nonunit
!                                    increments.
   35 ns = n*incx
      do 40 i = 1,ns,incx
         sx(i) = sy(i) + sa*sx(i)
   40 continue
      return
      end
      SUBROUTINE DLPDOC
!***BEGIN PROLOGUE  DLPDOC
!***DATE WRITTEN   890404   (YYMMDD)
!***REVISION DATE  890404   (YYMMDD)
!***CATEGORY NO.  D2B, Z
!***KEYWORDS  LIBRARY=SLATEC(DLAP),
!             TYPE=DOUBLE PRECISION(DLPDOC-A),
!             Sparse Iterative Methods, Iterative Improvement,
!             Preconditioned Conjugate Gradient, Normal Equations,
!             BiConjugate Gradient, BiConjugate Gradient Squared,
!             Orthomin, Generalize Minimum Residual, Documentation
!***AUTHOR  Seager, Mark. K., (LLNL)
!             User Systems Division
!             Lawrence Livermore National Laboratory
!             PO BOX 808, L-300
!             Livermore, CA 94550
!             (FTS) 543-3141, (415) 423-3141
!             seager@lll-crg.llnl.gov
!***PURPOSE  Sparse Linear Algebra Package Version 2.0
!            Routines to  solve large sparse symmetric and nonsymmetric
!            positive definite linear systems, Ax = b, using precondit-
!            ioned iterative methods.
!
!            This package was originally derived from a set of iterative
!            routines written by Anne Greenbaum, "Routines for Solving
!            Large Sparse Linear Systems", Lawrence Livermore Nat.
!            Laboratory, Livermore Computing Center, January 1986
!            Tentacle, pp 15-21.
!***DESCRIPTION
!                                 The
!                    Sparse Linear Algebra Package
!                      Double Precision Routines
!
!                @@@@@@@  @            @@@    @@@@@@@@
!               @       @ @           @   @   @       @
!               @         @          @     @  @       @
!                @@@@@@@  @         @       @ @@@@@@@@
!                       @ @         @@@@@@@@@ @
!               @       @ @         @       @ @
!                @@@@@@@  @@@@@@@@@ @       @ @
!
!      @       @                            @@@@@@@        @@@@@
!      @       @                           @       @      @    @@
!      @       @  @@@@@@@  @ @@                    @     @    @  @
!      @       @ @       @ @@  @             @@@@@@      @   @   @
!       @     @  @@@@@@@@@ @                @            @  @    @
!        @   @   @         @               @         @@@  @@    @
!         @@@     @@@@@@@  @               @@@@@@@@@ @@@   @@@@@
!
!
!    =================================================================
!    ========================== Introduction =========================
!    =================================================================
!    This document  contains the specifications for  the  SLAP Version
!    2.0 package, a Fortran 77  package  for  the  solution  of  large
!    sparse   linear systems, Ax  =  b,  via  preconditioned iterative
!    methods.   Included in  this  package are "core"  routines  to do
!    Iterative   Refinement  (Jacobi's  method),  Conjugate  Gradient,
!    Conjugate Gradient on the normal equations, AA'y = b,  (where x =
!    A'y and  A' denotes the  transpose of   A), BiConjugate Gradient,
!    BiConjugate  Gradient  Squared, Orthomin and  Generalized Minimum
!    Residual Iteration.    These "core" routines   do  not  require a
!    "fixed"   data  structure   for storing  the   matrix  A  and the
!    preconditioning   matrix  M.   The  user  is free  to  choose any
!    structure that facilitates  efficient solution  of the problem at
!    hand.  The drawback  to this approach  is that the user must also
!    supply at least two routines  (MATVEC and MSOLVE,  say).   MATVEC
!    must calculate, y = Ax, given x and the user's data structure for
!    A.  MSOLVE must solve,  r = Mz, for z (*NOT*  r) given r  and the
!    user's data  structure for  M (or its  inverse).  The user should
!    choose   M  so that MA   is  approximately the   identity and the
!    solution step r = Mz is "easy" to  solve.  For some of the "core"
!    routines (Orthomin,  BiConjugate Gradient and  Conjugate Gradient
!    on the  normal equations)   the user must  also  supply  a matrix
!    transpose times   vector  routine  (MTTVEC,  say)  and (possibly,
!    depending    on the "core"  method)   a  routine  that solves the
!    transpose  of   the   preconditioning    step     (MTSOLV,  say).
!    Specifically, MTTVEC is a routine which calculates y = A'x, given
!    x and the user's data structure for A (A' is the transpose of A).
!    MTSOLV is a routine which solves the system r = M'z for z given r
!    and the user's data structure for M.
!
!    This process of writing the matrix vector operations  can be time
!    consuming and error  prone.  To alleviate  these problems we have
!    written drivers   for  the  "core" methods  that  assume the user
!    supplies one of two specific data structures (SLAP Triad and SLAP
!    Column format), see  below.  Utilizing these  data structures  we
!    have augmented   each  "core" method  with   two preconditioners:
!    Diagonal  Scaling and Incomplete Factorization.  Diagonal scaling
!    is easy to implement, vectorizes very  well and for problems that
!    are  not too  ill-conditioned  reduces the  number  of iterations
!    enough   to warrant its use.  On   the other  hand, an Incomplete
!    factorization  (Incomplete  Cholesky for  symmetric systems   and
!    Incomplete LU for nonsymmetric  systems) may  take much longer to
!    calculate, but it reduces the iteration count (for most problems)
!    significantly.  Our implementations  of IC and ILU  vectorize for
!    machines with hardware gather scatter, but the vector lengths can
!    be quite short if  the  number  of nonzeros   in a column is  not
!    large.
!
!    =================================================================
!    ==================== Supplied Data Structures ===================
!    =================================================================
!    The following describes the data   structures supplied  with  the
!    package: SLAP Triad and Column formats.
!
!    ====================== S L A P Triad format =====================
!    In the SLAP Triad format only the non-zeros are stored.  They may
!    appear in *ANY* order.  The user supplies three  arrays of length
!    NELT, where NELT  is the   number of  non-zeros  in the   matrix:
!    (IA(NELT),  JA(NELT), A(NELT)).  If  the matrix is symmetric then
!    one need only store the lower triangle (including  the  diagonal)
!    and NELT would be the corresponding  number  of non-zeros stored.
!    For each non-zero the user puts the row and column  index of that
!    matrix  element   in the  IA  and JA  arrays.  The  value  of the
!    non-zero matrix element is placed  in  the corresponding location
!    of  the A array.   This  is an extremely  easy  data structure to
!    generate.  On the other hand, it is not very  efficient on vector
!    computers for the iterative  solution of  linear systems.  Hence,
!    SLAP changes this input data structure to  the SLAP Column format
!    for the iteration (but does not change it back).
!
!    Here  is an example   of  the  SLAP  Triad storage  format  for a
!    nonsymmetric 5x5 Matrix.  NELT=11.   Recall that the  entries may
!    appear in any order.
!
!     5x5 Matrix       SLAP Triad format for 5x5 matrix on left.
!                           1  2  3  4  5  6  7  8  9 10 11
!    |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
!    |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
!    | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
!    | 0  0  0 44  0|
!    |51  0 53  0 55|
!
!       =================== S L A P Column format ==================
!       This routine requires  that the  matrix  A be  stored in the
!       SLAP Column format.  In this format the non-zeros are stored
!       counting down columns (except for  the diagonal entry, which
!       must appear first in each  "column")  and are stored  in the
!       double precision array A.   In other words,  for each column
!       in the matrix put the diagonal entry in  A.  Then put in the
!       other non-zero  elements going down  the column (except  the
!       diagonal) in order.   The  IA array holds the  row index for
!       each non-zero.  The JA array holds the offsets  into the IA,
!       A arrays  for  the  beginning  of each   column.   That  is,
!       IA(JA(ICOL)),  A(JA(ICOL)) points   to the beginning  of the
!       ICOL-th   column    in    IA and   A.      IA(JA(ICOL+1)-1),
!       A(JA(ICOL+1)-1) points to  the  end of the   ICOL-th column.
!       Note that we always have  JA(N+1) = NELT+1,  where N is  the
!       number of columns in  the matrix and NELT  is the number  of
!       non-zeros in the matrix.
!
!       Here is an example of the  SLAP Column  storage format for a
!       5x5 Matrix (in the A and IA arrays '|'  denotes the end of a
!       column):
!
!       5x5 Matrix      SLAP Column format for 5x5 matrix on left.
!       1  2  3    4  5    6  7    8    9 10 11
!       |11 12  0  0 15|   A: 11 21 51 | 22 12 | 33 53 | 44 | 55 15 35
!       |21 22  0  0  0|  IA:  1  2  5 |  2  1 |  3  5 |  4 |  5  1  3
!       | 0  0 33  0 35|  JA:  1  4  6    8  9   12
!       | 0  0  0 44  0|
!       |51  0 53  0 55|
!
!    =================================================================
!    ======================= Naming Conventions ======================
!    =================================================================
!    SLAP  iterative  methods,    matrix vector    and  preconditioner
!    calculation  routines   follow a naming   convention  which, when
!    understood, allows one to determine the iterative method and data
!    structure(s) used.  The  subroutine  naming convention  takes the
!    following form:
!                          P[S][M]DESC
!    where P stands  for the precision (or  data type)  of the routine
!    and is required in   all names, S denotes   whether  or  not  the
!    routine requires the SLAP Triad or Column format (it  does if the
!    second letter  of the name   is S and   does not otherwise),  the
!    optional  M   stands for the   type of preconditioner used  (only
!    appears in drivers for  "core" routines) and  DESC is some number
!    of letters describing the method  or purpose of the  routine.  In
!    this  incarnation of SLAP  only single  precision  data types are
!    supported (no double precision or complex data type routines have
!    been written).   Hence,  all routines start with an   S,  boring.
!    The  brackets around  S  and M  designate  that these  fields are
!    optional.
!
!    The  following is a  list  of  the  "DESC"  fields for  iterative
!    methods  and   their meaning: BCG:   BiConjugate   Gradient;  CG:
!    Conjugate  Gradient; CGN:   Conjugate  Gradient   on   the Normal
!    equations; CGS, CS: biConjugate Gradient Squared; GMRES, GMR, GM:
!    Generalized Minimum  RESidual; IR, R:  Iterative Refinement; JAC:
!    JACobi's method; GS: Gauss-Seidel; OMN, OM: Orthomin.
!
!    Here are some examples of the routines:
!    1) DBCG: Double  precision BiConjugate Gradient  "core"  routine.
!    On can deduce that this is a "core" routine,  because the S and M
!    fields are   missing  and BiConjugate  Gradient  is an  iterative
!    method. 2) DSDBCG: Dingle precision, SLAP data structure BCG with
!    Diagonal scaling.  3)   DSLUBC: Double    precision,  BCG    with
!    incomplete  LU factorization  as   the preconditioning.  4)  DCG:
!    Double precision Conjugate  Gradient  "core" routine.  5)  DSDCG:
!    Double precision,  SLAP data  structure  Conjugate  Gradient with
!    Diagonal scaling.  6)  DSICCG:  Double   precision,    SLAP  data
!    structure   Conjugate    Gradient  with    Incomplete    Cholesky
!    factorization preconditioning.
!
!
!    =================================================================
!    ===================== USER CALLABLE ROUTINES ====================
!    =================================================================
!    The following is a list of  the "user callable" SLAP routines and
!    their one  line description.  The headers denote  the  file names
!    where the routines can be found, as distributed for Unix systems.
!
!    ============================= DBCG.F ============================
!    DBCG: Preconditioned BiConjugate Gradient Sparse Ax=b solver.
!    DSDBCG: Diagonally Scaled BiConjugate Gradient Sparse Ax=b solver.
!    DSLUBC: Incomplete LU BiConjugate Gradient Sparse Ax=b solver.
!
!    ============================= DCG.F =============================
!    DCG: Preconditioned Conjugate Gradient iterative Ax=b solver.
!    DSDCG: Diagonally Scaled Conjugate Gradient Sparse Ax=b Solver.
!    DSICCG: Incomplete Cholesky Conjugate Gradient Sparse Ax=b Solver.
!
!    ============================= DCGN.F ============================
!    DCGN: Preconditioned CG Sparse Ax=b Solver for Normal Equations.
!    DSDCGN: Diagonally Scaled CG Sparse Ax=b Solver for Normal Eqn's.
!    DSLUCN: Incomplete LU CG Sparse Ax=b Solver for Normal Equations.
!
!    ============================= DCGS.F ============================
!    DCGS: Preconditioned BiConjugate Gradient Sparse Ax=b solver.
!    DSDCGS: Diagonally Scaled CGS Sparse Ax=b Solver.
!    DSLUCS: Incomplete LU BiConjugate Gradient Sparse Ax=b solver.
!
!    ============================ DGMRES.F ===========================
!    DGMRES: Preconditioned GMRES iterative sparse Ax=b solver.
!    DSDGMR: Diagonally scaled GMRES iterative sparse Ax=b solver.
!    DSLUGM: Incomplete LU GMRES iterative sparse Ax=b solver.
!
!    ============================= DIR.F =============================
!    DIR: Preconditioned Iterative Refinement sparse Ax = b solver.
!    DSJAC: Jacobi's method iterative sparse Ax = b solver.
!    DSGS: Gauss-Seidel method iterative sparse Ax = b solver.
!    DSILUR: Incomplete LU Iterative Refinement sparse Ax = b solver.
!
!    ============================ DOMN.F =============================
!    DOMN: Preconditioned Orthomin Sparse Iterative Ax=b Solver.
!    DSDOMN: Diagonally Scaled Orthomin Sparse Iterative Ax=b Solver.
!    DSLUOM: Incomplete LU Orthomin Sparse Iterative Ax=b Solver.
!
!    ============================ DMSET.F ============================
!    DSDS: Diagonal Scaling Preconditioner SLAP Set Up.
!    DSDSCL: Diagonally Scales/Unscales a SLAP Column Matrix.
!    DSD2S: Diagonal Scaling Preconditioner SLAP Normal Eqns Set Up.
!    DS2LT: Lower Triangle Preconditioner SLAP Set Up.
!    DSICS: Incomplete Cholesky Decomp. Preconditioner SLAP Set Up.
!    DSILUS: Incomplete LU Decomposition Preconditioner SLAP Set Up.
!
!    =========================== DMVOPS.F ============================
!       Most of the incomplete  factorization  (LL' and LDU) solvers
!       in this  file require an  intermediate routine  to translate
!       from the SLAP MSOLVE(N, R, Z, NELT, IA,  JA, A, ISYM, RWORK,
!       IWORK) calling  convention to the calling  sequence required
!       by  the solve routine.   This generally  is  accomplished by
!       fishing out pointers to the preconditioner (stored in RWORK)
!       from the  IWORK  array and then making a call to the routine
!       that actually does the backsolve.
!
!    DSMV: SLAP Column Format Sparse Matrix Vector Product.
!    DSMTV: SLAP Column Format Sparse Matrix (transpose) Vector Prod.
!    DSDI: Diagonal Matrix Vector Multiply.
!    DSLI: SLAP MSOLVE for Lower Triangle Matrix (set up for SSLI2).
!    DSLI2: Lower Triangle Matrix Backsolve.
!    DSLLTI: SLAP MSOLVE for LDL' (IC) Fact. (set up for SLLTI2).
!    DLLTI2: Back solve routine for LDL' Factorization.
!    DSLUI: SLAP MSOLVE for LDU Factorization (set up for SSLUI2).
!    DSLUI2: SLAP Back solve for LDU Factorization.
!    DSLUTI: SLAP MTSOLV for LDU Factorization (set up for SSLUI4).
!    DSLUI4: SLAP back solve for LDU Factorization.
!    DSMMTI: SLAP MSOLVE for LDU Fact of Normal Eq (set up for SSMMI2).
!    DSMMI2: SLAP Back solve for LDU Factorization of Normal Equations.
!
!    ========================== SLAPUTIL.F ==========================
!    DCHKW: SLAP WORK/IWORK Array Bounds Checker.
!    QS2I1D: Quick Sort Integer array coupled with int and double
!            precision arrays.
!    DS2Y: SLAP Triad to SLAP Column Format Converter.
!    DCPPLT: Printer Plot of SLAP Column Format Matrix.
!    DTOUT: Write out SLAP Triad Format Linear System.
!    DTIN: Read in SLAP Triad Format Linear System.
!
!    =================================================================
!    ====================== Which Method To Use ======================
!    =================================================================
!                          BACKGROUND
!    In solving a large sparse linear system Ax = b using an iterative
!    method, it   is  not necessary to actually   store  the matrix A.
!    Rather, what is needed is a procedure  for multiplying the matrix
!    A times a given vector y to obtain the matrix-vector product, Ay.
!    SLAP has been written to take advantage of this fact.  The higher
!    level routines in the package require storage only of the nonzero
!    elements of   A (and  their  positions), and  even this   can  be
!    avoided, if the  user  writes his own subroutine for  multiplying
!    the matrix times a vector  and   calls the lower-level  iterative
!    routines in the package.
!
!    If  the matrix A is ill-conditioned,  then most iterative methods
!    will be slow to converge (if they converge  at all!).  To improve
!    the  convergence  rate,  one  may use  a "matrix  splitting," or,
!    "preconditioning matrix," say, M.  It is then necessary to solve,
!    at each iteration, a linear system  with coefficient matrix M.  A
!    good preconditioner  M should have  two  properties: (1) M should
!    "approximate" A, in the sense that the  matrix inv(M)*A  (or some
!    variant  thereof) is better conditioned  than the original matrix
!    A; and  (2) linear  systems with coefficient  matrix M should  be
!    much easier  to solve  than  the original system with coefficient
!    matrix   A.   Preconditioning routines  in the   SLAP package are
!    separate from the  iterative   routines,  so   that any of    the
!    preconditioners provided in the package,   or one that the   user
!    codes himself, can be used with any of the iterative routines.
!
!                        CHOICE OF PRECONDITIONER
!    If you  willing   to live with   either the SLAP Triad or  Column
!    matrix data structure  you  can then  choose one  of two types of
!    preconditioners   to   use:   diagonal  scaling    or  incomplete
!    factorization.  To  choose   between these two   methods requires
!    knowing  something  about the computer you're going  to run these
!    codes on  and how well incomplete factorization  approximates the
!    inverse of your matrix.
!
!    Let's   suppose you have   a scalar  machine.   Then,  unless the
!    incomplete factorization is very,  very poor this  is *GENERALLY*
!    the method to choose.  It  will reduce the  number of  iterations
!    significantly and is not all  that expensive  to compute.  So  if
!    you have just one  linear system to solve  and  "just want to get
!    the job  done" then try  incomplete factorization first.   If you
!    are thinking of integrating some SLAP  iterative method into your
!    favorite   "production  code" then  try incomplete  factorization
!    first,  but  also check  to see that  diagonal  scaling is indeed
!    slower for a large sample of test problems.
!
!    Let's now  suppose  you have  a  vector  computer  with  hardware
!    gather/scatter support (Cray X-MP, Y-MP, SCS-40 or Cyber 205, ETA
!    10, ETA Piper  or Convex C-1,  etc.).   Then  it's much harder to
!    choose  between the  two  methods.   The  versions  of incomplete
!    factorization in SLAP do in fact vectorize, but have short vector
!    lengths and the factorization step is relatively  more expensive.
!    Hence,  for  most problems (i.e.,  unless  your  problem  is  ill
!    conditioned,  sic!)  diagonal  scaling is  faster,  with its very
!    fast    set up  time    and  vectorized  (with   long    vectors)
!    preconditioning step (even though  it  may take more iterations).
!    If you have several systems (or  right hand sides) to  solve that
!    can  utilize  the  same  preconditioner  then the   cost   of the
!    incomplete factorization can   be  amortized over these  several
!    solutions.  This situation gives more advantage to the incomplete
!    factorization methods.  If  you have  a  vector  machine  without
!    hardware  gather/scatter (Cray  1,  Cray  2  &  Cray 3) then  the
!    advantages for incomplete factorization are even less.
!
!    If you're trying to shoehorn SLAP into your  favorite "production
!    code" and can not easily generate either the SLAP Triad or Column
!    format  then  you are  left  to   your  own  devices in terms  of
!    preconditioning.  Also,  you may  find that the   preconditioners
!    supplied with SLAP are not sufficient  for your problem.  In this
!    situation we would  recommend  that you   talk  with a  numerical
!    analyst  versed in   iterative   methods   about   writing  other
!    preconditioning  subroutines (e.g.,  polynomial  preconditioning,
!    shifted incomplete factorization,  SOR  or SSOR  iteration).  You
!    can always "roll your own"  by using the "core" iterative methods
!    and supplying your own MSOLVE and MATVEC (and possibly MTSOLV and
!    MTTVEC) routines.  If you do develop a new preconditioner for the
!    SLAP data structure send the code to us  (if you can do that with
!    no strings attached!, i.e.  copyright restrictions) and we'll add
!    it to the package!
!
!                          SYMMETRIC SYSTEMS
!    If your matrix is symmetric then you would want to use one of the
!    symmetric system  solvers.    If  your  system  is  also positive
!    definite,   (Ax,x) (Ax dot  product  with x) is  positive for all
!    non-zero  vectors x,  then use   Conjugate Gradient (SCG,  SSDCG,
!    SSICSG).  If you're  not sure it's SPD   (symmetric and  Positive
!    Definite)  then try SCG anyway and  if it works, fine.  If you're
!    sure your matrix is not  positive definite  then you  may want to
!    try the iterative refinement   methods  (SIR)  or the  GMRES code
!    (SGMRES) if SIR converges too slowly.
!
!                         NONSYMMETRIC SYSTEMS
!    This   is currently  an  area  of  active research  in  numerical
!    analysis  and   there   are   new  strategies  being   developed.
!    Consequently take the following advice with a grain of salt.   If
!    you matrix is positive definite, (Ax,x)  (Ax  dot product  with x
!    is positive for all non-zero  vectors x), then you can use any of
!    the    methods   for   nonsymmetric   systems (Orthomin,   GMRES,
!    BiConjugate Gradient, BiConjugate Gradient  Squared and Conjugate
!    Gradient applied to the normal equations).  If your system is not
!    too ill conditioned then try  BiConjugate Gradient Squared (BCGS)
!    or GMRES (SGMRES).  Both  of  these methods converge very quickly
!    and do  not require A'  or M' ('  denotes transpose) information.
!    SGMRES  does require  some  additional storage,  though.  If  the
!    system is very  ill conditioned  or   nearly positive  indefinite
!    ((Ax,x) is positive,  but may be  very small),  then GMRES should
!    be the first choice,  but try the  other  methods  if you have to
!    fine tune  the solution process for a  "production code".  If you
!    have a great preconditioner for the normal  equations (i.e., M is
!    an approximation to the inverse of AA' rather than  just  A) then
!    this is not a bad route to travel.  Old wisdom would say that the
!    normal equations are a disaster  (since it squares the  condition
!    number of the system and SCG convergence is linked to this number
!    of    infamy), but   some     preconditioners    (like incomplete
!    factorization) can reduce the condition number back below that of
!    the original system.
!***REFERENCES  1. M. Seager, ``A SLAP for the Masses,'' Lawrence
!                 Livermore Nat. Laboratory Technical Report,
!                 UCRL-100267, December 1988.
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DLPDOC
!
!     This is a *DUMMY* subroutine and should be called only for
!     the futility of it all.  Aren't *SLATEC* conventions FUN!!!
!
!***FIRST EXECUTABLE STATEMENT  DLPDOC
      RETURN
!------------- LAST LINE OF DLPDOC FOLLOWS -----------------------------
      END
function d1mach ( i )

!*****************************************************************************80
!
!! D1MACH returns double precision real machine constants.
!
!  Discussion:
!
!    Assuming that the internal representation of a double precision real
!    number is in base B, with T the number of base-B digits in the mantissa,
!    and EMIN the smallest possible exponent and EMAX the largest possible
!    exponent, then
!
!      D1MACH(1) = B**(EMIN-1), the smallest positive magnitude.
!      D1MACH(2) = B**EMAX*(1-B**(-T)), the largest magnitude.
!      D1MACH(3) = B**(-T), the smallest relative spacing.
!      D1MACH(4) = B**(1-T), the largest relative spacing.
!      D1MACH(5) = log10(B).
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    Phyllis Fox, Andrew Hall, Norman Schryer
!
!  Reference:
!
!    Phyllis Fox, Andrew Hall, Norman Schryer,
!    Algorithm 528:
!    Framework for a Portable Library,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978, page 176-188.
!
!  Parameters:
!
!    Input, integer I, chooses the parameter to be returned.
!    1 <= I <= 5.
!
!    Output, real ( kind = 8 ) D1MACH, the value of the chosen parameter.
!
  implicit none

  real ( kind = 8 ) d1mach
  integer i

  if ( i < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'D1MACH - Fatal error!'
    write ( *, '(a)' ) '  The input argument I is out of bounds.'
    write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 5.'
    write ( *, '(a,i12)' ) '  I = ', i
    d1mach = 0.0D+00
    stop
  else if ( i == 1 ) then
    d1mach = 4.450147717014403D-308
  else if ( i == 2 ) then
    d1mach = 8.988465674311579D+307
  else if ( i == 3 ) then
    d1mach = 1.110223024625157D-016
  else if ( i == 4 ) then
    d1mach = 2.220446049250313D-016
  else if ( i == 5 ) then
    d1mach = 0.301029995663981D+000
  else if ( 5 < i ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'D1MACH - Fatal error!'
    write ( *, '(a)' ) '  The input argument I is out of bounds.'
    write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 5.'
    write ( *, '(a,i12)' ) '  I = ', i
    d1mach = 0.0D+00
    stop
  end if

  return
end
function i1mach ( i )

!*****************************************************************************80
!
!! I1MACH returns integer machine constants.
!
!  Discussion:
!
!    Input/output unit numbers.
!
!      I1MACH(1) = the standard input unit.
!      I1MACH(2) = the standard output unit.
!      I1MACH(3) = the standard punch unit.
!      I1MACH(4) = the standard error message unit.
!
!    Words.
!
!      I1MACH(5) = the number of bits per integer storage unit.
!      I1MACH(6) = the number of characters per integer storage unit.
!
!    Integers.
!
!    Assume integers are represented in the S digit base A form:
!
!      Sign * (X(S-1)*A**(S-1) + ... + X(1)*A + X(0))
!
!    where 0 <= X(1:S-1) < A.
!
!      I1MACH(7) = A, the base.
!      I1MACH(8) = S, the number of base A digits.
!      I1MACH(9) = A**S-1, the largest integer.
!
!    Floating point numbers
!
!    Assume floating point numbers are represented in the T digit
!    base B form:
!
!      Sign * (B**E) * ((X(1)/B) + ... + (X(T)/B**T) )
!
!    where 0 <= X(I) < B for I=1 to T, 0 < X(1) and EMIN <= E <= EMAX.
!
!      I1MACH(10) = B, the base.
!
!    Single precision
!
!      I1MACH(11) = T, the number of base B digits.
!      I1MACH(12) = EMIN, the smallest exponent E.
!      I1MACH(13) = EMAX, the largest exponent E.
!
!    Double precision
!
!      I1MACH(14) = T, the number of base B digits.
!      I1MACH(15) = EMIN, the smallest exponent E.
!      I1MACH(16) = EMAX, the largest exponent E.
!
!    To alter this function for a particular environment, the desired set of
!    statements should be activated.
!
!    On rare machines, a STATIC statement may need to be added, but
!    probably more systems prohibit than require it.
!
!    Also, the values of I1MACH(1) through I1MACH(4) should be
!    checked for consistency with the local operating system.
!
!    For IEEE-arithmetic machines (binary standard), the first set of
!    constants below should be appropriate, except perhaps for
!    IMACH(1) - IMACH(4).
!
!  Modified:
!
!    18 April 2004
!
!  Reference:
!
!    P A Fox, A D Hall, N L Schryer,
!    Algorithm 528,
!    Framework for a Portable Library,
!    ACM Transactions on Mathematical Software,
!    Volume 4, number 2, page 176-188.
!
!  Parameters:
!
!    Input, integer I, chooses the parameter to be returned.
!    1 <= I <= 16.
!
!    Output, integer I1MACH, the value of the chosen parameter.
!
  implicit none
!
  integer i
  integer i1mach
!
!  All IEEE machines:
!
!    AT&T 3B series
!    AT&T PC 6300
!    AT&T PC 7300
!    DEC/COMPAQ/HP ALPHA
!    DEC PMAX
!    HP RISC-chip machines
!    IBM PC
!    IBM RISC-chip machines
!    Intel 80x87 machines
!    Motorola 68000 machines
!    NEXT
!    SGI Iris
!    SPARC RISC-chip machines
!    SUN 3
!
  integer, parameter, dimension ( 16 ) :: imach = (/ &
    5, 6, 7, 6, 32, 4, 2, 31, 2147483647, 2, &
    24, -125, 128, 53, -1021, 1024 /)
!
!  ALLIANT FX/8 UNIX FORTRAN compiler:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 6, 0, 32, 4, 2, 32, 2147483647, 2, &
!    24, -126, 128, 53, -1022, 1024 /)
!
!  AMDAHL machines:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 7, 6, 32, 4, 2, 31, 2147483647, 16, &
!    6, -64, 63, 14, -64, 63 /)
!
!  BURROUGHS 1700 system:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    7, 2, 2, 2, 36, 4, 2, 33, Z1FFFFFFFF, 2, &
!    24, -256, 255, 60, -256, 255 /)
!
!  BURROUGHS 5700 system:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 7, 6, 48, 6, 2, 39, O0007777777777777, 8, &
!    13, -50, 76, 26, -50, 76 /)
!
!  BURROUGHS 6700/7700 systems:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 7, 6, 48, 6, 2, 39, O0007777777777777, 8, &
!    13, -50, 76, 26, -32754, 32780 /)
!
!  CDC CYBER 170/180 series using NOS:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 7, 6, 60, 10, 2, 48, O"00007777777777777777", 2, &
!    48, -974, 1070, 96, -927, 1070 /)
!
!  CDC CYBER 170/180 series using NOS/VE:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 7, 6, 64, 8, 2, 63, 9223372036854775807, 2, &
!    47, -4095, 4094, 94, -4095, 4094 /)
!
!  CDC CYBER 200 series:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 7, 6, 64, 8, 2, 47, X'00007FFFFFFFFFFF', 2, &
!    47, -28625, 28718, 94, -28625, 28718 /)
!
!  CDC 6000/7000 series using FTN4:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 7, 6, 60, 10, 2, 48, 00007777777777777777B, 2, &
!    47, -929, 1070, 94, -929, 1069 /)
!
!  CDC 6000/7000 series using FTN5:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 7, 6, 60, 10, 2, 48, O"00007777777777777777", 2, &
!    47, -929, 1070, 94, -929, 1069 /)
!
!  CONVEX C-1:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 7, 6, 32, 4, 2, 31, 2147483647, 2, &
!    24, -128, 127, 53, -1024, 1023 /)
!
!  CONVEX C-120 (native mode) without -R8 option:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 0, 6, 32, 4, 2, 31, 2147483647, 2, &
!    24, -127, 127, 53, -1023, 1023 /)
!
!  CONVEX C-120 (native mode) with -R8 option:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 0, 6, 32, 4, 2, 31, 2147483647, 2, &
!    53, -1023, 1023, 53, -1023, 1023 /)
!
!  CONVEX C-120 (IEEE mode) without -R8 option:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 0, 6, 32, 4, 2, 31, 2147483647, 2,
!    24, -125, 128, 53, -1021, 1024 /)
!
!  CONVEX C-120 (IEEE mode) with -R8 option:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 0, 6, 32, 4, 2, 31, 2147483647, 2,
!    53, -1021, 1024, 53, -1021, 1024 /)
!
!  CRAY 1, 2, 3, XMP, YMP, and C90:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 102, 6, 64, 8, 2, 63, 777777777777777777777B, 2, &
!    47, -8189, 8190, 94, -8099, 8190 /)
!
!  DATA GENERAL ECLIPSE S/200:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    11, 12, 8, 10, 16, 2, 2, 15, 32767, 16, &
!    6, -64, 63, 14, -64, 63 /)
!
!  ELXSI 6400:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 6, 6, 32, 4, 2, 32, 2147483647, 2, &
!    24, -126, 127, 53, -1022, 1023 /)
!
!  HARRIS 220:
!
!      data imach( 1) /       5 /
!      data imach( 2) /       6 /
!      data imach( 3) /       0 /
!      data imach( 4) /       6 /
!      data imach( 5) /      24 /
!      data imach( 6) /       3 /
!      data imach( 7) /       2 /
!      data imach( 8) /      23 /
!      data imach( 9) / 8388607 /
!      data imach(10) /       2 /
!      data imach(11) /      23 /
!      data imach(12) /    -127 /
!      data imach(13) /     127 /
!      data imach(14) /      38 /
!      data imach(15) /    -127 /
!      data imach(16) /     127 /
!
!  HARRIS SLASH 6 and SLASH 7:
!
!      data imach( 1) /       5 /
!      data imach( 2) /       6 /
!      data imach( 3) /       0 /
!      data imach( 4) /       6 /
!      data imach( 5) /      24 /
!      data imach( 6) /       3 /
!      data imach( 7) /       2 /
!      data imach( 8) /      23 /
!      data imach( 9) / 8388607 /
!      data imach(10) /       2 /
!      data imach(11) /      23 /
!      data imach(12) /    -127 /
!      data imach(13) /     127 /
!      data imach(14) /      38 /
!      data imach(15) /    -127 /
!      data imach(16) /     127 /
!
!  HONEYWELL DPS 8/70 and 600/6000 series:
!
!      data imach( 1) /    5 /
!      data imach( 2) /    6 /
!      data imach( 3) /   43 /
!      data imach( 4) /    6 /
!      data imach( 5) /   36 /
!      data imach( 6) /    4 /
!      data imach( 7) /    2 /
!      data imach( 8) /   35 /
!      data imach( 9) / O377777777777 /
!      data imach(10) /    2 /
!      data imach(11) /   27 /
!      data imach(12) / -127 /
!      data imach(13) /  127 /
!      data imach(14) /   63 /
!      data imach(15) / -127 /
!      data imach(16) /  127 /
!
!  HP 2100, 3 word double precision option with FTN4:
!
!      data imach( 1) /    5 /
!      data imach( 2) /    6 /
!      data imach( 3) /    4 /
!      data imach( 4) /    1 /
!      data imach( 5) /   16 /
!      data imach( 6) /    2 /
!      data imach( 7) /    2 /
!      data imach( 8) /   15 /
!      data imach( 9) / 32767 /
!      data imach(10) /    2 /
!      data imach(11) /   23 /
!      data imach(12) / -128 /
!      data imach(13) /  127 /
!      data imach(14) /   39 /
!      data imach(15) / -128 /
!      data imach(16) /  127 /
!
!  HP 2100, 4 word double precision option with FTN4:
!
!      data imach( 1) /    5 /
!      data imach( 2) /    6 /
!      data imach( 3) /    4 /
!      data imach( 4) /    1 /
!      data imach( 5) /   16 /
!      data imach( 6) /    2 /
!      data imach( 7) /    2 /
!      data imach( 8) /   15 /
!      data imach( 9) / 32767 /
!      data imach(10) /    2 /
!      data imach(11) /   23 /
!      data imach(12) / -128 /
!      data imach(13) /  127 /
!      data imach(14) /   55 /
!      data imach(15) / -128 /
!      data imach(16) /  127 /
!
!  HP 9000:
!
!      data imach( 1) /     5 /
!      data imach( 2) /     6 /
!      data imach( 3) /     6 /
!      data imach( 4) /     7 /
!      data imach( 5) /    32 /
!      data imach( 6) /     4 /
!      data imach( 7) /     2 /
!      data imach( 8) /    32 /
!      data imach( 9) / 2147483647 /
!      data imach(10) /     2 /
!      data imach(11) /    24 /
!      data imach(12) /  -126 /
!      data imach(13) /   127 /
!      data imach(14) /    53 /
!      data imach(15) / -1015 /
!      data imach(16) /  1017 /
!
!  IBM 360/370 series,
!  XEROX SIGMA 5/7/9,
!  SEL systems 85/86,
!  PERKIN ELMER 3230,
!  PERKIN ELMER (INTERDATA) 3230:
!
!      data imach( 1) /   5 /
!      data imach( 2) /   6 /
!      data imach( 3) /   7 /
!      data imach( 4) /   6 /
!      data imach( 5) /  32 /
!      data imach( 6) /   4 /
!      data imach( 7) /   2 /
!      data imach( 8) /  31 /
!      data imach( 9) / Z7FFFFFFF /
!      data imach(10) /  16 /
!      data imach(11) /   6 /
!      data imach(12) / -64 /
!      data imach(13) /  63 /
!      data imach(14) /  14 /
!      data imach(15) / -64 /
!      data imach(16) /  63 /
!
!  IBM PC - Microsoft FORTRAN:
!
!      data imach( 1) /     5 /
!      data imach( 2) /     6 /
!      data imach( 3) /     6 /
!      data imach( 4) /     0 /
!      data imach( 5) /    32 /
!      data imach( 6) /     4 /
!      data imach( 7) /     2 /
!      data imach( 8) /    31 /
!      data imach( 9) / 2147483647 /
!      data imach(10) /     2 /
!      data imach(11) /    24 /
!      data imach(12) /  -126 /
!      data imach(13) /   127 /
!      data imach(14) /    53 /
!      data imach(15) / -1022 /
!      data imach(16) /  1023 /
!
!  IBM PC - Professional FORTRAN and Lahey FORTRAN:
!
!      data imach( 1) /     4 /
!      data imach( 2) /     7 /
!      data imach( 3) /     7 /
!      data imach( 4) /     0 /
!      data imach( 5) /    32 /
!      data imach( 6) /     4 /
!      data imach( 7) /     2 /
!      data imach( 8) /    31 /
!      data imach( 9) / 2147483647 /
!      data imach(10) /     2 /
!      data imach(11) /    24 /
!      data imach(12) /  -126 /
!      data imach(13) /   127 /
!      data imach(14) /    53 /
!      data imach(15) / -1022 /
!      data imach(16) /  1023 /
!
!  INTERDATA 8/32 with the UNIX system FORTRAN 77 compiler:
!
!  For the INTERDATA FORTRAN VII compiler, replace the Z's
!  specifying hex constants with Y's.
!
!      data imach( 1) /   5 /
!      data imach( 2) /   6 /
!      data imach( 3) /   6 /
!      data imach( 4) /   6 /
!      data imach( 5) /  32 /
!      data imach( 6) /   4 /
!      data imach( 7) /   2 /
!      data imach( 8) /  31 /
!      data imach( 9) / Z'7FFFFFFF' /
!      data imach(10) /  16 /
!      data imach(11) /   6 /
!      data imach(12) / -64 /
!      data imach(13) /  62 /
!      data imach(14) /  14 /
!      data imach(15) / -64 /
!      data imach(16) /  62 /
!
!  PDP-10 (KA processor):
!
!      data imach( 1) /    5 /
!      data imach( 2) /    6 /
!      data imach( 3) /    7 /
!      data imach( 4) /    6 /
!      data imach( 5) /   36 /
!      data imach( 6) /    5 /
!      data imach( 7) /    2 /
!      data imach( 8) /   35 /
!      data imach( 9) / "377777777777 /
!      data imach(10) /    2 /
!      data imach(11) /   27 /
!      data imach(12) / -128 /
!      data imach(13) /  127 /
!      data imach(14) /   54 /
!      data imach(15) / -101 /
!      data imach(16) /  127 /
!
!  PDP-10 (KI processor):
!
!      data imach( 1) /    5 /
!      data imach( 2) /    6 /
!      data imach( 3) /    7 /
!      data imach( 4) /    6 /
!      data imach( 5) /   36 /
!      data imach( 6) /    5 /
!      data imach( 7) /    2 /
!      data imach( 8) /   35 /
!      data imach( 9) / "377777777777 /
!      data imach(10) /    2 /
!      data imach(11) /   27 /
!      data imach(12) / -128 /
!      data imach(13) /  127 /
!      data imach(14) /   62 /
!      data imach(15) / -128 /
!      data imach(16) /  127 /
!
!  PDP-11 FORTRAN supporting 32-bit integer arithmetic:
!
!      data imach( 1) /    5 /
!      data imach( 2) /    6 /
!      data imach( 3) /    7 /
!      data imach( 4) /    6 /
!      data imach( 5) /   32 /
!      data imach( 6) /    4 /
!      data imach( 7) /    2 /
!      data imach( 8) /   31 /
!      data imach( 9) / 2147483647 /
!      data imach(10) /    2 /
!      data imach(11) /   24 /
!      data imach(12) / -127 /
!      data imach(13) /  127 /
!      data imach(14) /   56 /
!      data imach(15) / -127 /
!      data imach(16) /  127 /
!
!  PDP-11 FORTRAN supporting 16-bit integer arithmetic:
!
!      data imach( 1) /    5 /
!      data imach( 2) /    6 /
!      data imach( 3) /    7 /
!      data imach( 4) /    6 /
!      data imach( 5) /   16 /
!      data imach( 6) /    2 /
!      data imach( 7) /    2 /
!      data imach( 8) /   15 /
!      data imach( 9) / 32767 /
!      data imach(10) /    2 /
!      data imach(11) /   24 /
!      data imach(12) / -127 /
!      data imach(13) /  127 /
!      data imach(14) /   56 /
!      data imach(15) / -127 /
!      data imach(16) /  127 /
!
!  PRIME 50 series systems with 32-bit integers and 64V MODE instructions:
!
!      data imach( 1) /            1 /
!      data imach( 2) /            1 /
!      data imach( 3) /            2 /
!      data imach( 4) /            1 /
!      data imach( 5) /           32 /
!      data imach( 6) /            4 /
!      data imach( 7) /            2 /
!      data imach( 8) /           31 /
!      data imach( 9) / :17777777777 /
!      data imach(10) /            2 /
!      data imach(11) /           23 /
!      data imach(12) /         -127 /
!      data imach(13) /         +127 /
!      data imach(14) /           47 /
!      data imach(15) /       -32895 /
!      data imach(16) /       +32637 /
!
!  SEQUENT BALANCE 8000:
!
!      data imach( 1) /     0 /
!      data imach( 2) /     0 /
!      data imach( 3) /     7 /
!      data imach( 4) /     0 /
!      data imach( 5) /    32 /
!      data imach( 6) /     1 /
!      data imach( 7) /     2 /
!      data imach( 8) /    31 /
!      data imach( 9) /  2147483647 /
!      data imach(10) /     2 /
!      data imach(11) /    24 /
!      data imach(12) /  -125 /
!      data imach(13) /   128 /
!      data imach(14) /    53 /
!      data imach(15) / -1021 /
!      data imach(16) /  1024 /
!
!  SUN Microsystems UNIX F77 compiler:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 6, 0, 32, 4, 2, 32, 2147483647, 2, &
!    24, -126, 128, 53, -1022, 1024 /)
!
!  SUN 3 (68881 or FPA):
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 6, 0, 32, 4, 2, 31, 2147483647, 2, &
!    24, -125, 128, 53, -1021, 1024 /)
!
!  UNIVAC 1100 series:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 7, 6, 36, 6, 2, 35, O377777777777, 2, &
!    27, -128, 127, 60, -1024, 1023 /)
!
!  VAX:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    5, 6, 7, 6, 32, 4, 2, 31, 2147483647, 2, &
!    24, -127, 127, 56, -127, 127 /)
!
!  Z80 microprocessor:
!
!  integer, parameter, dimension ( 16 ) :: imach = (/ &
!    1, 1, 0, 1, 16, 2, 2, 15, 32767, 2, &
!    24, -127, 127, 56, -127, 127 /)
!
  if ( i < 1 .or. 16 < i ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I1MACH - Fatal error!'
    write ( *, '(a)' ) '  The input argument I is out of bounds.'
    write ( *, '(a)' ) '  Legal values satisfy 1 <= I <= 16.'
    write ( *, '(a,i12)' ) '  I = ', i
    i1mach = 0
    stop
  else
    i1mach = imach(i)
  end if

  return
end
subroutine xerabt(messg,nmessg)
!VD$G NOVECTOR
!VD$G NOCONCUR
!***begin prologue  xerabt
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3c
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  abort program execution and print error message.
!***description
!
!     abstract
!        ***note*** machine dependent routine
!        xerabt aborts the execution of the program.
!        the error message causing the abort is given in the calling
!        sequence, in case one needs it for printing on a dayfile,
!        for example.
!
!     description of parameters
!        messg and nmessg are as in xerror, except that nmessg may
!        be zero, in which case no message is being supplied.
!
!     written by ron jones, with slatec common math library subcommittee
!     latest revision ---  1 august 1982
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  (none)
!***end prologue  xerabt
      character ( len = * ) messg

!***first executable statement  xerabt

      call exit(1)

      end
      subroutine xerctl(messg1,nmessg,nerr,level,kontrl)
!***begin prologue  xerctl
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3c
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  allow user control over handling of errors.
!***description
!
!     abstract
!        allows user control over handling of individual errors.
!        just after each message is recorded, but before it is
!        processed any further (i.e., before it is printed or
!        a decision to abort is made), a call is made to xerctl.
!        if the user has provided his own version of xerctl, he
!        can then override the value of kontrol used in processing
!        this message by redefining its value.
!        kontrl may be set to any value from -2 to 2.
!        the meanings for kontrl are the same as in xsetf, except
!        that the value of kontrl changes only for this message.
!        if kontrl is set to a value outside the range from -2 to 2,
!        it will be moved back into that range.
!
!     description of parameters
!
!      --input--
!        messg1 - the first word (only) of the error message.
!        nmessg - same as in the call to xerror or xerrwv.
!        nerr   - same as in the call to xerror or xerrwv.
!        level  - same as in the call to xerror or xerrwv.
!        kontrl - the current value of the control flag as set
!                 by a call to xsetf.
!
!      --output--
!        kontrl - the new value of kontrl.  if kontrl is not
!                 defined, it will remain at its original value.
!                 this changed value of control affects only
!                 the current occurrence of the current message.
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  (none)
!***end prologue  xerctl
      character*20 messg1
!***first executable statement  xerctl
      return
      end
      subroutine xerprt(messg,nmessg)
!***begin prologue  xerprt
!***date written   790801   (yymmdd)
!***revision date  851213   (yymmdd)
!***category no.  r3
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  print error messages.
!***description
!
!     abstract
!        print the hollerith message in messg, of length nmessg,
!        on each file indicated by xgetua.
!     latest revision ---  1 august 1985
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  i1mach,xgetua
!***end prologue  xerprt
      integer lun(5)
      character*(*) messg
!     obtain unit numbers and write line to each unit
!***first executable statement  xerprt
      call xgetua(lun,nunit)
      lenmes = len(messg)
      do 20 kunit=1,nunit
         iunit = lun(kunit)
         if (iunit.eq.0) iunit = i1mach(4)
         do ichar=1,lenmes,72
            last = min0(ichar+71 , lenmes)
            write (iunit,'(1x,a)') messg(ichar:last)
         end do
   20 continue
      return
      end
      subroutine xerror(messg,nmessg,nerr,level)
!***begin prologue  xerror
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3c
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  process an error (diagnostic) message.
!***description
!
!     abstract
!        xerror processes a diagnostic message, in a manner
!        determined by the value of level and the current value
!        of the library error control flag, kontrl.
!        (see subroutine xsetf for details.)
!
!     description of parameters
!      --input--
!        messg - the hollerith message to be processed, containing
!                no more than 72 characters.
!        nmessg- the actual number of characters in messg.
!        nerr  - the error number associated with this message.
!                nerr must not be zero.
!        level - error category.
!                =2 means this is an unconditionally fatal error.
!                =1 means this is a recoverable error.  (i.e., it is
!                   non-fatal if xsetf has been appropriately called.)
!                =0 means this is a warning message only.
!                =-1 means this is a warning message which is to be
!                   printed at most once, regardless of how many
!                   times this call is executed.
!
!     examples
!        call xerror('smooth -- num was zero.',23,1,2)
!        call xerror('integ  -- less than full accuracy achieved.',
!    1                43,2,1)
!        call xerror('rooter -- actual zero of f found before interval f
!    1ully collapsed.',65,3,0)
!        call xerror('exp    -- underflows being set to zero.',39,1,-1)
!
!     written by ron jones, with slatec common math library subcommittee
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  xerrwv
!***end prologue  xerror
      character*(*) messg
!***first executable statement  xerror
      call xerrwv(messg,nmessg,nerr,level,0,0,0,0,0.,0.)
      return
      end
      subroutine xerrwv(messg,nmessg,nerr,level,ni,i1,i2,nr,r1,r2)
!***begin prologue  xerrwv
!***date written   800319   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3c
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  process an error message allowing 2 integer and 2 real
!            values to be included in the message.
!***description
!
!     abstract
!        xerrwv processes a diagnostic message, in a manner
!        determined by the value of level and the current value
!        of the library error control flag, kontrl.
!        (see subroutine xsetf for details.)
!        in addition, up to two integer values and two real
!        values may be printed along with the message.
!
!     description of parameters
!      --input--
!        messg - the hollerith message to be processed.
!        nmessg- the actual number of characters in messg.
!        nerr  - the error number associated with this message.
!                nerr must not be zero.
!        level - error category.
!                =2 means this is an unconditionally fatal error.
!                =1 means this is a recoverable error.  (i.e., it is
!                   non-fatal if xsetf has been appropriately called.)
!                =0 means this is a warning message only.
!                =-1 means this is a warning message which is to be
!                   printed at most once, regardless of how many
!                   times this call is executed.
!        ni    - number of integer values to be printed. (0 to 2)
!        i1    - first integer value.
!        i2    - second integer value.
!        nr    - number of real values to be printed. (0 to 2)
!        r1    - first real value.
!        r2    - second real value.
!
!     examples
!        call xerrwv('smooth -- num (=i1) was zero.',29,1,2,
!    1   1,num,0,0,0.,0.)
!        call xerrwv('quadxy -- requested error (r1) less than minimum (
!    1r2).,54,77,1,0,0,0,2,errreq,errmin)
!
!     latest revision ---  1 august 1985
!     written by ron jones, with slatec common math library subcommittee
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  fdump,i1mach,j4save,xerabt,xerctl,xerprt,xersav,
!                    xgetua
!***end prologue  xerrwv
      character*(*) messg
      character*20 lfirst
      character*37 form
      dimension lun(5)
!     get flags
!***first executable statement  xerrwv
      lkntrl = j4save(2,0,.false.)
      maxmes = j4save(4,0,.false.)
!     check for valid input
      if ((nmessg.gt.0).and.(nerr.ne.0).and. &
          (level.ge.(-1)).and.(level.le.2)) go to 10
         if (lkntrl.gt.0) call xerprt('fatal error in...',17)
         call xerprt('xerror -- invalid input',23)
!        if (lkntrl.gt.0) call fdump
         if (lkntrl.gt.0) call xerprt('job abort due to fatal error.', &
        29)
         if (lkntrl.gt.0) call xersav(' ',0,0,0,kdummy)
         call xerabt('xerror -- invalid input',23)
         return
   10 continue
!     record message
      junk = j4save(1,nerr,.true.)
      call xersav(messg,nmessg,nerr,level,kount)
!     let user override
      lfirst = messg
      lmessg = nmessg
      lerr = nerr
      llevel = level
      call xerctl(lfirst,lmessg,lerr,llevel,lkntrl)
!     reset to original values
      lmessg = nmessg
      lerr = nerr
      llevel = level
      lkntrl = max0(-2,min0(2,lkntrl))
      mkntrl = iabs(lkntrl)
!     decide whether to print message
      if ((llevel.lt.2).and.(lkntrl.eq.0)) go to 100
      if (((llevel.eq.(-1)).and.(kount.gt.min0(1,maxmes))) &
      .or.((llevel.eq.0)   .and.(kount.gt.maxmes)) &
      .or.((llevel.eq.1)   .and.(kount.gt.maxmes).and.(mkntrl.eq.1)) &
      .or.((llevel.eq.2)   .and.(kount.gt.max0(1,maxmes)))) go to 100
         if (lkntrl.le.0) go to 20
            call xerprt(' ',1)
!           introduction
            if (llevel.eq.(-1)) call xerprt &
      ('warning message...this message will only be printed once.',57)
            if (llevel.eq.0) call xerprt('warning in...',13)
            if (llevel.eq.1) call xerprt &
            ('recoverable error in...',23)
            if (llevel.eq.2) call xerprt('fatal error in...',17)
   20    continue
!        message
         call xerprt(messg,lmessg)
         call xgetua(lun,nunit)
         isizei = log10(float(i1mach(9))) + 1.0
         isizef = log10(float(i1mach(10))**i1mach(11)) + 1.0
         do 50 kunit=1,nunit
            iunit = lun(kunit)
            if (iunit.eq.0) iunit = i1mach(4)
            do 22 i=1,min(ni,2)
               write (form,21) i,isizei
   21          format ('(11x,21hin above message, i',i1,'=,i',i2,')   ')
               if (i.eq.1) write (iunit,form) i1
               if (i.eq.2) write (iunit,form) i2
   22       continue
            do 24 i=1,min(nr,2)
               write (form,23) i,isizef+10,isizef
   23          format ('(11x,21hin above message, r',i1,'=,e', &
               i2,'.',i2,')')
               if (i.eq.1) write (iunit,form) r1
               if (i.eq.2) write (iunit,form) r2
   24       continue
            if (lkntrl.le.0) go to 40
!              error number
               write (iunit,30) lerr
   30          format (15h error number =,i10)
   40       continue
   50    continue
!        trace-back
!        if (lkntrl.gt.0) call fdump
  100 continue
      ifatal = 0
      if ((llevel.eq.2).or.((llevel.eq.1).and.(mkntrl.eq.2))) &
      ifatal = 1
!     quit here if message is not fatal
      if (ifatal.le.0) return
      if ((lkntrl.le.0).or.(kount.gt.max0(1,maxmes))) go to 120
!        print reason for abort
         if (llevel.eq.1) call xerprt &
         ('job abort due to unrecovered error.',35)
         if (llevel.eq.2) call xerprt &
         ('job abort due to fatal error.',29)
!        print error summary
         call xersav(' ',-1,0,0,kdummy)
  120 continue
!     abort
      if ((llevel.eq.2).and.(kount.gt.max0(1,maxmes))) lmessg = 0
      call xerabt(messg,lmessg)
      return
      end
      subroutine xersav(messg,nmessg,nerr,level,icount)
!***begin prologue  xersav
!***date written   800319   (yymmdd)
!***revision date  851213   (yymmdd)
!***category no.  r3
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  record that an error has occurred.
!***description
!
!     abstract
!        record that this error occurred.
!
!     description of parameters
!     --input--
!       messg, nmessg, nerr, level are as in xerror,
!       except that when nmessg=0 the tables will be
!       dumped and cleared, and when nmessg is less than zero the
!       tables will be dumped and not cleared.
!     --output--
!       icount will be the number of times this message has
!       been seen, or zero if the table has overflowed and
!       does not contain this message specifically.
!       when nmessg=0, icount will not be altered.
!
!     written by ron jones, with slatec common math library subcommittee
!     latest revision ---  1 august 1985
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  i1mach,xgetua
!***end prologue  xersav
      integer lun(5)
      character*(*) messg
      character*20 mestab(10),mes
      dimension nertab(10),levtab(10),kount(10)
      save mestab,nertab,levtab,kount,kountx
!     next two data statements are necessary to provide a blank
!     error table initially
      data kount(1),kount(2),kount(3),kount(4),kount(5), &
           kount(6),kount(7),kount(8),kount(9),kount(10) &
           /0,0,0,0,0,0,0,0,0,0/
      data kountx/0/
!***first executable statement  xersav
      if (nmessg.gt.0) go to 80
!     dump the table
         if (kount(1).eq.0) return
!        print to each unit
         call xgetua(lun,nunit)
         do 60 kunit=1,nunit
            iunit = lun(kunit)
            if (iunit.eq.0) iunit = i1mach(4)
!           print table header
            write (iunit,10)
   10       format (32h0          error message summary/ &
            51h message start             nerr     level     count)
!           print body of table
            do 20 i=1,10
               if (kount(i).eq.0) go to 30
               write (iunit,15) mestab(i),nertab(i),levtab(i),kount(i)
   15          format (1x,a20,3i10)
   20       continue
   30       continue
!           print number of other errors
            if (kountx.ne.0) write (iunit,40) kountx
   40       format (41h0other errors not individually tabulated=,i10)
            write (iunit,50)
   50       format (1x)
   60    continue
         if (nmessg.lt.0) return
!        clear the error tables
         do 70 i=1,10
   70       kount(i) = 0
         kountx = 0
         return
   80 continue
!     process a message...
!     search for this messg, or else an empty slot for this messg,
!     or else determine that the error table is full.
      mes = messg
      do 90 i=1,10
         ii = i
         if (kount(i).eq.0) go to 110
         if (mes.ne.mestab(i)) go to 90
         if (nerr.ne.nertab(i)) go to 90
         if (level.ne.levtab(i)) go to 90
         go to 100
   90 continue
!     three possible cases...
!     table is full
         kountx = kountx+1
         icount = 1
         return
!     message found in table
  100    kount(ii) = kount(ii) + 1
         icount = kount(ii)
         return
!     empty slot found for new message
  110    mestab(ii) = mes
         nertab(ii) = nerr
         levtab(ii) = level
         kount(ii)  = 1
         icount = 1
         return
      end
      subroutine xgetf(kontrl)
!***begin prologue  xgetf
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3c
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  return the current value of the error control flag.
!***description
!
!   abstract
!        xgetf returns the current value of the error control flag
!        in kontrl.  see subroutine xsetf for flag value meanings.
!        (kontrl is an output parameter only.)
!
!     written by ron jones, with slatec common math library subcommittee
!     latest revision ---  7 june 1978
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  j4save
!***end prologue  xgetf
!***first executable statement  xgetf
      kontrl = j4save(2,0,.false.)
      return
      end
      subroutine xgetua(iunita,n)
!***begin prologue  xgetua
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3c
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  return unit number(s) to which error messages are being
!            sent.
!***description
!
!     abstract
!        xgetua may be called to determine the unit number or numbers
!        to which error messages are being sent.
!        these unit numbers may have been set by a call to xsetun,
!        or a call to xsetua, or may be a default value.
!
!     description of parameters
!      --output--
!        iunit - an array of one to five unit numbers, depending
!                on the value of n.  a value of zero refers to the
!                default unit, as defined by the i1mach machine
!                constant routine.  only iunit(1),...,iunit(n) are
!                defined by xgetua.  the values of iunit(n+1),...,
!                iunit(5) are not defined (for n .lt. 5) or altered
!                in any way by xgetua.
!        n     - the number of units to which copies of the
!                error messages are being sent.  n will be in the
!                range from 1 to 5.
!
!     latest revision ---  19 mar 1980
!     written by ron jones, with slatec common math library subcommittee
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  j4save
!***end prologue  xgetua
      dimension iunita(5)
!***first executable statement  xgetua
      n = j4save(5,0,.false.)
      do 30 i=1,n
         index = i+4
         if (i.eq.1) index = 3
         iunita(i) = j4save(index,0,.false.)
   30 continue
      return
      end
      function j4save(iwhich,ivalue,iset)
!***begin prologue  j4save
!***refer to  xerror
!***routines called  (none)
!***description
!
!     abstract
!        j4save saves and recalls several global variables needed
!        by the library error handling routines.
!
!     description of parameters
!      --input--
!        iwhich - index of item desired.
!                = 1 refers to current error number.
!                = 2 refers to current error control flag.
!                 = 3 refers to current unit number to which error
!                    messages are to be sent.  (0 means use standard.)
!                 = 4 refers to the maximum number of times any
!                     message is to be printed (as set by xermax).
!                 = 5 refers to the total number of units to which
!                     each error message is to be written.
!                 = 6 refers to the 2nd unit for error messages
!                 = 7 refers to the 3rd unit for error messages
!                 = 8 refers to the 4th unit for error messages
!                 = 9 refers to the 5th unit for error messages
!        ivalue - the value to be set for the iwhich-th parameter,
!                 if iset is .true. .
!        iset   - if iset=.true., the iwhich-th parameter will be
!                 given the value, ivalue.  if iset=.false., the
!                 iwhich-th parameter will be unchanged, and ivalue
!                 is a dummy parameter.
!      --output--
!        the (old) value of the iwhich-th parameter will be returned
!        in the function value, j4save.
!
!     written by ron jones, with slatec common math library subcommittee
!    adapted from bell laboratories port library error handler
!     latest revision ---  1 august 1985
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***end prologue  j4save
      logical iset
      integer iparam(9)
      save iparam
      data iparam(1),iparam(2),iparam(3),iparam(4)/0,2,0,10/
      data iparam(5)/1/
      data iparam(6),iparam(7),iparam(8),iparam(9)/0,0,0,0/
!***first executable statement  j4save
      j4save = iparam(iwhich)
      if (iset) iparam(iwhich) = ivalue
      return
      end
      subroutine xerclr
!***begin prologue  xerclr
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3c
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  reset current error number to zero.
!***description
!
!     abstract
!        this routine simply resets the current error number to zero.
!        this may be necessary to do in order to determine that
!        a certain error has occurred again since the last time
!        numxer was referenced.
!
!     written by ron jones, with slatec common math library subcommittee
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  j4save
!***end prologue  xerclr
!***first executable statement  xerclr
      junk = j4save(1,0,.true.)
      return
      end
      subroutine xerdmp
!***begin prologue  xerdmp
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3c
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  print the error tables and then clear them.
!***description
!
!     abstract
!        xerdmp prints the error tables, then clears them.
!
!     written by ron jones, with slatec common math library subcommittee
!     latest revision ---  7 june 1978
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  xersav
!***end prologue  xerdmp
!***first executable statement  xerdmp
      call xersav(' ',0,0,0,kount)
      return
      end
      subroutine xermax(max)
!***begin prologue  xermax
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3c
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  set maximum number of times any error message is to be
!            printed.
!***description
!
!     abstract
!        xermax sets the maximum number of times any message
!        is to be printed.  that is, non-fatal messages are
!        not to be printed after they have occured max times.
!        such non-fatal messages may be printed less than
!        max times even if they occur max times, if error
!        suppression mode (kontrl=0) is ever in effect.
!
!     description of parameter
!      --input--
!        max - the maximum number of times any one message
!              is to be printed.
!
!     written by ron jones, with slatec common math library subcommittee
!     latest revision ---  7 june 1978
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  j4save
!***end prologue  xermax
!***first executable statement  xermax
      junk = j4save(4,max,.true.)
      return
      end
      subroutine xgetun(iunit)
!***begin prologue  xgetun
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3c
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  return the (first) output file to which error messages
!            are being sent.
!***description
!
!     abstract
!        xgetun gets the (first) output file to which error messages
!        are being sent.  to find out if more than one file is being
!        used, one must use the xgetua routine.
!
!     description of parameter
!      --output--
!        iunit - the logical unit number of the  (first) unit to
!                which error messages are being sent.
!                a value of zero means that the default file, as
!                defined by the i1mach routine, is being used.
!
!     written by ron jones, with slatec common math library subcommittee
!     latest revision --- 23 may 1979
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  j4save
!***end prologue  xgetun
!***first executable statement  xgetun
      iunit = j4save(3,0,.false.)
      return
      end
      subroutine xsetf(kontrl)
!***begin prologue  xsetf
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3a
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  set the error control flag.
!***description
!
!     abstract
!        xsetf sets the error control flag value to kontrl.
!        (kontrl is an input parameter only.)
!        the following table shows how each message is treated,
!        depending on the values of kontrl and level.  (see xerror
!        for description of level.)
!
!        if kontrl is zero or negative, no information other than the
!        message itself (including numeric values, if any) will be
!        printed.  if kontrl is positive, introductory messages,
!        trace-backs, etc., will be printed in addition to the message.
!
!              iabs(kontrl)
!        level        0              1              2
!        value
!          2        fatal          fatal          fatal
!
!          1     not printed      printed         fatal
!
!          0     not printed      printed        printed
!
!         -1     not printed      printed        printed
!                                  only           only
!                                  once           once
!
!     written by ron jones, with slatec common math library subcommittee
!     latest revision ---  19 mar 1980
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  j4save,xerrwv
!***end prologue  xsetf
!***first executable statement  xsetf
      if ((kontrl.ge.(-2)).and.(kontrl.le.2)) go to 10
         call xerrwv('xsetf  -- invalid value of kontrl (i1).',33,1,2, &
        1,kontrl,0,0,0.,0.)
         return
   10 junk = j4save(2,kontrl,.true.)
      return
      end
      subroutine xsetua(iunita,n)
!***begin prologue  xsetua
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3b
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  set logical unit numbers (up to 5) to which error
!            messages are to be sent.
!***description
!
!     abstract
!        xsetua may be called to declare a list of up to five
!        logical units, each of which is to receive a copy of
!        each error message processed by this package.
!        the purpose of xsetua is to allow simultaneous printing
!        of each error message on, say, a main output file,
!        an interactive terminal, and other files such as graphics
!        communication files.
!
!     description of parameters
!      --input--
!        iunit - an array of up to five unit numbers.
!                normally these numbers should all be different
!                (but duplicates are not prohibited.)
!        n     - the number of unit numbers provided in iunit
!                must have 1 .le. n .le. 5.
!
!     written by ron jones, with slatec common math library subcommittee
!     latest revision ---  19 mar 1980
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  j4save,xerrwv
!***end prologue  xsetua
      dimension iunita(5)
!***first executable statement  xsetua
      if ((n.ge.1).and.(n.le.5)) go to 10
         call xerrwv('xsetua -- invalid value of n (i1).',34,1,2, &
        1,n,0,0,0.,0.)
         return
   10 continue
      do 20 i=1,n
         index = i+4
         if (i.eq.1) index = 3
         junk = j4save(index,iunita(i),.true.)
   20 continue
      junk = j4save(5,n,.true.)
      return
      end
      subroutine xsetun(iunit)
!***begin prologue  xsetun
!***date written   790801   (yymmdd)
!***revision date  851111   (yymmdd)
!***category no.  r3b
!***keywords  error,xerror package
!***author  jones, r. e., (snla)
!***purpose  set output file to which error messages are to be sent.
!***description
!
!     abstract
!        xsetun sets the output file to which error messages are to
!        be sent.  only one file will be used.  see xsetua for
!        how to declare more than one file.
!
!     description of parameter
!      --input--
!        iunit - an input parameter giving the logical unit number
!                to which error messages are to be sent.
!
!     written by ron jones, with slatec common math library subcommittee
!     latest revision ---  7 june 1978
!***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
!                 handling package', sand82-0800, sandia laboratories,
!                 1982.
!***routines called  j4save
!***end prologue  xsetun
!***first executable statement  xsetun
      junk = j4save(3,iunit,.true.)
      junk = j4save(5,1,.true.)
      return
      end
      FUNCTION RAND(R)
!***BEGIN PROLOGUE  RAND
!***DATE WRITTEN   770401   (YYMMDD)
!***REVISION DATE  861211   (YYMMDD)
!***CATEGORY NO.  L6A21
!***KEYWORDS  LIBRARY=SLATEC(FNLIB),TYPE=SINGLE PRECISION(RAND-S),
!             RANDOM NUMBER,SPECIAL FUNCTIONS,UNIFORM
!***AUTHOR  FULLERTON, W., (LANL)
!***PURPOSE  Generates a uniformly distributed random number.
!***DESCRIPTION
!
!      This pseudo-random number generator is portable among a wide
! variety of computers.  RAND(R) undoubtedly is not as good as many
! readily available installation dependent versions, and so this
! routine is not recommended for widespread usage.  Its redeeming
! feature is that the exact same random numbers (to within final round-
! off error) can be generated from machine to machine.  Thus, programs
! that make use of random numbers can be easily transported to and
! checked in a new environment.
!      The random numbers are generated by the linear congruential
! method described, e.g., by Knuth in Seminumerical Methods (p.9),
! Addison-Wesley, 1969.  Given the I-th number of a pseudo-random
! sequence, the I+1 -st number is generated from
!             X(I+1) = (A*X(I) + C) MOD M,
! where here M = 2**22 = 4194304, C = 1731 and several suitable values
! of the multiplier A are discussed below.  Both the multiplier A and
! random number X are represented in double precision as two 11-bit
! words.  The constants are chosen so that the period is the maximum
! possible, 4194304.
!      In order that the same numbers be generated from machine to
! machine, it is necessary that 23-bit integers be reducible modulo
! 2**11 exactly, that 23-bit integers be added exactly, and that 11-bit
! integers be multiplied exactly.  Furthermore, if the restart option
! is used (where R is between 0 and 1), then the product R*2**22 =
! R*4194304 must be correct to the nearest integer.
!      The first four random numbers should be .0004127026,
! .6750836372, .1614754200, and .9086198807.  The tenth random number
! is .5527787209, and the hundredth is .3600893021 .  The thousandth
! number should be .2176990509 .
!      In order to generate several effectively independent sequences
! with the same generator, it is necessary to know the random number
! for several widely spaced calls.  The I-th random number times 2**22,
! where I=K*P/8 and P is the period of the sequence (P = 2**22), is
! still of the form L*P/8.  In particular we find the I-th random
! number multiplied by 2**22 is given by
! I   =  0  1*P/8  2*P/8  3*P/8  4*P/8  5*P/8  6*P/8  7*P/8  8*P/8
! RAND=  0  5*P/8  2*P/8  7*P/8  4*P/8  1*P/8  6*P/8  3*P/8  0
! Thus the 4*P/8 = 2097152 random number is 2097152/2**22.
!      Several multipliers have been subjected to the spectral test
! (see Knuth, p. 82).  Four suitable multipliers roughly in order of
! goodness according to the spectral test are
!    3146757 = 1536*2048 + 1029 = 2**21 + 2**20 + 2**10 + 5
!    2098181 = 1024*2048 + 1029 = 2**21 + 2**10 + 5
!    3146245 = 1536*2048 +  517 = 2**21 + 2**20 + 2**9 + 5
!    2776669 = 1355*2048 + 1629 = 5**9 + 7**7 + 1
!
!      In the table below LOG10(NU(I)) gives roughly the number of
! random decimal digits in the random numbers considered I at a time.
! C is the primary measure of goodness.  In both cases bigger is better.
!
!                   LOG10 NU(I)              C(I)
!       A       I=2  I=3  I=4  I=5    I=2  I=3  I=4  I=5
!
!    3146757    3.3  2.0  1.6  1.3    3.1  1.3  4.6  2.6
!    2098181    3.3  2.0  1.6  1.2    3.2  1.3  4.6  1.7
!    3146245    3.3  2.2  1.5  1.1    3.2  4.2  1.1  0.4
!    2776669    3.3  2.1  1.6  1.3    2.5  2.0  1.9  2.6
!   Best
!    Possible   3.3  2.3  1.7  1.4    3.6  5.9  9.7  14.9
!
!             Input Argument --
! R      If R=0., the next random number of the sequence is generated.
!        If R .LT. 0., the last generated number will be returned for
!          possible use in a restart procedure.
!        If R .GT. 0., the sequence of random numbers will start with
!          the seed R mod 1.  This seed is also returned as the value of
!          RAND provided the arithmetic is done exactly.
!
!             Output Value --
! RAND   a pseudo-random number between 0. and 1.
!***REFERENCES  (NONE)
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  RAND
      SAVE IA1, IA0, IA1MA0, IC, IX1, IX0
      DATA IA1, IA0, IA1MA0 /1536, 1029, 507/
      DATA IC /1731/
      DATA IX1, IX0 /0, 0/
!***FIRST EXECUTABLE STATEMENT  RAND
      IF (R.LT.0.) GO TO 10
      IF (R.GT.0.) GO TO 20
!
!           A*X = 2**22*IA1*IX1 + 2**11*(IA1*IX1 + (IA1-IA0)*(IX0-IX1)
!                   + IA0*IX0) + IA0*IX0
!
      IY0 = IA0*IX0
      IY1 = IA1*IX1 + IA1MA0*(IX0-IX1) + IY0
      IY0 = IY0 + IC
      IX0 = MOD (IY0, 2048)
      IY1 = IY1 + (IY0-IX0)/2048
      IX1 = MOD (IY1, 2048)
!
 10   RAND = IX1*2048 + IX0
      RAND = RAND / 4194304.
      RETURN
!
 20   IX1 = AMOD(R,1.)*4194304. + 0.5
      IX0 = MOD (IX1, 2048)
      IX1 = (IX1-IX0)/2048
      GO TO 10
!
      END
subroutine timestamp ( )

!*******************************************************************************
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    May 31 2001   9:45:54.872 AM
!
!  Modified:
!
!    15 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none
!
  character ( len = 40 ) string
!
  call timestring ( string )

  write ( *, '(a)' ) trim ( string )

  return
end
subroutine timestring ( string )

!*******************************************************************************
!
!! TIMESTRING writes the current YMDHMS date into a string.
!
!  Example:
!
!    STRING = 'May 31 2001   9:45:54.872 AM'
!
!  Modified:
!
!    15 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = * ) STRING, contains the date information.
!    A character length of 40 should always be sufficient.
!
  implicit none
!
  character ( len = 8 ) ampm
  integer d
  character ( len = 8 ) date
  integer h
  integer m
  integer mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer n
  integer s
  character ( len = * ) string
  character ( len = 10 ) time
  integer values(8)
  integer y
  character ( len = 5 ) zone
!
  call date_and_time ( date, time, zone, values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( string, '(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    trim ( month(m) ), d, y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
