      subroutine dfault (iparm,rparm)
      implicit double precision (a-h, o-z)
c
c ... dfault sets the default values of iparm and rparm.
c
c ... parameters -- 
c
c          iparm
c           and
c          rparm  arrays specifying options and tolerances
c
c
c ... specifications for parameters
c
      integer   iparm(30)
      dimension rparm(30)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
c     description of variables in common blocks in main routine
c
c     srelpr  - computer precision (approx.)
c     if installer of package does not know srelpr value,
c     an approximate value can be determined from a simple
c     fortran program such as 
c
c     srelpr = 1.0d0
c  2  srelpr = 0.5d0*srelpr
c     temp = srelpr + 1.0d0
c     if (temp .gt. 1.0d0) go to 2
c     srelpr = 2.0d0*srelpr
c     write (6,3) srelpr
c  3  format (1x,'srelpr = ',d20.10)
c     stop
c     end 
c
c
c     some values are-
c
c     srelpr = 7.1d-15   for cray x-mp  (approx.) 2**-47
c            = 1.49d-8   for dec 10  (approx.) 2**-26
c            = 1.192d-7  for vax 11/780 (approx) 2**-23
c            = 4.768d-7  for ibm 370/158
c
c             *** should be changed for other machines ***
c
c     to facilitate convergence, rparm(1) should be set to
c          500.*srelpr or larger
c
c
      srelpr = 7.1d-15
c
c ... keygs is a flag to specify how gather/scatter operations
c     are performed.
c       = 1    gather explicitly into a workspace vector
c       = 2    gather implicitly using indirect addressing
c
c
      keygs = 1
c
c ... keyzer is a flag to specify if memory has been zeroed out.
c     i.e., is the operation  0.0 * indefinite = 0.0  legal 
c       = 0    not legal
c       = 1    legal
c
      keyzer = 0
c
c
      iparm(1)  =      2
      iparm(2)  =    100
      iparm(3)  =      0
      iparm(4)  =      6
      iparm(5)  =      0
      iparm(6)  =      1
      iparm(7)  =      1
      iparm(8)  =      1
      iparm(9)  =      5
      iparm(10) = 100000
      iparm(11) =      0
      iparm(12) =      2
      iparm(13) =      0
      iparm(14) =      0
      iparm(15) =      1
      iparm(16) =      0
      iparm(17) =      0
      iparm(18) =      2
      iparm(19) =     -1
      iparm(20) =     -1
      iparm(21) =      1
      iparm(22) =      1
      iparm(23) =      2
      iparm(24) =      0
      iparm(25) =      1
c
      rparm(1)  = 1.0d-6
      rparm(2)  = 2.0d0
      rparm(3)  = 1.0d0
      rparm(4)  = 0.75d0
      rparm(5)  = 0.75d0
      rparm(6)  = 0.0d0
      rparm(7)  = 0.0d0
      rparm(8)  = 0.0d0
      rparm(9)  = 1.0d0
      rparm(10) = 0.0d0
      rparm(11) = 0.25d0
      rparm(12) = 0.0d0
      rparm(13) = 0.0d0
      rparm(14) = 0.0d0
      rparm(15) = 500.0d0*srelpr
      rparm(16) = 0.0d0
c
      return
      end 
      double precision function timer (timdmy) 
      implicit double precision (a-h, o-z)
c
c ... timer is a routine to return the execution time in
c ... seconds.  timer uses the fortran timing routine second.
c
c ... parameters -- 
c
c          timdmy   dummy argument
c
c
c     note -- on many computer systems there is a cpu-time subprogram 
c             which is more accurate than the fortran routine second. 
c
c
c     use the following when using second
c
c     timer = second (0.0)
c
c
c *********************************************
c **                                         **
c **   this routine is not portable.         **
c **                                         **
c *********************************************
c
c ... specifications for parameters
c
c
      timer = 1.0
c      timer = dble (second ())
c
c     real tarray(2)
c     time = dble(etime (tarray))
c
c     call system_clock (count = icount, count_rate = irate)
c     timer = dble(icount) / dble(irate)
c
      return
      end 
