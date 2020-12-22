      program exercise0312
      implicit none
      double precision :: t0, x0, h, t, x, tmax, k1, k2, y0, gosa, gosa0
      integer :: last

      write(*,*) 'Input initial values for (t,x)'
      read(*,*) t0, x0
      write(*,*) 'Input time step h'
      read(*,*) h
      write(*,*) 'Input tmax'
      read(*,*) tmax

      y0 = x0 - t0**3 ! used for the exact solution

      open(10,file='sol.dat',form='formatted',action='write')

      t = t0 ; x = x0 ; last = 0 ; gosa0 = 0.0d0
      Integrator : do
       if(t + h >= tmax) then
        h = tmax - t
        last = 1
       endif
       k1 = bibun1(t,x)
       x = x + h * k1
       t = t + h       
       write(10,'(2(1X,ES16.8E3))') t, x

! difference from exact solution
       gosa = x - ( t**3 + 1.0d0 / ( 1.0d0 - (1.0d0 - 1.0d0 / y0) * dexp(-t) ) )
       write(*,'(3(1X,ES16.8E3))') t, x, gosa
       gosa0 = max(gosa0,dabs(gosa))

       if(last == 1) exit Integrator
      enddo Integrator

      stop

      contains
      
      function bibun1(p,q)
      implicit none
      double precision, intent(IN) :: p, q
      double precision :: bibun1, p2, p3, p6, q2

      p2 = p * p
      p3 = p * p2
      p6 = p3 * p3
      q2 = q * q
      bibun1 = 3.0d0 * p2 - p3 - p6 + ( 2.0d0 * p3 + 1.0d0 ) * q - q2

      end function bibun1

      end program
