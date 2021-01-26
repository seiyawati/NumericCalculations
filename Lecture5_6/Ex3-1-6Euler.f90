      program exercise3162
      implicit none
      double precision :: t0, x0, y0, h, t, x, y, tmax, &
        k11, k12
      integer :: last
      double precision, parameter :: a = 1.0d0, b = 2.0d0, c = 1.0d0, d = 3.0d0


      write(*,*) 'Input initial values for (t,x,y)'
      read(*,*) t0, x0, y0

      write(*,*) 'Input time step h'
      read(*,*) h

      write(*,*) 'Input tmax'
      read(*,*) tmax

      open(10, file='result.txt', form='formatted', action='write')

      write(10,'(A,ES13.6E2)') 't0 = ', t0
      write(10,'(A,ES13.6E2)') 'x0 = ', x0
      write(10,'(A,ES13.6E2)') 'y0 = ', y0
      write(10,'(A,ES13.6E2)') 'h = ', h
      write(10,'(A,ES13.6E2)') 'tmax = ', tmax
      write(10,*)
      write(10,*)
      write(10,'(3A14)') 't', 'x', 'y'

      t = t0 ; x = x0 ; y = y0 ; last = 0
      Integrator : do
       if(t + h >= tmax) then
        h = tmax - t
        last = 1
       endif

       t = t + h 

       k11 = bibun1(x,y)
       k12 = bibun2(x,y)

       x = x + h * k11
       y = y + h * k12

       write(10,'(3(1X,ES13.6E2))') t, x, y

       if(last == 1) exit Integrator

      enddo Integrator

      stop

      contains


      function bibun1 (x,y)
      implicit none
      double precision, intent(IN) :: x, y
      double precision :: bibun1

      bibun1 = a * x - b * x * y

      end function bibun1


      function bibun2 (x,y)
      implicit none
      double precision, intent(IN) :: x, y
      double precision :: bibun2

      bibun2 = - c * y + d * x * y

      end function bibun2

      end program

