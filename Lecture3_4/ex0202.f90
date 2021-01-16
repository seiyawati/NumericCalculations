      program exercise0202
      implicit none
      double precision, parameter :: eps = 1.0d-8
      double precision :: x0, dx, f, dfdx

      write(*,*) 'Input initial guess for the solution'
      read(*,*) x0

      write(*,'(3A19)') 'x', 'f', 'dfdx'

      Newton : do
       f = kansuu(x0)
       dfdx = bibun1(x0)
       write(*,'(3(1X,ES18.10E3))') x0, f, dfdx

       dx = - f / dfdx
       x0 = x0 + dx
       if(dabs(dx) < eps) exit Newton

      enddo Newton

      f = kansuu(x0)
      dfdx = bibun1(x0)
      write(*,'(3(1X,ES18.10E3))') x0, f, dfdx

      stop

      contains


      function kansuu(x)
      implicit none
      double precision :: kansuu
      double precision, intent(IN) :: x

      kansuu = x - dcos(x)

      end function kansuu


      function bibun1(x)
      implicit none
      double precision :: bibun1
      double precision, intent(IN) :: x

      bibun1 = 1.0d0 + dsin(x)

      end function bibun1

      end program
