      program exercise0101
      implicit none
      integer, parameter :: nall = 8 
      integer mark(nall)
      data mark/71,62,83,94,55,76,87,38/
      integer :: goukei, i

      goukei = 0

      do i = 1, nall
       goukei = goukei + mark(i)
      enddo

      write(*,'(A,F5.1)') 'Average mark is ' &
         , nint(real(goukei) / nall * 10) / 10.0

      end program
