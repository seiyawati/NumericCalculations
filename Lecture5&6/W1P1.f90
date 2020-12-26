program Rungekutta
    implicit none
    
      real :: x0, y1_0, y2_0, x_end, y1_end, y2_end, ans_dx
      real, external :: func1, func2
      integer :: i, n, m
     
    !  results are output at  x = x0 + j*ans_dx (j=1, m) 
    !   n : ans_dx/n = dx  in Runge Kutta integration
    !      d(y1)/dx = func1( x, y1, y2 )
    !      d(y2)/dx = func2( x, y1, y2 )
    !  y1_0 and y2_0 are initial value at x0
    !      
        ans_dx = 0.1
        m = (15*3.1415/ans_dx);  n=100
    
        x0=0; y1_0=0; y2_0=0;
    
       write(*,*) '# n  Euler  Runge-Kutta Answer'
       write(*,*) x0, y1_0, y2_0
    
        x_end=ans_dx
        do i=1, m
         call dbl_rk(x0, y1_0, y2_0, n, x_end, func1, func2,  y1_end, y2_end)
         write(*,*) x_end, y1_end, y2_end
         x0 = x_end; x_end=x0+ans_dx
         y1_0  = y1_end; y2_0 = y2_end
        end do
    
    end
    
    
    subroutine dbl_rk( x0, y1_0, y2_0, n, x_end, f1, f2, y1_end, y2_end)
    implicit none
      real :: x0, y1_0, y2_0, x_end, y1_end, y2_end
      real, external :: f1, f2
      real ::  k11, k12, k13, k14
      real ::  k21, k22, k23, k24
      real :: x, y1, y2, dx
      integer :: i, n
    
      dx = (x_end - x0)/n
      x=x0
      y1=y1_0; y2=y2_0
      do i=1, n
        k11 = dx * f1(x, y1, y2)
        k21 = dx * f2(x, y1, y2)
        k12 = dx * f1( x+dx/2, y1+k11/2, y2+k21/2 )
        k22 = dx * f2( x+dx/2, y1+k11/2, y2+k21/2 )
        k13 = dx * f1( x+dx/2, y1+k12/2, y2+k22/2 )
        k23 = dx * f2( x+dx/2, y1+k12/2, y2+k22/2 )
        k14 = dx * f1( x+dx  , y1+k13, y2+k23/2  )
        k24 = dx * f2( x+dx  , y1+k13, y2+k23/2  )
        y1 = y1 + (1.0/6.0)*(k11+2*k12+2*k13+k14)
        y2 = y2 + (1.0/6.0)*(k21+2*k22+2*k23+k24)
        x = x + dx
      end do
      y1_end=y1
      y2_end=y2
    
    !  write(*,*) 'Rungekutta ', x0, dx, n, x_end, y_end
    end subroutine
    
    ! d^2 y / dx^2 + p dy/dx +qy = f( x )
    !   y1 = y; y2 = dy/dx
    !
    !  d (y1)/dx = y2
    !  d (y2)/dx = -p y2 - q y1 + f(x)
    
    real function func1(x, y1, y2)
    real :: x, y1, y2
    
       func1 =  y2
    
    end function
    
    real function func2(x, y1, y2)
    real :: x, y1, y2
    
       func2 =  -y1+sin((sqrt(3.0)/2.0)*x)
    
    end function

end program Rungekutta