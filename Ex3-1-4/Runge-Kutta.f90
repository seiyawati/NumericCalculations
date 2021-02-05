program RungeKutta_method
    implicit none
    double precision :: t0, x0, h, t, x, xe, tmax, k1, k2, k3, k4, y0, gosa, gosa0
    integer :: last
    double precision, parameter :: a = 1.0d0, b = 2.0d0

    write(*,*) 'Input initial values for (t,x)'
    read(*,*) t0, x0
    write(*,*) 'Input time step h'
    read(*,*) h
    write(*,*) 'Input tmax'
    read(*,*) tmax

    y0 = x0 - t0**3 ! used for the exact solution

    open(10, file='sol.dat', form='formatted', action='write')

    t = t0 ; x = x0 ; last = 0 ; gosa0 = 0.0d0
    write(10,'(3(1X,ES13.5E3))') t, x, x

    Integrator : do
        if(t + h >= tmax) then
            h = tmax - t
            last = 1
        endif
        k1 = bibun1(t,x)
        k2 = bibun1(t+0.5d0*h, x+0.5d0*h*k1)
        k3 = bibun1(t+0.5d0*h, x+0.5d0*h*k2)
        k4 = bibun1(t+h, x+h*k3)
        x = x + h * (k1+k2+2*k3+k4) / 6.0d0
        t = t + h
        if(y0 + (1.0d0 - y0) * dexp(t0 - t) <= 0.0d0) then
            write(*,'(A,ES13.5E3)') &
                'STOP : the exact solution blows up before', t
            stop
        endif
        xe = exact(t)
        write(10,'(3(1X,ES13.5E3))') t, x, xe

! difference from exact solution
        gosa = x - xe
        gosa0 = max(gosa0,dabs(gosa))

        if(last == 1) exit Integrator
    enddo Integrator
    
    write(*,'(A,ES13.5E3)') 'log10(error) = ', dlog10(gosa0)
    stop

    contains

    function bibun1 (x,y)
        implicit none
        double precision, intent(IN) :: x, y
        double precision :: bibun1
    
        bibun1 = a * x - b * x * y
    end function bibun1

end program RungeKutta_method