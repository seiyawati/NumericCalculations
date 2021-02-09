program Heun
  implicit none
  DOUBLE PRECISION :: t0, x0, h, t, x, tmax, k1, k2, y0, gosa, gosa0
  INTEGER :: last

  WRITE(*,*) 'Input initial values for (t,x)'
  READ(*,*) t0, x0
  WRITE(*,*) 'Input time step h'
  READ(*,*) h
  WRITE(*,*) 'Input tmax'
  READ(*,*) tmax

  y0 = x0 - t0**3

  OPEN(10, file='sol.dat', form='formatted', action='write')

  t = t0 ; x = x0 ; last = 0 ; gosa0 = 0.0d0
  Integrator : do
    if(t + h >= tmax) then
      h = tmax - t
      last = 1
    endif
    k1 = bibun1(t,x)
    k2 = bibun1(t + h, x + h * k1)
    x = x + h * 0.5d0 * (k1 + k2)
    t = t + h
    WRITE(10,'(2(1X,ES16.8E3))') t, x

! difference from exact solution
    gosa = x - ( t**3 + 1.0d0 / (1.0d0 - (1.0d0 - 1.0d0 / y0) * dexp(-t) ) ) ! dexp:指数
    gosa0 = max(gosa0,dabs(gosa)) ! dabs:絶対値

    if(last == 1) exit Integrator
  enddo Integrator

  WRITE(*,'(A,ES13.5E3)') 'error = ', gosa0
  stop

  contains

  function bibun1(p,q)
    implicit none
    DOUBLE PRECISION, INTENT(IN) :: p, q ! intent(in)は引数を受け取る, エラーを吐いてくれる
    DOUBLE PRECISION :: bibun1 , p2, p3, p6, q2

    p2 = p * p
    p3 = p * p2
    p6 = p3 * p3
    q2 = q * q
    bibun1 = 3.0d0 * p2 - p3 -p6 + ( 2.0d0 * p3 + 1.0d0 ) * q - q2

  end function bibun1

end program Heun