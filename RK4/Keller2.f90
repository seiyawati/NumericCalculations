program Runge_Kutta_method
  implicit none
  DOUBLE PRECISION :: t, &
    k11, k21, k31, k41, k12, k22, k32, k42
  INTEGER :: k, last
  integer :: nb
  double precision :: h, Tmax, eps, Tprt, Xmin, Xmax, Xint, &
    Rref, Pref, sigma, kappa, Pv, Rho_L, Pinf, Va
  double precision :: x(2), y(2), z(2), R0(2), V0(2), r(2), v(2)
  namelist /CONTROL_DATA/ nb, h, Tmax, eps, Tprt, Xmin, Xmax, Xint
  namelist /BUBBLE0/ Rref, Pref, sigma, kappa, Pv
  namelist /BUBBLEs/ x, y, z, R0, V0
  namelist /LIQUID_DATA/ Rho_L, Pinf, Va

  open(11,file='settings1.txt',status='old',action='read')
  read(11,nml=CONTROL_DATA)
!  allocate(x(nb)) 
!  allocate(y(nb)) 
!  allocate(z(nb)) 
!  allocate(R0(nb)) 
!  allocate(V0(nb)) 
  read(11,nml=BUBBLE0)
  read(11,nml=BUBBLEs)
  read(11,nml=LIQUID_DATA)
  close(11)

!  allocate(r(nb)) 
!  allocate(v(nb)) 

  open(10, file='result.txt', form='formatted', action='write')

  write(10,'(A,ES13.6E2)') 'h = ', h
  write(10,'(A,ES13.6E2)') 'Tmax = ', Tmax
  write(10,*)
  write(10,*)
  write(10,'(3A14)') 't', 'r', 'v'

  t = 0.0d0 ; last = 0
  do k = 1, nb
   r(k) = R0(k)
   v(k) = V0(k)
  enddo

  Integrator : do
    if(t + h >= Tmax) then
      h = Tmax - t
      last = 1
    endif

    t = t + h 
    do k = 1, nb
  
       k11 = bibun1(r(k),v(k))
       k12 = bibun2(r(k),v(k))

       k21 = bibun1(r(k) + 0.5d0 * h * k11, v(k) + 0.5d0 * h * k12)
       k22 = bibun2(r(k) + 0.5d0 * h * k11, v(k) + 0.5d0 * h * k12)

       k31 = bibun1(r(k) + 0.5d0 * h * k21, v(k) + 0.5d0 * h * k22)
       k32 = bibun2(r(k) + 0.5d0 * h * k21, v(k) + 0.5d0 * h * k22)

       k41 = bibun1(r(k) + h * k31, v(k) + h * k32)
       k42 = bibun2(r(k) + h * k31, v(k) + h * k32)

       r(k) = r(k) + h *( k11 + 2 * k21 + 2 * k31 + k41 ) / 6.0d0
       v(k) = v(k) + h *( k12 + 2 * k22 + 2 * k32 + k42 ) / 6.0d0

    enddo

    write(10,'(5(1X,ES13.6E2))') t, r(1), v(1), r(2), v(2)

    if(last == 1) exit Integrator

  enddo Integrator

  stop

  contains


  function bibun1 (r,v)
    implicit none
    double precision, intent(IN) :: r, v
    double precision :: bibun1

    bibun1 = v

  end function bibun1


  function bibun2 (r,v)
    implicit none
    double precision, intent(IN) :: r, v
    double precision :: bibun2, Pb, Pg, Ps, k3

    k3 = 3 * kappa

    Pg = Pref * ((Rref/r) ** k3)

    Ps = 2 * sigma / r

    Pb = Pv + Pg - Ps
    
    bibun2 = (                                       &
        ( 1 + v / Va ) * (Pb  - Pinf) / Rho_L        &
      + ( v / Va ) * ( Ps - k3 * Pg ) / Rho_L        &
      - 1.5d0 * (1 - v / ( 3 * Va ) ) * (v**2)       &
              ) / ( r * (1 - v / Va )) 
  end function bibun2

  function func5(xi, xj)
    implicit none
    double precision, intent(IN) :: xi, xj
    double precision :: func5, dij

    dij = abs(xi - xj)

    func5 = t - (dij/Pinf)
  end function func5

end program Runge_Kutta_method
