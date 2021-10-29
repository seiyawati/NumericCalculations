program synchronization
  implicit none
  double precision :: x_, y_, z_, R0_, V0_, &
    k11, k21, k31, k41, k12, k22, k32, k42
  integer :: nb, last, i, j, k, n, Nt
  double precision :: t, h, Tmax, eps, Tprt, Xmin, Xmax, Xint, &
    Rref, Pref, sigma, kappa, Pv, Rho_L, Pinf, Va
  double precision, allocatable, dimension(:) :: x, y, z, R0, V0
  double precision, allocatable, dimension(:,:) :: r, v
  character :: fmt*80
  namelist /CONTROL_DATA/ nb, h, Tmax, eps, Tprt, Xmin, Xmax, Xint
  namelist /BUBBLE0/ Rref, Pref, sigma, kappa, Pv
  namelist /BUBBLEs/ x_, y_, z_, R0_, V0_
  namelist /LIQUID_DATA/ Rho_L, Pinf, Va

  open(11,file='set_sync2a.txt',status='old',action='read')
  read(11,nml=CONTROL_DATA)
  allocate(x(nb)) 
  allocate(y(nb)) 
  allocate(z(nb)) 
  allocate(R0(nb)) 
  allocate(V0(nb)) 
  read(11,nml=LIQUID_DATA)
  read(11,nml=BUBBLE0)
  do k = 1, nb
   read(11,nml=BUBBLEs)
   x(k) = x_
   y(k) = y_
   z(k) = z_
   R0(k) = R0_
   V0(k) = V0_
  enddo
  close(11)

  Nt = int(Tmax / h) + 1
  allocate(r(0:Nt,nb)) 
  allocate(v(0:Nt,nb)) 


  open(10, file='result.txt', form='formatted', action='write')

  write(10,'(A,ES13.6E2)') 'h = ', h
  write(10,'(A,ES13.6E2)') 'Tmax = ', Tmax
  write(10,*)
  write(10,*)
  write(fmt,'(A,I1,A)') '(A14,', 2 *nb, '(A11,I3.3))'
  write(10,fmt) 't', ( 'r', k, 'v', k, k = 1, nb )

  write(fmt,'(A,I3.3,A)') '(', 1 + 2 *nb, '(1X,ES13.6E2))'

  t = 0.0d0
  n = 0
  last = 0
  do i = 1, nb
   r(0,i) = R0(i)
   v(0,i) = V0(i)
  enddo

  Integrator : do
    if(t + h >= Tmax) then
      h = Tmax - t
      last = 1
    endif

    do i = 1, nb
  
      k11 = bibun1(r(n,i), v(n,i))
      k12 = bibun2(r(n,i), v(n,i), i, t)

      k21 = bibun1(r(n,i) + 0.5d0 * h * k11, v(n,i) + 0.5d0 * h * k12)
      k22 = bibun2(r(n,i) + 0.5d0 * h * k11, v(n,i) + 0.5d0 * h * k12, i, t + 0.5d0 * h)

      k31 = bibun1(r(n,i) + 0.5d0 * h * k21, v(n,i) + 0.5d0 * h * k22)
      k32 = bibun2(r(n,i) + 0.5d0 * h * k21, v(n,i) + 0.5d0 * h * k22, i, t + 0.5d0 * h)

      k41 = bibun1(r(n,i) + h * k31, v(n,i) + h * k32)
      k42 = bibun2(r(n,i) + h * k31, v(n,i) + h * k32, i, t + h)

      r(n+1,i) = r(n,i) + h * ( k11 + 2 * k21 + 2 * k31 + k41 ) / 6.0d0
      v(n+1,i) = v(n,i) + h * ( k12 + 2 * k22 + 2 * k32 + k42 ) / 6.0d0

    enddo
    n = n + 1
    t = n * h
    if(last == 1) t = Tmax

    write(10,fmt) t, (r(n,i), v(n,i), i = 1, nb)

    if(last == 1) exit Integrator

  enddo Integrator

  stop

  contains


  function bibun1 (rr,vv)
    implicit none
    double precision, intent(IN) :: rr, vv
    double precision :: bibun1

    bibun1 = vv

  end function bibun1


  function bibun2 (rr,vv,i,t)
    implicit none
    integer :: m, j
    integer, intent(IN) :: i
    double precision, intent(IN) :: rr, vv, t
    double precision :: bibun2, Pb, Pg, Ps, k3, &
      Eq2, Eq3, Eq4, Eq11, Eq13, Eq15, &
      dist, tau, Rj, Vj, Aj, Fj, Gj, dFj_dt, dGj_dt, &
      Pgj, Psj, Pbj, Pij, dtau_dt, dPij_dt, tmp, Sum_Pij, dSum_Pij_dt

    k3 = 3 * kappa

    Pg = Pref * ((Rref/rr) ** k3)

    Ps = 2 * sigma / rr

    Pb = Pv + Pg - Ps
    
    Eq2 = Pb  - Pinf

    Eq3 = ( Ps - k3 * Pg ) * vv / rr

    Sum_Pij = 0.0d0
    dSum_Pij_dt = 0.0d0
    IA : do j = 1, nb
      if ( j == i ) cycle IA
      dist = dsqrt( ( x(i) - x(j) )**2 + ( y(i) - y(j) )**2 + ( z(i) - z(j) )**2 )
      tau = t - dist / Va
      if(tau >= 0.0) then
       m = int(tau / h)

       tmp = tau - m * h
       Rj = ( ( h - tmp) * r(m,j) + tmp * r(m+1,j) ) / h  ! Eq10-1
       Vj = ( ( h - tmp) * v(m,j) + tmp * v(m+1,j) ) / h  ! Eq10-2

       Pgj = Pref * ((Rref/Rj) ** k3)

       Psj = 2 * sigma / Rj

       Pbj = Pv + Pgj - Psj

       tmp = 0.5 * Vj**2 - ( Pinf - Pbj ) / Rho_L
       Fj = (Rj**2) * ( - Vj + tmp / Va ) ! Eq6
       Gj = - Rj * tmp   ! Eq7

       tmp = Gj / dist
       Pij = - Rho_L * ( tmp + 0.5 * ( Fj / dist**2 + tmp / Va ) ** 2 ) ! Eq4

       dtau_dt = 1 / ( 1 - Vj / Va )  ! Eq12

       Eq15 = ( - k3 * Pgj + Psj ) / Rj ! Eq15

       Aj = ( - v(m,j) + v(m+1,j)  ) / h   ! Eq17

       dFj_dt = Rj**2 * ( ( Vj / Va - 1 ) * Aj +  Eq15 * Vj / (Rho_L * Va) ) &
              + Rj * ( ( Vj / Va - 2 ) * Vj -  2 * ( Pinf - Pbj ) / (Rho_L * Va) )  ! Eq14

       dGj_dt = - Vj * ( 0.5 * Vj**2 - ( Pinf - Pbj ) / Rho_L ) &
              - Rj * Vj * ( Aj + Eq15 / Rho_L ) ! Eq16

       Eq13 = - Rho_L * ( dGj_dt / dist + ( Fj / dist**2 + Gj / ( Va * dist ) )  &
                          * ( dFj_dt / dist**2 + dGj_dt / ( Va * dist) ) )

       dPij_dt = dtau_dt * Eq13 ! Eq11

       Sum_Pij = Sum_Pij + Pij

       dSum_Pij_dt = dSum_Pij_dt + dPij_dt

      endif

    enddo IA ! j = 1, nb

    bibun2 = (                           &
        ( 1 + vv / Va ) * ( Eq2 - Sum_Pij ) / Rho_L     &
      + ( Eq3 - dSum_Pij_dt ) * rr / ( Va * Rho_L )         &
      - 1.5d0 * (1 - vv / ( 3 * Va ) ) * (vv**2)    &
              ) / ( rr * (1 - vv / Va )) 
  end function bibun2

  end program synchronization
