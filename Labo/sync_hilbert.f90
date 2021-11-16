program synchronization
  implicit none
  double precision :: x_, y_, z_, R0_, V0_, &
    k11, k21, k31, k41, k12, k22, k32, k42, sum, res, tmp
  integer :: nb, last, i, j, k, m, n, Nt, NHT
  double precision :: t, h, Tmax, eps, Tprt, Xmin, Xmax, Xint, vk, Hv, &
    Rref, Pref, sigma, kappa, Pv, Rho_L, Pinf, Va, sum1, sum2, pi2, amp, phs
  double precision, allocatable, dimension(:) :: x, y, z
  double precision, allocatable, dimension(:,:) :: r, v, envlp, phase
  character :: fmt*80
  namelist /CONTROL_DATA/ nb, h, Tmax, eps, Tprt, Xmin, Xmax, Xint, NHT
  namelist /BUBBLE0/ Rref, Pref, sigma, kappa, Pv
  namelist /BUBBLEs/ x_, y_, z_, R0_, V0_
  namelist /LIQUID_DATA/ Rho_L, Pinf, Va

  pi2 = 2.0d0 * dacos(-1.0d0)

  open(11,file='set_sync2d.txt',status='old',action='read')
  read(11,nml=CONTROL_DATA)
  Nt = int(Tmax / h) + 1
  allocate(x(nb)) 
  allocate(y(nb)) 
  allocate(z(nb)) 
  allocate(r(0:Nt,nb)) 
  allocate(v(0:Nt,nb)) 
  allocate(envlp(int(Nt/NHT),nb))
  allocate(phase(int(Nt/NHT),nb))
  read(11,nml=LIQUID_DATA)
  read(11,nml=BUBBLE0)
  do k = 1, nb
   read(11,nml=BUBBLEs)
   x(k) = x_
   y(k) = y_
   z(k) = z_
   r(0,k) = R0_
   v(0,k) = V0_
  enddo
  close(11)


  open(10, file='result.txt', form='formatted', action='write')

  write(10,'(A,ES13.6E2)') 'h = ', h
  write(10,'(A,ES13.6E2)') 'Tmax = ', Tmax
  write(10,*)
  write(10,*)
  write(fmt,'(A,I1,A)') '(A14,', 2 *nb, '(A11,I3.3))'
  write(10,fmt) 't', ( 'r', k, 'v', k, k = 1, nb )

  write(fmt,'(A,I3.3,A)') '(', 1 + 2 *nb, '(1X,ES13.6E2))'

  t = 0.0d0  ! the latest time when we know the values
  n = 0
  last = 0

  Integrator : do
    if(t + h >= Tmax) then
      h = Tmax - t
      last = 1
    endif

    do i = 1, nb
  
      k11 = v(n,i)
      k12 = bibun2(r(n,i), v(n,i), i, t)

      k21 = v(n,i) + h * k12 / 2
      k22 = bibun2(r(n,i) + h * k11 / 2, v(n,i) + h * k12 / 2, i, t + h / 2)

      k31 = v(n,i) + 0.5d0 * h * k22
      k32 = bibun2(r(n,i) + h * k21 / 2, v(n,i) + h * k22 / 2, i, t + h / 2)

      k41 = v(n,i) + h * k32
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
  close(10)


  do i = 1, nb
  do k = NHT, n-NHT, NHT

   sum = v(0,i) / (k - 0.5d0)
   res = 0.0d0
   do m = 1, k-1
    res = res + v(m,i) * ( 1.0d0 / (k - m - 0.5d0) + 1.0d0 / (k - m + 0.5d0) )
    tmp = sum
    sum = sum + res
    tmp = sum - tmp
    res = res - tmp
   enddo ! m = 1, k-1
   sum1 = sum

   sum = v(n,i) / ( n - k - 0.5d0)
   res = 0.0d0
   do m = k+1, n-1
    res = res + v(m,i) * ( 1.0d0 / (m - k - 0.5d0) + 1.0d0 / (m - k + 0.5d0) )
    tmp = sum
    sum = sum + res
    tmp = sum - tmp
    res = res - tmp
   enddo ! m = k+1, n-1
   sum2 = sum

   vk = v(k,i)
   Hv = (sum1 - sum2) / pi2 
   amp = dsqrt( vk**2 + Hv**2 )
   phs = dacos( vk / amp )
   if(Hv < 0.0d0) phs = pi2 - phs
   m = k / NHT
   envlp(m,i) = amp
   phase(m,i) = phs

  enddo ! k = NHT, n-NHT, NHT
  enddo ! i = 1, nb

  open(20, file='phs.txt', form='formatted', action='write')
  write(fmt,'(A,I3.3,A)') '(', 1 + 2 * nb, '(1X,ES13.6E2))'
  do k = 1, m
   write(20,fmt) NHT * k * h, (envlp(k,i), phase(k,i), i = 1, nb)
  enddo
  close(20)

  stop

  contains


  function bibun2 (Ri,Vi,i,tt)
    implicit none
    integer :: m, j
    integer, intent(IN) :: i
    double precision, intent(IN) :: Ri, Vi, tt
    double precision :: bibun2, Pbi, Pgi, Psi, k3, tmp, &
      Eq2, Eq3, Eq4, Eq11, Eq13, Eq15, &
      dist, tau, Rj, Vj, Aj, Fj, Gj, dFj_dt, dGj_dt, &
      Pgj, Psj, Pbj, Pij, dtau_dt, dPij_dt, Sum_Pij, dSum_Pij_dt

    k3 = 3 * kappa

    Pgi = Pref * ((Rref/Ri) ** k3)

    Psi = 2 * sigma / Ri

    Pbi = Pv + Pgi - Psi
    
    Eq2 = Pbi  - Pinf

    Eq3 = ( Psi - k3 * Pgi ) * Vi / Ri

    Sum_Pij = 0.0d0
    dSum_Pij_dt = 0.0d0
    IA : do j = 1, nb
      if ( j == i ) cycle IA
      dist = dsqrt( ( x(i) - x(j) )**2 + ( y(i) - y(j) )**2 + ( z(i) - z(j) )**2 )
      tau = tt - dist / Va
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
       Pij = - Rho_L * ( tmp + 0.5 * ( Fj / (dist**2) + tmp / Va ) ** 2 ) ! Eq4

       dtau_dt = 1 / ( 1 - Vj / Va )  ! Eq12

       Eq15 = ( - k3 * Pgj + Psj ) / Rj ! Eq15

       Aj = ( - v(m,j) + v(m+1,j)  ) / h   ! Eq17

       dFj_dt = Rj**2 * ( ( Vj / Va - 1 ) * Aj +  Eq15 * Vj / (Rho_L * Va) ) &
        + Rj * Vj * ( ( Vj / Va - 2 ) * Vj -  2 * ( Pinf - Pbj ) / (Rho_L * Va) )  ! Eq14

       dGj_dt = - Vj * ( 0.5 * Vj**2 - ( Pinf - Pbj ) / Rho_L ) &
              - Rj * Vj * ( Aj + Eq15 / Rho_L ) ! Eq16

       Eq13 = - Rho_L * ( dGj_dt / dist + ( Fj / dist**2 + Gj / ( Va * dist ) )  &
                          * ( dFj_dt / dist**2 + dGj_dt / ( Va * dist) ) )

       dPij_dt = dtau_dt * Eq13 ! Eq11

       Sum_Pij = Sum_Pij + Pij

       dSum_Pij_dt = dSum_Pij_dt + dPij_dt

      endif ! (tau >= 0.0) then

    enddo IA ! j = 1, nb

    bibun2 = (                           &
        ( 1 + Vi / Va ) * ( Eq2 - Sum_Pij ) / Rho_L     &
      + ( Eq3 - dSum_Pij_dt ) * Ri / ( Va * Rho_L )         &
      - 1.5d0 * (1 - Vi / ( 3 * Va ) ) * (Vi**2)    &
              ) / ( Ri * (1 - Vi / Va )) 
  end function bibun2

end program synchronization
