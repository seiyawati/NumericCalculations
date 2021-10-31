program Runge_Kutta_method
  implicit none
  DOUBLE PRECISION :: t, r, v, &
    k11, k21, k31, k41, k12, k22, k32, k42
  INTEGER :: last

  !_CONTROL_DATA_
  INTEGER :: nb = 1 ! number of bubble
  DOUBLE PRECISION :: h = 1.0e-9 ! time_step_for_computation
  DOUBLE PRECISION :: Tmax = 1.0e-4 ! maximum_time_of_computation
  DOUBLE PRECISION :: eps = 1.0e-6 ! tolerance_of_RKF45_scheme 
  DOUBLE PRECISION :: Tprt = 5.0e-7 ! output_interval_of_pressure_distribution 
  DOUBLE PRECISION :: Xmin = -1.0e-3 ! x_minimum_of_output_interval 
  DOUBLE PRECISION :: Xmax = 5.0e-3 ! x_maximum_of_output_interval
  DOUBLE PRECISION :: Xint = 1000 ! number_of_division_of_output_interval

  !_BUBBLE_DATA_
  DOUBLE PRECISION, PARAMETER :: x = 0.0
  DOUBLE PRECISION, PARAMETER :: y = 0.0
  DOUBLE PRECISION, PARAMETER :: z = 0.0
  DOUBLE PRECISION, PARAMETER :: Rref = 1.0e-5 ! reference_radius
  DOUBLE PRECISION, PARAMETER :: Pref = 2.500e3 ! reference_pressure
  DOUBLE PRECISION, PARAMETER :: sigma = 7.275e-2 ! surface_tension
  DOUBLE PRECISION, PARAMETER :: kappa = 5.0d0 / 3.0d0 ! specific_heat_ratio
  DOUBLE PRECISION, PARAMETER :: Pv = 2.337e3 ! vapor pressure
  DOUBLE PRECISION, PARAMETER :: R0 = 1.0e-5
  DOUBLE PRECISION, PARAMETER :: v0 = 0.0

  !_LIQUID_DATA_
  DOUBLE PRECISION :: Rho_L = 998.2 ! liquid_density
  DOUBLE PRECISION :: Pinf = 1.0e5 ! pressure_at_infinity
  DOUBLE PRECISION :: Va = 1478.0 ! speed_of_sound

  open(10, file='result.txt', form='formatted', action='write')

  write(10,'(A,ES13.6E2)') 'h = ', h
  write(10,'(A,ES13.6E2)') 'Tmax = ', Tmax
  write(10,*)
  write(10,*)
  write(10,'(3A14)') 't', 'r', 'v'

  t = 0.0d0 ; r = R0 ; v = V0 ; last = 0
  Integrator : do
    if(t + h >= Tmax) then
      h = Tmax - t
      last = 1
    endif

    t = t + h 

       k11 = bibun1(r,v)
       k12 = bibun2(r,v)

       k21 = bibun1(r + 0.5d0 * h * k11, v + 0.5d0 * h * k12)
       k22 = bibun2(r + 0.5d0 * h * k11, v + 0.5d0 * h * k12)

       k31 = bibun1(r + 0.5d0 * h * k21, v + 0.5d0 * h * k22)
       k32 = bibun2(r + 0.5d0 * h * k21, v + 0.5d0 * h * k22)

       k41 = bibun1(r + h * k31, v + h * k32)
       k42 = bibun2(r + h * k31, v + h * k32)

       r = r + h *( k11 + 2 * k21 + 2 * k31 + k41 ) / 6.0d0
       v = v + h *( k12 + 2 * k22 + 2 * k32 + k42 ) / 6.0d0

    write(10,'(3(1X,ES13.6E2))') t, r, v

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
    double precision :: bibun2, Pb

    Pb = Pv + Pref * ((Rref/r) ** (3.0*kappa)) 
    
    bibun2 = ((Pb - (2.0d0 * sigma / r) - Pinf) / Rho_L - 1.5d0 * (v**2)) / r

  end function bibun2

end program Runge_Kutta_method
