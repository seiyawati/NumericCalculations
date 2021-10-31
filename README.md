# 気泡の相互作用の挙動解析

## フローチャート

1. bibun2
  ```fortran=
    Eq1 = (                           &
        ( 1 + vv / Va ) * ( Eq2 - Sum_Pij ) / Rho_L     &
      + ( Eq3 - dSum_Pij_dt ) * rr / ( Va * Rho_L )         &
      - 1.5d0 * (1 - vv / ( 3 * Va ) ) * (vv**2)    &
              ) / ( rr * (1 - vv / Va )) 

    Eq2 = Pb  - Pinf

    Eq3 = ( Ps - k3 * Pg ) * vv / rr

    Eq4 = - Rho_L * ( tmp + 0.5 * ( Fj / dist**2 + tmp / Va ) ** 2 )

    Eq5 = t - dist / Va

    Eq6 = (Rj**2) * ( - Vj + tmp / Va )

    Eq7 = - Rj * tmp

    Eq10-1 = ( ( h - tmp) * r(m,j) + tmp * r(m+1,j) ) / h 

    Eq10-2 = ( ( h - tmp) * v(m,j) + tmp * v(m+1,j) ) / h

    Eq11 = dtau_dt * Eq13

    Eq12 = 1 / ( 1 - Vj / Va )

    Eq13 = - Rho_L * ( dGj_dt / dist + ( Fj / dist**2 + Gj / ( Va * dist ) )  &
                          * ( dFj_dt / dist**2 + dGj_dt / ( Va * dist) ) )

    Eq14 = Rj**2 * ( ( Vj / Va - 1 ) * Aj +  Eq15 * Vj / (Rho_L * Va) ) &
              + Rj * ( ( Vj / Va - 2 ) * Vj -  2 * ( Pinf - Pbj ) / (Rho_L * Va) )

    Eq15 = ( - k3 * Pgj + Psj ) / Rj

    Eq16 = - Vj * ( 0.5 * Vj**2 - ( Pinf - Pbj ) / Rho_L ) &
              - Rj * Vj * ( Aj + Eq15 / Rho_L )
    
    Eq17 = ( - v(m,j) + v(m+1,j)  ) / h
  ``` 
