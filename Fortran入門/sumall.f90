program sumall
    implicit none

    integer :: i
    real(8) :: value(10),total

    do i=1,10
        value(i) = i
    enddo

    ! total=0.d0
    ! do i=1,10
    !     total = total + value(i)
    ! enddo

    ! 上記の処理をsum関数でやってくれる
    total = sum( value )

    print *, total


end program sumall