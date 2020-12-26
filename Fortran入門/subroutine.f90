subroutine sum_val(n,value,total)

    implicit none

    integer, intent(in) :: n ! inはメインプログラムから呼ぶ
    real(8), intent(inout) :: value(n) !inoutは両方
    real(8), intent(out) :: total ! outはsubroutineからメインプログラムに呼ぶ
    integer :: i

    do i=1,n
        value(i) = i
    enddo

    total = sum( value )

end subroutine sum_val

!-------------------------------------------------------------------

program sumall
    implicit none

    integer :: n
    real(8),allocatable :: value(:) ! 配列の動的割り付け
    real(8) :: total

    read(*,*) n

    allocate( value(n) )

    call sum_val(n,value,total)

    print *, total

    deallocate( value ) !領域の解放を明示している


end program sumall