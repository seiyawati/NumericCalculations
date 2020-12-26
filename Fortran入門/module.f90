module utils

    implicit none

contains

    !------------------------------------------
    function sum_val(n) result(total) !resultの変数に値を代入することで、それが関数の返り値となる
        implicit none

        integer, intent(in) :: n
        real(8) :: total

        integer :: i
        real(8) :: value(n)

        do i=1,n
            value(i) = i
        enddo

        total = sum( value )

    end function sum_val
    !------------------------------------------

    subroutine print_values(n)

        implicit none
        integer, intent(in) :: n

        integer :: i

        do i=1,n
            print *, i
        enddo
    
    end subroutine print_values

end module utils

!-------------------------------------------------

program sumall

    use utils ! 一番上にmoduleを使うことを明示
    implicit none

    integer :: i,n
    real(8) :: total

    read(*,*) n

    total = sum_val(n)

    call print_values(n)

    print *, total

    
end program sumall







