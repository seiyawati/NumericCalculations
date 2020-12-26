program sumall
    implicit none

    integer :: i,n
    real(8) :: total

    read(*,*) n
    total = sum_val(n)

    print *, total

contains

    function sum_val(n) result(total)
        implicit none

        integer, intent(in) :: n ! inはメインプログラムから呼ぶ
        real(8) :: total
        integer :: i
        real(8) :: value(n)

        do i=1,n
            value(i) = i
        enddo

        total = sum( value )

    end function sum_val

end program sumall