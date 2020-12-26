module utils

    implicit none
    real(8) :: total_global ! グローバル変数

contains

    !------------------------------------------
    function read_num() result(num)

        implicit none

        integer :: num

        read(*,*) num

    end function read_num

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
        total_global = total

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

    !-------------------------------------

    subroutine print_total()

        implicit none

        print *, 'total_value = ',total_global
    
    end subroutine print_total

end module utils

!-------------------------------------------------

program sumall

    use utils ! 一番上にmoduleを使うことを明示
    implicit none

    integer :: i,n
    real(8) :: total

    n = read_num()

    total = sum_val(n)

    call print_values(n)

    call print_total()
    
end program sumall







