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
