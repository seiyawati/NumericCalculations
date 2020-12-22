program exercise0101
    ! 「暗黙の方宣言」を防ぐ、コンパイル時のバグを防止
    implicit none
    ! parameterで変数が定数であることを示す
    integer, parameter :: nall = 8

    ! これでmark(1)からmark(nall)までの配列を用意
    integer mark(nall)

    ! dataでmark(1)=71, mark(2)=62, ...., mark(8)=38を指定
    data mark/71, 62, 83, 94, 55, 76, 87, 38/

    integer :: goukei, i

    ! これより前で値が入っている可能性があるから0を代入す（習慣）
    goukei = 0

    do i = 1, nall
        goukei = goukei + mark(i)
    enddo

    write(*, '(A,F5.1)') 'Average mark is ' &
        , nint(real(goukei) / nall * 10) / 10.0
        
end program exercise0101