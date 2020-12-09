program exercise0102
    implicit none

    integer :: gender, men, women
    ! realは実数型
    real :: sum1, sum2, height

    ! 1行の途中で複数の命令をする時、セミコロン ; で区切る
    sum1 = 0.0d0 ; sum2 = 0.0d0
    men = 0 ; women = 0

    ! height.datというファイルを開き、それに装置番号10番を割り当てる
    open(10,file='height.dat', form='formatted')

    input : do

    ! 初めの数字をgenderに代入し、二つ目をheightに代入
    read(10,*,end=999) gender, height

    if(gender == 1) then
        sum1 = sum1 + height ; men = men + 1
    elseif(gender == 2) then
        sum2 = sum2 + height ; women = women + 1
    else
        ! &は次の行とつなげることを意味する
        write(*,*) &
            'gender is not 1 nor 2 in', men + women + 1, '-th line'
    endif

    enddo input
999 close(10)

    write(*, '(A,2(1X,I3))') &
        'Numbers of men/women are', men, women

    write(*, '(A,2(ES9.3E1,A))') &
        'Average height is' &
        , sum1 / men, 'm for men and' &
        , sum2 / women, 'm for women.'

end program exercise0102