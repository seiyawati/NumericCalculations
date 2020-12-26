program freefall
    implicit none

    real(8) :: z ! 垂直位置
    real(8) :: g ! 重力加速度
    real(8) :: t ! 時間
    real(8) :: v0 ! 初期速度
    real(8) :: v ! 速度
    integer :: i
    
    g = 9.8d0

    open(11, file='input.dat', status='old')
    ! read(11,*) t
    read(11,*) v0
    ! t = 1.0d0
    ! print *, 'Please input time [s]'
    ! read(*,*) t ! *.*はデフォルトの書式で標準入力から入力することを示す
    ! print *, 'Please input initial velocity [m/s]'
    ! read(*,*) v0 ! *.*はデフォルトの書式で標準入力から入力することを示す

    open(10, file='output.dat', status='replace') ! ファイルの指定, 5番と6番は使わない statusにはold, new. replace

    do i=1,10

        t =i
        z = (1.0d0/2.0d0)*g*t**2d0 + v0*t
        v = g*t + v0
        write(10,*) z,v

    enddo

    close(10)
    ! print *, z,v

end program freefall
