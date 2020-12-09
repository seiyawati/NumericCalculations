      program exercise0103
      implicit none
      integer :: i = 1234
      real :: a = 1.234
      double precision :: d = 9.87654321d0
      complex :: c = (1.5,2.345)
      character(len=4) :: s = "abcd"
      integer :: m, n
      integer :: nmax = 10
      character*30 :: fn, kata

      write(*,'(i10)') i         ! 幅=10、右寄せ
      write(*,'(i0)') i          ! 左寄せ
      write(*,'(1x,i0)') i       ! 一文字空けて左寄せ
      write(*,'(i1)') i          ! 幅が足りない場合はアスタリスクが出力される
      write(*,'(" x = ", f5.2)') 99999.0  ! 幅が足りない場合のもう一つの例
      write(*,'(i10.7)') i    ! 幅=10、右寄せ、最低出力桁数=7 (満たない場合は0を出力）
      write(*,*)              ! 空行の挿入
      write(*,'(f10.2)') a       ! 幅=10、右寄せ、小数点以下2桁
      write(*,'(f0.2)') a        ! 左寄せ、小数点以下2桁
      write(*,'(e10.3, f15.5)') a, d   ! 複数の書式指定をカンマで区切る例
      write(*,'(2(1X,e10.3))') a, d    ! 同じ書式指定を繰り返す例
      write(*,'(e10.3e1)') a           ! 指数部の桁数まで指定する例
      write(*,'(es10.4e1,1X,es10.3e1)') a, d/10 ! 科学的表記
      write(*,*)              ! 空行の挿入
      write(*,'(e10.3, f10.4)') c      ! 複素数は２つの実数の書式を指定する
      write(*,'(2f10.4)') c            ! 複素数を反復数=2で指定する
      write(*,'("(real=",f0.2," imag=",f0.2,")")') c ! 文字定数表現を書式指定に含める例
      write(*,*)              ! 空行の挿入
      write(*,'(a)') s      ! 幅を指定しないと、宣言時の長さ（ここでは4）が使われる例
      write(*,'(a2)') s     ! 幅が足りない場合は切り捨てられる

! 1からnmaxまでの番号がついたファイルを作成する場合(nmaxが2桁と分かっているとき)
      do n = 1, nmax
       write(fn,'(A,I2.2,A)') 'rslt/', n, '.dat'
       open(10,file=fn,form='formatted',action='write')
       write(10,'(I2)') n
       close(10)
      enddo

! 上でnmaxがあらかじめ分からないときには次のように変数に応じて書式を指定する方法がある
      m = int(log10(nmax*1.0)) + 1
      write(kata,'(A,I0,A,I0,A)') '(A,I', m, '.', m, ',A)'
      write(*,*) kata
      do n = 1, nmax
       write(fn,kata) 'rslt/', n, '.txt'
       open(10,file=fn,form='formatted',action='write')
       write(10,'(I2)') n
       close(10)
      enddo

      end program

