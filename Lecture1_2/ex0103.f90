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

      write(*,'(i10)') i         ! ��=10�A�E��
      write(*,'(i0)') i          ! ����
      write(*,'(1x,i0)') i       ! �ꕶ���󂯂č���
      write(*,'(i1)') i          ! ��������Ȃ��ꍇ�̓A�X�^���X�N���o�͂����
      write(*,'(" x = ", f5.2)') 99999.0  ! ��������Ȃ��ꍇ�̂�����̗�
      write(*,'(i10.7)') i    ! ��=10�A�E�񂹁A�Œ�o�͌���=7 (�����Ȃ��ꍇ��0���o�́j
      write(*,*)              ! ��s�̑}��
      write(*,'(f10.2)') a       ! ��=10�A�E�񂹁A�����_�ȉ�2��
      write(*,'(f0.2)') a        ! ���񂹁A�����_�ȉ�2��
      write(*,'(e10.3, f15.5)') a, d   ! �����̏����w����J���}�ŋ�؂��
      write(*,'(2(1X,e10.3))') a, d    ! ���������w����J��Ԃ���
      write(*,'(e10.3e1)') a           ! �w�����̌����܂Ŏw�肷���
      write(*,'(es10.4e1,1X,es10.3e1)') a, d/10 ! �Ȋw�I�\�L
      write(*,*)              ! ��s�̑}��
      write(*,'(e10.3, f10.4)') c      ! ���f���͂Q�̎����̏������w�肷��
      write(*,'(2f10.4)') c            ! ���f���𔽕���=2�Ŏw�肷��
      write(*,'("(real=",f0.2," imag=",f0.2,")")') c ! �����萔�\���������w��Ɋ܂߂��
      write(*,*)              ! ��s�̑}��
      write(*,'(a)') s      ! �����w�肵�Ȃ��ƁA�錾���̒����i�����ł�4�j���g�����
      write(*,'(a2)') s     ! ��������Ȃ��ꍇ�͐؂�̂Ă���

! 1����nmax�܂ł̔ԍ��������t�@�C�����쐬����ꍇ(nmax��2���ƕ������Ă���Ƃ�)
      do n = 1, nmax
       write(fn,'(A,I2.2,A)') 'rslt/', n, '.dat'
       open(10,file=fn,form='formatted',action='write')
       write(10,'(I2)') n
       close(10)
      enddo

! ���nmax�����炩���ߕ�����Ȃ��Ƃ��ɂ͎��̂悤�ɕϐ��ɉ����ď������w�肷����@������
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

