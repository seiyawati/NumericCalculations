program test
integer :: i, j
integer, parameter :: Nx = 20, Ny = 20
double precision, parameter :: a = 0.2d0
double precision :: pi, xmin, xmax, ymin, ymax, x, y

pi = dacos(-1.0d0)

xmin = 0.0d0
xmax = 2.0d0 * pi
ymin = -1.0d0
ymax = 2.0d0 * pi + 1.0d0

open(10,file='test.dat',form='formatted')

do j = 0, Ny
 y = ymin + (ymax - ymin) / Ny * j
 do i = 0, Nx
  x = xmin + (xmax - xmin) / Nx * i
  write(10,'(4(1X,ES13.5E3))') x, y, a, a * dsin(y)
 enddo
enddo 

close(10)

end program

