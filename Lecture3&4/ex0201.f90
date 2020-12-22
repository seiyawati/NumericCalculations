program bisection
implicit none
double precision :: a, b, c, eps
integer :: n

write(*,*) 'input eps (>0)'
read(*,*) eps

n = 0
a = 0.0d0
b = 1.0d0
write(*,'(I3,2(1X,ES33.25E3))') n, a, b

nibunhou : do

n = n + 1

c = (a + b) / 2.0d0

if(c - dcos(c) < 0.0d0) then
a = c
elseif(c - dcos(c) > 0.0d0) then
b = c
endif

write(*,'(I3,2(1X,ES33.25E3))') n, a, b

if(b - a < eps) exit nibunhou

enddo nibunhou

end program
