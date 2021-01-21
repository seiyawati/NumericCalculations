program test10
implicit none
integer k, n
integer, allocatable, dimension(:) :: a
open(10,file='data.txt',form='formatted',action='read')
n = 0
input : do
 read(10,*,end=99)
 n = n + 1
enddo input
99 rewind(10)
allocate(a(1:n))
do k = 1, n
 read(10,*) a(k)
enddo
close(10)
end program
