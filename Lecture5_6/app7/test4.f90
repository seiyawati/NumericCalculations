program test4
implicit none
integer sum, tmp, k
sum = 0
do k = 1, 3
 tmp = sum + k
 sum = tmp
enddo
write(*,'(A,I)') 'sum = ', sum
end program

