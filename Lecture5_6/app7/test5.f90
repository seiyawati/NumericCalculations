program test5
implicit none
integer sum, k
sum = 0
do k = 1, 3
 sum = sum + k
enddo
write(*,'(A,I)') 'sum = ', sum
end program
