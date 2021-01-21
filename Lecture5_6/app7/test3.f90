program test3
implicit none
integer sum(0:3), k
sum(0) = 0
do k = 1, 3
 sum(k) = sum(k-1) + k
enddo
write(*,'(A,I)') 'sum = ', sum(3)
end program
