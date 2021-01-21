program test2
implicit none
integer sum(0:3)
sum(0) = 0
sum(1) = sum(0) + 1
sum(2) = sum(1) + 2
sum(3) = sum(2) + 3
write(*,'(A,I)') 'sum = ', sum(3)

end program
