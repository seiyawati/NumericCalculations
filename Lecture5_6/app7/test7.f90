program test7
implicit none
integer a(3), tmp, k, n
a(1) = 1 ; a(2) = 1 ; a(3) = 1 
do n = 4, 10
 tmp = a(1) + a(2) + a(3)
 a(1) = a(2)
 a(2) = a(3)
 a(3) = tmp
 write(*,'(A,I2,A,I)') &
   'p(', n, ')= ', tmp
enddo ! n = 4, 10
end program
