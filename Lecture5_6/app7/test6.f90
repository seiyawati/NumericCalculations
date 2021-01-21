program test6
implicit none
integer a(3), b(3), k, n
a(1) = 1 ; a(2) = 1 ; a(3) = 1 
do n = 4, 10
 b(1) = a(2)
 b(2) = a(3)
 b(3) = a(1) + a(2) + a(3)
 write(*,'(A,I2,A,I)') &
   'p(', n, ')= ', b(3)
 do k = 1, 3
  a(k) = b(k)
 enddo
enddo ! n = 4, 10
end program
