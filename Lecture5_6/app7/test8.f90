program test8
implicit none
integer x, y, xn, yn, n
x = 1
y = 1
do n = 1, 5
 xn = x + y
 yn = x * y
 write(*,'(A,I2,2(1X,I))') &
   'n= ', n, xn, yn
 x = xn
 y = yn
enddo ! n = 1, 5
end program
